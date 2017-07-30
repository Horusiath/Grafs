/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace Grafs

open System
open System.Reflection
open FSharp.Reflection

[<AbstractClass>]
type GQLAttribute() = inherit Attribute()

[<AttributeUsage(AttributeTargets.All)>]
type GQLIgnoreAttribute() = inherit GQLAttribute()

[<AttributeUsage(AttributeTargets.Method|||AttributeTargets.Property)>]
type QueryAttribute() = inherit GQLAttribute()

[<AttributeUsage(AttributeTargets.Method|||AttributeTargets.Property)>]
type MutationAttribute() = inherit GQLAttribute()

[<Obsolete("Subscriptions are not yet supported in this setup")>]
[<AttributeUsage(AttributeTargets.Method|||AttributeTargets.Property)>]
type SubscriptionAttribute() = inherit GQLAttribute()

[<AttributeUsage(AttributeTargets.All)>]
type DescriptionAttribute(text: string) =
    inherit GQLAttribute()
    member __.Text = text
    
    
module internal Reflection =

    /// Returns array of all public instance properties declared in target type.
    /// This is .NET Core tolerant impl - a TypeInfo will be used if needed.
    let private getProperties t =
        if FSharpType.IsRecord t
        then FSharpType.GetRecordFields t
        else
            #if NETSTANDARD1_6
            t.GetTypeInfo().GetProperties()
            #else
            t.GetProperties()
            #endif
            
    /// Returns array of all public instance methods declared in target type.
    /// This is .NET Core tolerant impl - a TypeInfo will be used if needed.
    let inline private getMethods (t: Type) =
        #if NETSTANDARD1_6
        t.GetTypeInfo().GetMethods()
        #else
        t.GetMethods()
        #endif

    let private listDef = typedefof<list<_>>
    let private optionDef = typedefof<option<_>>

    /// Returns a pair of constructors (Cons, Nil) for a F# list of provided type.
    let listBuilder t = 
        let tList = listDef.MakeGenericType([| t |]).GetTypeInfo()
        let nil = tList.GetDeclaredProperty("Empty").GetValue (null)
        let cons = 
            let cons' = tList.GetDeclaredMethod("Cons")
            fun item list -> cons'.Invoke (null, [| item; list |])
        (cons, nil)

    /// Returns a pair of constructors (Some, None) for a F# option of provided type.
    let optionBuilder t =
        let tOption = optionDef.MakeGenericType([|t|]).GetTypeInfo()
        let none = tOption.GetDeclaredProperty("None").GetValue(null)
        let some =
            let createSome = tOption.GetDeclaredMethod "Some"
            fun value -> 
                if value <> null 
                then
                    let valueType = value.GetType().GetTypeInfo()
                    if valueType = tOption
                    then value
                    elif t .GetTypeInfo().IsAssignableFrom(valueType)
                    then createSome.Invoke(null, [| value |])
                    else null
                else none
        (some, none)
            
    [<Flags>]
    type MemberMetadata =
        { IsQuery: bool
          IsMutation: bool
          IsSubscription: bool
          DeprecationReason: string option
          Description: string option }
        static member Zero =
            { IsQuery = false;
              IsMutation = false;
              IsSubscription = false;
              DeprecationReason = None;
              Description = None }

    type Param = { Name: string; Type: Type; DefaultValue: obj option }

    [<CustomEquality; NoComparison>]
    type Member =
        | Property of name:string * returnType:Type * meta:MemberMetadata * getter:MethodInfo * setter:MethodInfo option
        | Method   of name:string * returnType:Type * meta:MemberMetadata * params:Param[] * caller:MethodInfo
        member x.Type =
            match x with
            | Property(_,t,_,_,_) -> t
            | Method(_,t,_,_,_) -> t
        interface IEquatable<Member> with
            member x.Equals(other: Member) =
                match x, other with
                | Property(n1, r1, m1, _, _), Property(n2, r2, m2, _, _) -> n1 = n2 && r1 = r2 && m1 = m2
                | Method(n1, r1, m1, p1, _), Method(n2, r2, m2, p2, _) -> n1 = n2 && r1 = r2 && m1 = m2 && p1 = p2
                | _, _ -> false

    [<Flags>]
    type TypeKind =
        | Unknown       = 0
        | Scalar        = 1
        | Object        = 2
        | InputObject   = 4
        | Interface     = 8
        | Union         = 16
        | Enum          = 32
        | List          = 64
        | Option        = 128

    type TypeMetadata =
        { DeprecationReason: string option
          Description: string option
          GenericTypeName: string option
          Kind: TypeKind
          DependentTypes: Type[] }
        static member Zero =
            { DeprecationReason = None;
              Description = None;
              GenericTypeName = None;
              Kind = TypeKind.Unknown;
              DependentTypes = [||] }
        static member Scalar =
            { DeprecationReason = None;
              Description = None;
              GenericTypeName = None;
              Kind = TypeKind.Scalar;
              DependentTypes = [||] }

    type TypeConstructionInfo =
        { Name: string
          ClrType: Type
          Metadata: TypeMetadata
          Members: Member[] }

    let commonTypes =
        let kInt = { Name = "Int"; ClrType = typeof<int>; Metadata = TypeMetadata.Scalar; Members = [||] }
        let kFloat = { Name = "Float"; ClrType = typeof<float>; Metadata = TypeMetadata.Scalar; Members = [||] }
        let kString = { Name = "String"; ClrType = typeof<string>; Metadata = TypeMetadata.Scalar; Members = [||] }
        let kBool = { Name = "Boolean"; ClrType = typeof<bool>; Metadata = TypeMetadata.Scalar; Members = [||] }
        let kList = { Name = "List"; ClrType = typedefof<seq<_>>; Metadata = TypeMetadata.Scalar; Members = [||] }
        [ kInt; kFloat; kString; kBool; kList ]
        |> Seq.map (fun x -> (x.Name, x))
        |> dict

    type ConstructionContext =
        { KnownTypes: System.Collections.Generic.IDictionary<string, TypeConstructionInfo>
          ScannedAssemblies: Assembly[]
          ScannedTypes: System.Collections.Generic.HashSet<Type>
          Errors: System.Collections.Generic.List<string> }
        static member Default() =
            let assemblies =
                Assembly.GetExecutingAssembly().GetReferencedAssemblies()
                |> Array.map (string >> Assembly.Load)
            { KnownTypes = commonTypes;
              ScannedAssemblies = assemblies;
              ScannedTypes = commonTypes |> Seq.map (fun (KeyValue(_, v)) -> v.ClrType) |> System.Collections.Generic.HashSet<Type>
              Errors = System.Collections.Generic.List<string>() }
        member x.AddError err = x.Errors.Add err
        member x.MarkAsScanned t = x.ScannedTypes.Add t |> ignore
        
    /// Finds a first constructor for target type that has matching fields.
    let matchConstructor (t: Type) (fields: string []) =
        if FSharpType.IsRecord(t, true) then FSharpValue.PreComputeRecordConstructorInfo(t, true)
        else
            let constructors = t.GetConstructors(BindingFlags.NonPublic|||BindingFlags.Public|||BindingFlags.Instance)
            let fieldNames = 
                fields
                |> Set.ofArray
            let (ctor, _) =
                constructors
                |> Array.map (fun ctor -> (ctor, ctor.GetParameters() |> Array.map (fun param -> param.Name)))
                // start from most complete constructors
                |> Array.sortBy (fun (_, paramNames) -> -paramNames.Length)                  
                // try match field with params by name
                // at last, default constructor should be used if defined
                |> Array.find (fun (_, paramNames) -> Set.isSubset (Set.ofArray paramNames) fieldNames)    
            ctor

    let private shouldInclude (m: #MemberInfo) =
        if m.GetCustomAttributes(typeof<GQLIgnoreAttribute>) |> Seq.isEmpty
        then
            match box m with
            | :? PropertyInfo as info -> info.PropertyType.GetCustomAttributes(typeof<GQLIgnoreAttribute>) |> Seq.isEmpty
            | :? MethodInfo as info -> info.ReturnType.GetCustomAttributes(typeof<GQLIgnoreAttribute>) |> Seq.isEmpty
        else false

    let private extractFlags attrs =
        let attr2Flag acc (a: Attribute) =
            match a with
            | :? QueryAttribute                -> { acc with IsQuery = true }
            | :? MutationAttribute             -> { acc with IsMutation = true }
            | :? SubscriptionAttribute         -> { acc with IsSubscription = true }
            | :? ObsoleteAttribute as obsolete -> { acc with DeprecationReason = Some obsolete.Message }
            | :? DescriptionAttribute as desc  -> { acc with Description = Some desc.Text }
            | _ -> acc
        attrs |> Seq.fold attr2Flag MemberMetadata.Zero

    let private camelCase (str: string) =
        if Char.IsLower str.[0]
        then str
        else Char.ToLower(str.[0]).ToString() + str.Substring(1)

    let private parameter2Param (p: ParameterInfo) =
        { Name = p.Name |> camelCase
          Type = p.ParameterType
          DefaultValue = p.DefaultValue |> Option.ofObj }

    let private prop2Member (p:PropertyInfo) =
        let name = p.Name |> camelCase
        let flags = p.GetCustomAttributes() |> extractFlags
        Property(name, p.PropertyType, flags, p.GetMethod, p.SetMethod |> Option.ofObj)

    let private method2Member (m:MethodInfo) =
        let name = m.Name |> camelCase
        let flags = m.GetCustomAttributes() |> extractFlags
        let args =
            m.GetParameters()
            |> Array.map parameter2Param
        Method(name, m.ReturnType, flags, args, m)

    let inline private getAttributes (t: Type) = t.GetCustomAttributes()

    let private getGenericArgs (t: Type) =
        if t.IsGenericType
        then t.GetGenericArguments()
        else [||]

    let private getMembers (t: Type) =
        let properties =
            getProperties t
            |> Array.filter shouldInclude
            |> Array.map prop2Member
        let methods =
            getMethods t
            |> Array.filter shouldInclude
            |> Array.map method2Member
        Array.append methods properties

    let rec private typeName (ctx: ConstructionContext) genericsAllowed (t: Type) =
        let tparams = getGenericArgs t
        match tparams with
        | [||]      -> Some t.Name
        | [| targ |] when genericsAllowed ->
            let def = t.GetGenericTypeDefinition()
            if typedefof<seq<_>>.IsAssignableFrom(def) then Some "List"
            elif typedefof<option<_>>.IsAssignableFrom(def) then Some "Option"
            else
                match typeName ctx false targ with
                | None -> None
                | Some n -> Some (n + def.Name)
        | [| _ |] ->
            ctx.AddError <| sprintf "%s: GraphQL schema constructor does not allow nested generic types" t.FullName
            None
        | _ ->
            ctx.AddError <| sprintf "%s: GraphQL schema constructor does not support multi-parameter generic types" t.FullName
            None

    let private getDependentTypes ctx (abstractType: Type) =
        if FSharpType.IsUnion abstractType
        then
            FSharpType.GetUnionCases abstractType
            |> Array.map (fun ci -> ci.DeclaringType)
            |> Array.distinct
            |> Array.filter shouldInclude
        elif abstractType.IsInterface
        then
            ctx.ScannedAssemblies
            |> Array.collect(fun asm -> asm.GetExportedTypes())
            |> Array.filter (abstractType.IsAssignableFrom)
            |> Array.filter shouldInclude
        else [||]

    let private getMeta (ctx: ConstructionContext) (t: Type) =
        let genericType =
            let targs = getGenericArgs t
            match targs with
            | [||] -> None
            | [| targ |] -> typeName ctx false targ
            | _ ->
                ctx.AddError <| sprintf "%s: GraphQL schema constructor does not support multi-parameter generic types" t.FullName
                None
        t
        |> getAttributes
        |> Seq.fold (fun acc attr ->
            match attr with
            | :? ObsoleteAttribute as obsolete -> { acc with DeprecationReason = Some obsolete.Message }
            | :? DescriptionAttribute as descr -> { acc with Description = Some descr.Text }
            | _ -> acc) { TypeMetadata.Zero with GenericTypeName = genericType; }

    let rec constructInfo (ctx: ConstructionContext) (t: Type) =
        if ctx.ScannedTypes.Contains t then ()
        else
            ctx.MarkAsScanned t
            match typeName ctx true t with
            | None -> ()
            | Some name ->
                if t.GetCustomAttributes(typeof<GQLIgnoreAttribute>) |> Seq.isEmpty
                then
                    let meta = getMeta ctx t
                    let info =
                        { Name = name
                          ClrType = t
                          Metadata = meta
                          Members = getMembers t }
                    ctx.KnownTypes.[name] <- info
                    meta.DependentTypes |> Seq.iter (constructInfo ctx)
                    info.Members
                    |> Seq.map (fun x -> x.Type)
                    |> Seq.iter (constructInfo ctx)