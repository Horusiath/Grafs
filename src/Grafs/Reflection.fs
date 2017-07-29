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

[<AttributeUsage(AttributeTargets.Method|||AttributeTargets.Property)>]
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