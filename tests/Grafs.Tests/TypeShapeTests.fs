/// The MIT License (MIT)
/// Copyright (c) 2016-2017 Eirik Tsarpalis
/// Copyright (c) 2017 Bazinga Technologies Inc

module Grafs.Tests.TypeShapeTests

open System
open System.Collections.Generic
open System.Runtime.Serialization
open System.Reflection
open FSharp.Reflection
open Expecto
open Grafs.TypeShape
open FsCheck

type TypeWithDefaultCtor(x : int) =
    new () = new TypeWithDefaultCtor(42)
    member __.Value = x
    
type CSharpRecord() =
    static let mutable counter = 0
    let count = System.Threading.Interlocked.Increment &counter
    member val Foo = "" with get,set
    member val Bar = false with get,set
    member val Baz = 0 with get,set
    member val TimeSpan = TimeSpan.Zero with get,set

    member __.GetterOnly = count

    override x.Equals y =
        match y with
        | :? CSharpRecord as y -> 
            x.Foo = y.Foo && x.Bar = y.Bar && 
            x.Baz = y.Baz && x.TimeSpan = y.TimeSpan
        | _ -> false

    override x.GetHashCode() = hash(x.Foo,x.Bar,x.Baz,x.TimeSpan)
    
type SimplePoco(x : string, y : int) =
    static let staticField = 42
    member __.X = x
    member __.Y = y
    member __.DoSmth () = ()

type Union7 = 
    | C1U7 of int * _tag:string
    | C2U7
    | C3U7 of int
    | C4U7 of unit
    | C5U7 of int * bool * byte[]
    | C6U7 of string
    | C7U7
    
type P = Z | S of P

type Record7 = 
    { 
        A1 : int ; A2 : string ; A3 : bool ; A4 : byte ; A5 : byte[] ; A6 : decimal ; A7 : int16 
    }

[<Struct>]
type StructRecord = { A : int ; B : string }

[<Struct>]
type StructUnion = 
    | SU1 of a:int
    | SU3
    | SU4 of string
    | SU5 of byte[] * int64
    
let testPrim<'T>() = 
    let shape = shapeof<'T>
    let shapeType = shape.GetType()
    shapeType = typeof<TypeShape<'T>> |> Expect.isTrue <| sprintf "Shape of %O should be %O" (shape.GetType()) typeof<TypeShape<'T>>
    let accepter = { new ITypeShapeVisitor<bool> with member __.Visit<'a>() = typeof<'T> = typeof<'a> }
    shape.Accept accepter |> Expect.isTrue <| sprintf "Shape of %O should accept generic visitor" shapeType
    
let testSeq<'E, 'T when 'E :> seq<'T>> (accepter) =
    match shapeof<'E> with 
    | Shape.Enumerable s -> s.Accept (accepter typeof<'E> typeof<'T>)
    | _ -> false
    |> Expect.isTrue <| sprintf "Visitor enumerable pattern works with %O" typeof<'t>
    
let testCollection<'E, 'T when 'E :> ICollection<'T>> (accepter) =
    match shapeof<'E> with 
    | Shape.Collection s -> s.Accept (accepter typeof<'E> typeof<'T>)
    | _ -> false
    |> Expect.isTrue <| sprintf "Visitor collection pattern works with %O" typeof<'t>
    
[<NoEquality; NoComparison>]
type NoEqNoComp = NoEqNoComp

let tests = 
    testList "TypeShape" [
        test "Should fail on invalid type inputs" {
            Expect.throwsT<ArgumentNullException> (fun () -> TypeShape.Create null |> ignore) "Should not accept nulls"
            Expect.throwsT<UnsupportedShape> (fun () -> TypeShape.Create typedefof<int option> |> ignore) "Should not accept open generic types"
            Expect.throwsT<UnsupportedShape> (fun () -> TypeShape.Create (typedefof<int option>.GetGenericArguments().[0]) |> ignore) "Should not accept generic type parameters"
            Expect.throwsT<UnsupportedShape> (fun () -> TypeShape.Create (typeof<int>.MakeByRefType()) |> ignore) "Should not accept ByRef types"
            Expect.throwsT<UnsupportedShape> (fun () -> TypeShape.Create (typeof<int>.MakePointerType()) |> ignore) "Should not accept pointer types"
        }

        test "Should correctly resolve untyped shapes" {
            TypeShape.Create typeof<int> :? TypeShape<int> |> Expect.isTrue <| "Shape of int type should be resolved"
            TypeShape.Create typeof<string []> :? TypeShape<string []> |> Expect.isTrue <| "Shape of array type should be resolved"
            TypeShape.Create typeof<int * string> :? TypeShape<int * string> |> Expect.isTrue <| "Shape of tuple type should be resolved"
            TypeShape.Create typeof<BindingFlags> :? TypeShape<BindingFlags> |> Expect.isTrue <| "Shape of enum type should be resolved"
        }

        test "Shape primitive" {
            testPrim<bool>()
            testPrim<byte>()
            testPrim<sbyte>()
            testPrim<int16>()
            testPrim<int32>()
            testPrim<int64>()
            testPrim<uint16>()
            testPrim<uint32>()
            testPrim<uint64>()
        }

        test "Shape BCL primitives" {
            testPrim<DateTime>()
            testPrim<DateTimeOffset>()
        }

        test "Shape Type with default ctor" {            
            let accepter = 
                { new IDefaultConstructorVisitor<bool> with
                    member __.Visit<'T when 'T : (new : unit -> 'T)> () = 
                        let t = new 'T() :> obj :?> TypeWithDefaultCtor 
                        t.Value = 42 }

            match shapeof<TypeWithDefaultCtor> with 
            | Shape.DefaultConstructor s -> s.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Default constructor shape should be detected"
        }

        test "Shape Struct" {
            let accepter1 = 
                { new IStructVisitor<bool> with
                    member __.Visit<'T when 'T : struct> () = true }
            let accepter2 = 
                { new INotStructVisitor<bool> with
                    member __.Visit<'T when 'T : not struct and 'T : null> () = false }

            match shapeof<int> with 
            | Shape.Struct s -> s.Accept accepter1 
            | Shape.NotStruct s -> s.Accept accepter2
            |> Expect.isTrue <| "Struct should be recognized as value type"

            match shapeof<string> with 
            | Shape.Struct s -> s.Accept accepter1 
            | Shape.NotStruct s -> s.Accept accepter2
            |> Expect.isFalse <| "String should not be recognized as value type"
        }

        test "Shape Binding Flags" {            
            let accepter = 
                { new IEnumVisitor<bool> with 
                    member __.Visit<'T, 'U when 'T : enum<'U>>() = 
                        typeof<'T> = typeof<BindingFlags> && typeof<'U> = typeof<int> }

            match shapeof<BindingFlags> with
            | Shape.Enum e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Enum should be recognized as enum shape"
        }

        test "Shape Nullable" {            
            let accepter = 
                { new INullableVisitor<bool> with 
                    member __.Visit<'T when 'T : struct and 'T : (new : unit -> 'T) and 'T :> ValueType>() = 
                        typeof<'T> = typeof<int> }

            match shapeof<Nullable<int>> with 
            | Shape.Nullable e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Nullable should be recognized as nullable type"
        }

        test "Shape Equality" {            
            let testType expected (t:Type) =
                match TypeShape.Create t with
                | Shape.Equality s ->
                    if not expected 
                    then false 
                    else
                        s.Accept { new IEqualityVisitor<bool> with
                                    member __.Visit<'T when 'T : equality> () = typeof<'T> = t }
                    |> Expect.isTrue <| sprintf "Type of %O should pass shape equality" t
                | _ -> expected |> Expect.isFalse <| sprintf "Type of %O should not pass shape equality" t

            typeof<int> |> testType true
            typeof<string> |> testType true
            typeof<string * int option> |> testType true
            typeof<Type list> |> testType true
            typeof<string []> |> testType true
            typeof<obj> |> testType true

            typeof<NoEqNoComp> |> testType false
            typeof<NoEqNoComp option> |> testType false
            typeof<NoEqNoComp ref> |> testType false
            typeof<NoEqNoComp []> |> testType false
            typeof<NoEqNoComp list> |> testType false
            typeof<NoEqNoComp * int> |> testType false
            typeof<int -> int> |> testType false
        }

        test "Shape Comparison" {            
            let testType expected (t:Type) =
                match TypeShape.Create t with
                | Shape.Comparison s ->
                    if not expected 
                    then false 
                    else
                        s.Accept { new IComparisonVisitor<bool> with
                            member __.Visit<'T when 'T : comparison>() = typeof<'T> = t }
                    |> Expect.isTrue <| sprintf "Type of %O should pass shape comparison" t
                | _ -> expected |> Expect.isFalse <| sprintf "Type of %O should not pass shape comparison" t
                

            typeof<int> |> testType true 
            typeof<string> |> testType true 
            typeof<IntPtr> |> testType true 
            typeof<string * int option> |> testType true 
            typeof<string []> |> testType true 
            typeof<string list> |> testType true 
            typeof<string ref> |> testType true 

            typeof<obj> |> testType false 
            typeof<Type> |> testType false 
            typeof<Type ref list option []> |> testType false 
            typeof<NoEqNoComp> |> testType false 
            typeof<NoEqNoComp option> |> testType false 
            typeof<NoEqNoComp ref> |> testType false 
            typeof<NoEqNoComp []> |> testType false 
            typeof<NoEqNoComp list> |> testType false 
            typeof<NoEqNoComp * int> |> testType false 
            typeof<int -> int> |> testType false 
        }

        test "Shape Tuple`1" {
            let accepter = 
                { new ITuple1Visitor<bool> with 
                    member __.Visit<'T>() = typeof<'T> = typeof<int> }
            match shapeof<Tuple<int>> with 
            | Shape.Tuple1 e -> e.Accept accepter 
            | _ -> false 
            |> Expect.isTrue <| "Tuple`1 should be recognized as tuple shape"
        }

        test "Shape Tuple`2" {            
            let accepter = 
                { new ITuple2Visitor<bool> with 
                    member __.Visit<'T1, 'T2>() = typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> }
            match shapeof<int * string> with 
            | Shape.Tuple2 e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Tuple`2 should be recognized as tuple shape"
        }

        test "Shape Tuple`3" {            
            let accepter = 
                { new ITuple3Visitor<bool> with 
                    member __.Visit<'T1, 'T2, 'T3>() = 
                        typeof<'T1> = typeof<int> && 
                        typeof<'T2> = typeof<string> && 
                        typeof<'T3> = typeof<bool> }
            match shapeof<int * string * bool> with 
            | Shape.Tuple3 e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Tuple`3 should be recognized as tuple shape"
        }

        test "Shape Tuple`4" {            
            let accepter = 
                { new ITuple4Visitor<bool> with 
                    member __.Visit<'T1, 'T2, 'T3, 'T4>() = 
                        typeof<'T1> = typeof<int> && 
                        typeof<'T2> = typeof<string> && 
                        typeof<'T3> = typeof<bool> &&
                        typeof<'T4> = typeof<byte> }
            match shapeof<int * string * bool * byte> with 
            | Shape.Tuple4 e -> e.Accept accepter 
            | _ -> false            
            |> Expect.isTrue <| "Tuple`4 should be recognized as tuple shape"
        }

        test "Shape Tuple`5" {            
            let accepter = 
                { new ITuple5Visitor<bool> with 
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5>() = 
                        typeof<'T1> = typeof<int> && 
                        typeof<'T2> = typeof<string> && 
                        typeof<'T3> = typeof<bool> &&
                        typeof<'T4> = typeof<byte> &&
                        typeof<'T5> = typeof<sbyte> }
            match shapeof<int * string * bool * byte * sbyte> with 
            | Shape.Tuple5 e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Tuple`5 should be recognized as tuple shape"
        }

        test "Shape Tuple`6" {            
            let accepter = 
                { new ITuple6Visitor<bool> with 
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>() = 
                        typeof<'T1> = typeof<int> && 
                        typeof<'T2> = typeof<string> && 
                        typeof<'T3> = typeof<bool> &&
                        typeof<'T4> = typeof<byte> &&
                        typeof<'T5> = typeof<sbyte> &&
                        typeof<'T6> = typeof<int16> }
            match shapeof<int * string * bool * byte * sbyte * int16> with 
            | Shape.Tuple6 e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Tuple`6 should be recognized as tuple shape"
        }

        test "Shape Tuple`7" {            
            let accepter = 
                { new ITuple7Visitor<bool> with 
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>() = 
                        typeof<'T1> = typeof<int> && 
                        typeof<'T2> = typeof<string> && 
                        typeof<'T3> = typeof<bool> &&
                        typeof<'T4> = typeof<byte> &&
                        typeof<'T5> = typeof<sbyte> &&
                        typeof<'T6> = typeof<int16> && 
                        typeof<'T7> = typeof<int64>}
            match shapeof<int * string * bool * byte * sbyte * int16 * int64> with 
            | Shape.Tuple7 e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Tuple`7 should be recognized as tuple shape"
        }

        test "Shape Tuple`8" {
            let accepter = 
                { new ITuple8Visitor<bool> with 
                    member __.Visit<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'TRest>() = 
                        typeof<'T1> = typeof<int> && 
                        typeof<'T2> = typeof<string> && 
                        typeof<'T3> = typeof<bool> &&
                        typeof<'T4> = typeof<byte> &&
                        typeof<'T5> = typeof<sbyte> &&
                        typeof<'T6> = typeof<int16> && 
                        typeof<'T7> = typeof<int64> &&
                        typeof<'TRest> = typeof<Tuple<int>> }
            match shapeof<int * string * bool * byte * sbyte * int16 * int64 * int> with 
            | Shape.Tuple8 e -> e.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Tuple`8 should be recognized as tuple shape"

        }

        test "Shape generic tuple" {            
            let checkShape tupleType =
                match TypeShape.Create tupleType with
                | Shape.Tuple shape ->
                    shape.Accept { new ITupleVisitor<bool> with
                        member __.Visit (tuple : ShapeTuple<'Tuple>) =
                            typeof<'Tuple> = tupleType &&
                            tuple.Elements.Length = FSharpType.GetTupleElements(tupleType).Length }
                    |> Expect.isTrue <| sprintf "Shape %O should be recognized as tuple" tupleType
                | _ -> failwithf "Shape %O not recognized as a tuple" tupleType

            checkShape typeof<Tuple<string>>
            checkShape typeof<int * string>  
            checkShape typeof<int * decimal * byte[] * bigint>
            checkShape typeof<int * int * int * int * int * int * int * int * int * int * int * int>
        }

        test "Shape CliMutable" {
            match TypeShape.Create<CSharpRecord>() with
            | Shape.CliMutable r -> 
                r.Accept { new ICliMutableVisitor<bool> with
                    member __.Visit (shape : ShapeCliMutable<'R>) =
                        Expect.equal typeof<'R> typeof<CSharpRecord> <| sprintf "Shape %O should be recognized as C# record shape" typeof<'R>
                        Expect.equal shape.Properties.Length 4 "CLI object should have 4 properties"
                        true }
            | _ -> failwithf "Type %O not recognized as C# record" typeof<CSharpRecord>
            |> ignore

            let source = new CSharpRecord(Foo = "Foo", Bar = true, Baz = 42, TimeSpan = TimeSpan.MaxValue)

            let cloner = mkCloner<CSharpRecord>()
            let target = cloner source
            Expect.isFalse (obj.ReferenceEquals(source, target)) "A deep cloning expected"
            Expect.equal source target "Cloned object should equal source using custom equality"
            Expect.notEqual source.GetterOnly target.GetterOnly "Cloned object custom fields should differ"

            let sCloner = mkCloner<CSharpRecord> ()
            let target = sCloner source
            Expect.isFalse (obj.ReferenceEquals(source, target)) "A deep cloning expected twice"
            Expect.equal source target "Cloned object should equal source using custom equality twice"
            Expect.notEqual source.GetterOnly target.GetterOnly "Twice cloned object custom fields should differ"
        }

        test "Shape Poco" {
            match TypeShape.Create<SimplePoco>() with
            | Shape.Poco s ->
                s.Accept { new IPocoVisitor<bool> with
                    member __.Visit (shape : ShapePoco<'P>) =
                        Expect.equal typeof<'P> typeof<SimplePoco> "Expected a SimplePoco"
                        Expect.equal 2 shape.Fields.Length "SimplePoco has 2 public fields"
                        Expect.equal 2 shape.Properties.Length "SimplePoco has 2 public properties"
                        Expect.equal 1 shape.Constructors.Length "SimplePoco has 1 public constructor"
                        Expect.equal 1 shape.Methods.Length "SimplePoco has 1 public method"
                        true }
                |> ignore

            | _ -> failwithf "Type %O not recognized as POCO" typeof<SimplePoco>

            let source = new SimplePoco("foo", 42)

            let cloner = mkCloner<SimplePoco>()
            let target = cloner source
            Expect.isFalse (obj.ReferenceEquals(source, target)) "SimplePoco should be deep cloned"
            Expect.equal source.X target.X "SimplePoco.X values should be copied"
            Expect.equal source.Y target.Y "SimplePoco.Y values should be copied"
        }

        test "Shape FSharpFunc" {
            let acceptor =
                { new IFSharpFuncVisitor<bool> with
                    member __.Visit<'D,'C>() = typeof<'D> = typeof<int> && typeof<'C> = typeof<string>}
            match shapeof<int -> string> with 
            | Shape.FSharpFunc s -> s.Accept acceptor 
            | _ -> false
            |> Expect.isTrue <| "Visitor pattern should works with F# Func"
        }

        test "Shape Exception" {
            let accepter =
                { new IExceptionVisitor<bool> with
                    member __.Visit<'exn when 'exn :> exn>() = typeof<'exn> = typeof<System.IO.FileNotFoundException> }
            match shapeof<System.IO.FileNotFoundException> with 
            | Shape.Exception s -> s.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Visitor pattern should works with .NET exceptions"
        }

        test "Shape delegate" {
            let accepter =
                { new IDelegateVisitor<bool> with
                    member __.Visit<'Delegate when 'Delegate :> Delegate>() = typeof<'Delegate> = typeof<Predicate<string>> }
            match shapeof<Predicate<string>> with 
            | Shape.Delegate s -> s.Accept accepter 
            | _ -> false
            |> Expect.isTrue <| "Visitor pattern should works with .NET delegates"
        }

        test "Shape sequence" {
            let accepter e t =
                { new IEnumerableVisitor<bool> with
                    member __.Visit<'E, 'T when 'E :> seq<'T>>() = typeof<'T> = t && typeof<'E> = e }
            testSeq<int [], int> accepter
            testSeq<int list, int> accepter
            testSeq<seq<int>, int> accepter
            testSeq<ResizeArray<int>, int> accepter
            testSeq<HashSet<int>, int> accepter
            testSeq<Dictionary<int, string>, KeyValuePair<int,string>> accepter
            testSeq<Set<int>, int> accepter
            testSeq<Map<int, string>, KeyValuePair<int,string>> accepter
            testSeq<IDictionary<int, string>, KeyValuePair<int,string>> accepter
            testSeq<Stack<int>, int> accepter
        }

        test "Shape Collection" {
            let accepter c t =
                { new ICollectionVisitor<bool> with
                    member __.Visit<'C, 'T when 'C :> ICollection<'T>>() = typeof<'T> = t && typeof<'C> = c}
            match shapeof<seq<int>> with 
            | Shape.Collection s -> s.Accept (accepter typeof<seq<int>> typeof<'T>)
            | _ -> true
            |> Expect.isFalse <| "Visitor collection pattern should not work with seq"
            testCollection<int [], int> accepter
            testCollection<ResizeArray<int>, int> accepter
            testCollection<HashSet<int>, int> accepter
            testCollection<Dictionary<int, string>, KeyValuePair<int,string>> accepter
            testCollection<Set<int>, int> accepter
            testCollection<Map<int, string>, KeyValuePair<int,string>> accepter
            testCollection<IDictionary<int, string>, KeyValuePair<int,string>> accepter
        }

        test "Shape Array" {
            let accepter rk = 
                { new IArrayVisitor<bool> with
                    member __.Visit<'T> rank = typeof<'T> = typeof<int> && rank = rk }

            match shapeof<int []> with Shape.Array s -> s.Accept (accepter 1) | _ -> false 
            |> Expect.isTrue <| "Array visitor works with rank 1 arrays"    
            match shapeof<int [,]> with Shape.Array s -> s.Accept (accepter 2) | _ -> false 
            |> Expect.isTrue <| "Array visitor works with rank 2 arrays"
            match shapeof<int [,,]> with Shape.Array s -> s.Accept (accepter 3) | _ -> false 
            |> Expect.isTrue <| "Array visitor works with rank 3 arrays"
            match shapeof<int [,,,]> with Shape.Array s -> s.Accept (accepter 4) | _ -> false 
            |> Expect.isTrue <| "Array visitor works with rank 4 arrays"
        }

        test "Shape ResizeArray" {
            let accepter = 
                { new IResizeArrayVisitor<bool> with
                    member __.Visit<'T>() = typeof<'T> = typeof<int> }

            match shapeof<ResizeArray<int>> with Shape.ResizeArray s -> s.Accept accepter | _ -> false
            |> Expect.isTrue <| "ResizeAray visitor works with resizable array lists"
        }

        test "Shape Dictionary" {
            let accepter = 
                { new IDictionaryVisitor<bool> with
                    member __.Visit<'K, 'V when 'K : equality>() = typeof<'K> = typeof<int> && typeof<'V> = typeof<string> }

            match shapeof<Dictionary<int, string>> with Shape.Dictionary s -> s.Accept accepter | _ -> false
            |> Expect.isTrue <| "Dictionary visitor works with dictionaries"
        }

        test "Shape F# Set" {
            let accepter = 
                { new IFSharpSetVisitor<bool> with
                    member __.Visit<'T when 'T : comparison>() = typeof<'T> = typeof<string> }

            match shapeof<Set<string>> with Shape.FSharpSet s -> s.Accept accepter | _ -> false
            |> Expect.isTrue <| "F# Set visitor works with F# sets"
        }

        test "Shape ISerializable" {
            let accepter =
                { new ISerializableVisitor<bool> with
                    member __.Visit<'T when 'T :> ISerializable> (_ : ShapeISerializable<'T>) = typeof<'T> = typeof<exn> }

            match shapeof<exn> with Shape.ISerializable s -> s.Accept accepter | _ -> false
            |> Expect.isTrue <| "Serializable visitor works with exceptions"

            let exn = new Exception("kaboom!")
    
            let cloner = mkCloner<Exception>()
            let exn' = cloner exn
            (not (refEq exn exn') && exn.Message = exn'.Message) |> Expect.isTrue <| "Serializable visitor allows to deep clone exceptions"
        }

        test "Shape F# Option" {
            let visitor ty =
                { new IFSharpOptionVisitor<bool> with member __.Visit<'T>() = typeof<'T> = ty }

            match shapeof<int option> with Shape.FSharpOption s -> s.Accept (visitor typeof<int>) | _ -> false
            |> Expect.isTrue <| "F# option visitor works with F# option type"
        }

        test "Shape F# List" {
            let visitor ty =
                { new IFSharpListVisitor<bool> with member __.Visit<'T>() = typeof<'T> = ty }

            match shapeof<int list> with Shape.FSharpList s -> s.Accept (visitor typeof<int>) | _ -> false
            |> Expect.isTrue <| "F# list visitor works with F# list"
        }

        test "Shape F# Map" {
            let accepter = 
                { new IFSharpMapVisitor<bool> with
                    member __.Visit<'K, 'V when 'K : comparison>() = typeof<'K> = typeof<string> && typeof<'V> = typeof<int> }

            match shapeof<Map<string, int>> with Shape.FSharpMap s -> s.Accept accepter | _ -> false
            |> Expect.isTrue <| "F# map visitor works with F# maps"
        }

        test "Shape Record" {
           let shape = 
               match shapeof<Record7> with
               | Shape.FSharpRecord (:? ShapeFSharpRecord<Record7> as r) -> r
               | _ -> raise <| new InvalidCastException()

           Expect.equal 7 shape.Fields.Length "Visitor should detect all fields"
           let cloner = mkCloner<Record7>()
           checkCloner cloner
        }

        test "Shape F# ref" {
            match shapeof<int ref> with Shape.FSharpRecord r -> r.Fields.Length = 1 | _ -> false
            |> Expect.isTrue <| "F# record should work on F# ref type"
            let accepter = { new IFSharpRefVisitor<bool> with member __.Visit<'T>() = typeof<'T> = typeof<int> }
            match shapeof<int ref> with Shape.FSharpRef s -> s.Accept accepter | _ -> false
            |> Expect.isTrue <| "F# Ref visitor should work on F# ref type"
        }

        test "Shape F# Choice`2" {
            let accepter = { new IFSharpChoice2Visitor<bool> with member __.Visit<'T1,'T2>() = typeof<'T1> = typeof<int> && typeof<'T2> = typeof<string> }
            match shapeof<Choice<int,string>> with Shape.FSharpChoice2 c -> c.Accept accepter | _ -> false
            |> Expect.isTrue <| "F# Choice visiitor works with Choice types"
        }
        

        test "Shape F# discriminated union" {
            let shape = 
                match shapeof<Union7> with 
                | Shape.FSharpUnion s -> 
                    s.Accept { new IFSharpUnionVisitor<bool> with
                        member __.Visit (shape : ShapeFSharpUnion<'U>) =
                            Expect.equal typeof<Union7> typeof<'U> "F# union visitor correctly recognized union type"
                            Expect.equal 7 shape.UnionCases.Length "F# union visitor correctly recognized union arity"
                            true
                    }

                | _ -> raise <| InvalidCastException()

            let cloner = mkCloner<Union7>()
            checkCloner cloner
        }

        test "Shape F# option as discriminated union" {
            match shapeof<int option> with Shape.FSharpUnion u -> u.UnionCases.Length = 2 | _ -> false
            |> Expect.isTrue <| "F# union visitor works with F# option type"
        }

        test "Shape F# list as discriminated union" {
            match shapeof<int list> with Shape.FSharpUnion u -> u.UnionCases.Length = 2 | _ -> false
            |> Expect.isTrue <| "F# union visitor works with F# list type"
        }

        test "Shape F# choice as discriminated union" {
            match shapeof<Choice<int,string,bool>> with Shape.FSharpUnion u -> u.UnionCases.Length = 3 | _ -> false
            |> Expect.isTrue <| "F# union visitor works with F# Choice type"
        }

        test "Shape clone recursive types" {
            let cloner = mkCloner<P>()
            checkCloner cloner
        }

        test "BinSearch should report correct indices" {
            let property (inputs : string []) =
                let inputs = Array.distinct inputs
                let binSearch = BinSearch inputs
                inputs 
                |> Seq.mapi (fun i v -> i,v)
                |> Seq.forall (fun (i,v) -> binSearch.TryFindIndex v = i)

            Check.QuickThrowOnFailure property
        }

        test "BinSearch should return -1 on non-existingValues" {
            let property (inputs : Set<string>) (otherValues : Set<string>) =
                let binSearch = BinSearch (Set.toArray inputs)
                let missingValues = otherValues - inputs
                missingValues
                |> Seq.forall (fun v -> binSearch.TryFindIndex v = -1)

            Check.QuickThrowOnFailure property
        }

        test "Should support struct records" {
            match shapeof<StructRecord> with
            | Shape.FSharpRecord (:? ShapeFSharpRecord<StructRecord> as s) ->
                (s.IsStructRecord && s.Fields.Length = 2) |> Expect.isTrue <| "F# record visitor works with F# struct records"
            | _ -> raise <| InvalidCastException()

            let cloner = mkCloner<StructRecord>()
            checkCloner cloner
        }

        test "Should support struct unions" {
            match shapeof<StructUnion> with
            | Shape.FSharpUnion (:? ShapeFSharpUnion<StructUnion> as s) ->
                (s.IsStructUnion && s.UnionCases.Length = 4 (* 6 *)) |> Expect.isTrue <| "F# union visitor works with F# struct unions"
                let fieldTypes = s.UnionCases |> Array.map (fun c -> c.Fields |> Array.map (fun f -> f.Member.Type))
                Expect.equal fieldTypes [|
                                [|typeof<int>|]
                                //[|typeof<int>|]
                                [||]
                                [|typeof<string>|];
                                [|typeof<byte[]>;typeof<int64>|]
                                //[||]
                            |]  "F# union visitor recognizes F# struct union fields"
            | _ -> raise <| InvalidCastException()

            let cloner = mkCloner<StructUnion>()
            checkCloner cloner
        }

        test "Should support struct tuples" {
            let testStructTuple (stuple : 'STuple) =
                let elems = FSharp.Reflection.FSharpType.GetTupleElements typeof<'STuple>
                match shapeof<'STuple> with
                | Shape.Tuple (:? ShapeTuple<'STuple> as s) ->
                    Expect.isTrue s.IsStructTuple "F# tuple visitor works with struct tuples"
                    Expect.equal elems (s.Elements |> Array.map (fun e -> e.Member.Type)) "F# tuple visitor picks tuple fields"
                | _ -> raise <| InvalidCastException()

                let cloner = mkCloner<'STuple>()
                Expect.equal stuple (cloner stuple) "F# tuple visitor deep clones structural tuples"
                
            testStructTuple (struct(1,"2"))
            testStructTuple (struct(1,"3",2,"4",5,"5",6,"7",8,"9",10))
            testStructTuple (struct(1,"3",2,"4",5,"5",6,"7",8,"9",10,"11",12,"13",14,"15",16,"17"))
        }
    ]
