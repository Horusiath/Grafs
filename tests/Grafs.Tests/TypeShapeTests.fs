/// The MIT License (MIT)
/// Copyright (c) 2016-2017 Eirik Tsarpalis
/// Copyright (c) 2017 Bazinga Technologies Inc

module Grafs.Tests.TypeShapeTests

open System
open System.Reflection
open FSharp.Reflection
open Expecto
open Grafs.TypeShape

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

type Union7 = 
    | C1U7 of int * _tag:string
    | C2U7
    | C3U7 of int
    | C4U7 of unit
    | C5U7 of int * bool * byte[]
    | C6U7 of string
    | C7U7
    
type P = Z | S of P

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
            test <@ obj.ReferenceEquals(source, target) |> not @>
            test <@ source = target @>
            test <@ source.GetterOnly <> target.GetterOnly @>

            let sCloner = mkCloner<CSharpRecord> ()
            let target = sCloner source
            test <@ obj.ReferenceEquals(source, target) |> not @>
            test <@ source = target @>
            test <@ source.GetterOnly <> target.GetterOnly @>
        }

        test "Shape Poco" {

        }

        test "Shape FSharpFunc" {

        }

        test "Shape Exception" {

        }

        test "Shape sequence" {

        }

        test "Shape Collection" {

        }

        test "Shape Array" {

        }

        test "Shape ResizeArray" {

        }

        test "Shape Dictionary" {

        }

        test "Shape FSharpSet" {

        }

        test "Shape ISerializable" {

        }

        test "Shape FSharpOption" {

        }

        test "Shape FSharpList" {

        }

        test "Shape FSharpMap" {

        }

        test "Shape Record" {

        }

        test "Shape F# ref" {

        }

        test "Shape FSharpChoice`2" {

        }

        test "Shape FSharpChoice`3" {

        }

        test "Shape FSharpChoice`4" {

        }

        test "Shape FSharpChoice`5" {

        }

        test "Shape FSharpChoice`6" {

        }

        test "Shape FSharpChoice`7" {

        }

        test "Shape F# discriminated union" {

        }

        test "Shape F# option as discriminated union" {

        }

        test "Shape F# list as discriminated union" {

        }

        test "Shape F# choice as discriminated union" {

        }

        test "Shape clone recursive types" {

        }

        test "BinSearch should report correct indices" {

        }

        test "BinSearch should return -1 on non-existingValues" {

        }

        test "Should support struct records" {

        }

        test "Should support struct unions" {

        }

        test "Should support struct tuples" {

        }
    ]
