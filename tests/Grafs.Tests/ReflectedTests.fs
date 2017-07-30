/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module Grafs.Tests.ReflectedTests

open Expecto
open Grafs
open Grafs.Reflection

[<Description("Test desc")>]
type SimpleCaseType = 
    { X: int; 
      Y: bool; 
      Z: string }

type OutputOptionType = 
    { O: int option }

let tests = 
    testList "ConstructionTests" [

        test "A reflected constructor works for a simple cases" {
            let ctx = ConstructionContext.Default()
            let info = constructInfo ctx typeof<SimpleCaseType>
            let actual = ctx.KnownTypes.["SimpleCaseType"]
            
            let expectedMeta = 
                { DeprecationReason = None
                  Description = Some "Test desc"
                  GenericTypeName = None
                  Kind = TypeKind.Object
                  DependentTypes = [| typeof<int>; typeof<bool>; typeof<string> |] }

            Expect.equal "SimpleCaseType" actual.Name "Type definition name should reflect original type name"
            Expect.equal typeof<SimpleCaseType> actual.ClrType "Type correlation should be kept"
            Expect.equal expectedMeta actual.Metadata "Type definition metadata should refer to used types and description provided"
            Expect.equal 3 (actual.Members.Length) "SimpleCaseType has 3 available fields"

            Expect.equal (Property("x", typeof<int>, MemberMetadata.Zero, null, None)) actual.Members.[0] "Member fields should have their name camel cased"
            Expect.equal (Property("y", typeof<bool>, MemberMetadata.Zero, null, None)) actual.Members.[1] "Member fields should have their name camel cased"
            Expect.equal (Property("z", typeof<string>, MemberMetadata.Zero, null, None)) actual.Members.[2] "Member fields should have their name camel cased"
        }
        
        test "A reflected constructor works with output options" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with output sequences" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with nested complex objects" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with methods taking simple inputs" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with methods taking input objects" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with methods taking input options" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with methods taking input sequences" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with methods taking inputs with default values" {
            failwith "not implemented"
        }

        test "A reflected constructor works with output types having single generic parameter" {
            failwith "not implemented"
        }
        
        test "A reflected constructor doesn't work with output types having more than single generic parameter" {
            failwith "not implemented"
        }
        
        test "A reflected constructor doesn't work with input types having more than single generic parameter" {
            failwith "not implemented"
        }

        test "A reflected constructor doesn't work with generic methods" {
            failwith "not implemented"
        }

        test "A reflected constructor works with interfaces as type definitions" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with discriminated unions as type definitions" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with enums as type definitions" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works with ObsoleteAttribute" {
            failwith "not implemented"
        }
        
        test "A reflected constructor works GQLIgnoreAttribute" {
            failwith "not implemented"
        }
    ]