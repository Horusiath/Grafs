/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module Grafs.Tests.JsonTests

open Expecto
open Grafs
open Grafs.Json


let tests = 
    testList "GQLValue" [
        test "Can serialize itself to JSON string" {
            let value = GQLObj [
                struct ("a", GQLNull)
                struct ("b", GQLInt 1)
                struct ("c", GQLString "ok")
                struct ("d", GQLBool true)
                struct ("e", GQLBool false)
                struct ("f", GQLFloat 1.2)
                struct ("g", GQLList [ GQLString "ok"; GQLInt 1 ])
                struct ("h", GQLObj [
                    struct ("ha", GQLList [])
                ])
            ]
            let expected = """{"a":null,"b":1,"c":"ok","d":true,"e":false,"f":1.2,"g":["ok",1],"h":{"ha":[]}}"""
            Expect.equal expected (value.ToJson()) "GQLValue should be stringified to compact JSON string"
        }
    ]