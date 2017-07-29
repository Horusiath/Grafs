/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module Grafs.Tests.AsyncValTests

open Expecto
open Grafs

let tests = 
    testList "AsyncVal" [
        test "AsyncVal computation allows to return constant values" {
            let v = asyncVal { return 1 }
            Expect.isFalse (AsyncVal.isAsync v) "Should be synchronous"
            Expect.isTrue  (AsyncVal.isSync v) "Should be synchronous"
            Expect.equal 1 (AsyncVal.get v) "Value should be 1"
        }
        
        test "AsyncVal computation allows to return from async computation" {
            let v = asyncVal { return! async { return 2 } }
            Expect.isTrue  (AsyncVal.isAsync v) "Should be asynchronous"
            Expect.isFalse (AsyncVal.isSync v) "Should be asynchronous"
            Expect.equal 2 (AsyncVal.get v) "Value should be 2"
        }
        
        test "AsyncVal computation allows to return from another AsyncVal" {
            let v = asyncVal { return! asyncVal { return 3 } }
            Expect.isFalse (AsyncVal.isAsync v) "Should be synchronous"
            Expect.isTrue  (AsyncVal.isSync v) "Should be synchronous"
            Expect.equal 3 (AsyncVal.get v) "Value should be 3"
        }
        
        test "AsyncVal computation allows to bind async computations" {
            let v = asyncVal { 
                let! value = async { return 4 }
                return value }
            Expect.isTrue  (AsyncVal.isAsync v) "Should be asynchronous"
            Expect.isFalse (AsyncVal.isSync v) "Should be asynchronous"
            Expect.equal 4 (AsyncVal.get v) "Value should be 4"
        }
        
        test "AsyncVal computation allows to bind another AsyncVal" {
            let v = asyncVal { 
                let! value = asyncVal { return 5 }
                return value }
            Expect.isFalse (AsyncVal.isAsync v) "Should be synchronous"
            Expect.isTrue  (AsyncVal.isSync v) "Should be synchronous"
            Expect.equal 5 (AsyncVal.get v) "Value should be 5"
        }
        
        test "AsyncVal computation defines zero value" {
            let v = AsyncVal.empty
            Expect.isFalse (AsyncVal.isAsync v) "Should be synchronous"
            Expect.isTrue (AsyncVal.isSync v) "Should be synchronous"
        }
        
        test "AsyncVal can be returned from Async computation" {
            let a = async { return! asyncVal { return 6 } }
            let res = a |> sync
            Expect.equal 6 res "Should resolve 6 from nested async val"
        }
        
        test "AsyncVal can be bound inside Async computation" {
            let a = async { 
                let! v = asyncVal { return 7 }
                return v }
            let res = a |> sync
            Expect.equal 7 res "Should resolve 7 from nested async val"
        }
        
        test "AsyncVal sequential collection resolves all values in order of execution" {
            let mutable flag = "none"
            let a = async {
                // if async routines are run in order, this flag assignment should be first
                // even thou we are sleeping here
                do! Async.Sleep 1000
                flag <- "a"
                return 2
            }
            let b = async {
                flag <- "b"
                return 4 }
            let array = [| AsyncVal.ofValue 1; AsyncVal.ofAsync a; AsyncVal.ofValue 3; AsyncVal.ofAsync b |]
            let v = array |> AsyncVal.collectSequential
            Expect.equal [| 1; 2; 3; 4 |] (AsyncVal.get v) "Should collect async val results in order"
            Expect.equal "b" flag "Asynchronous values should be called in order of execution"
        }
        
        test "AsyncVal parallel collection resolves all values with no order of execution" {
            let mutable flag = "none"
            let a = async {
                // if async routines are run in parallel, this flag assignment should be second
                do! Async.Sleep 1000 
                flag <- "a"
                return 2
            }
            let b = async {
                flag <- "b"
                return 4 }
            let array = [| AsyncVal.ofValue 1; AsyncVal.ofAsync a; AsyncVal.ofValue 3; AsyncVal.ofAsync b |]
            let v = array |> AsyncVal.collectParallel
            Expect.equal [| 1; 2; 3; 4 |] (AsyncVal.get v) "Should collect all async val results"
            Expect.equal "a" flag "Asynchronous computations should run in parallel"
        }
    ]
