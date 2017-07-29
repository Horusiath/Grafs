/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace Grafs

open System.Collections.Generic

/// A common lightweight abstraction over either an asynhronous computation 
/// with value deferred in time, or a synchronous computation with a value 
/// available right away.
[<Struct>]
type AsyncVal<'T> =
    | Synchronous of value:'T
    | Asynchronous of cont:Async<'T>
    static member Zero = Synchronous Unchecked.defaultof<'T>

[<RequireQualifiedAccess>]
module AsyncVal =

    let inline ofAsync a = Asynchronous a
    let inline ofValue v = Synchronous v
    let inline ofError e = Asynchronous (async { raise e; return Unchecked.defaultof<_> })

    let isAsync = function Asynchronous _ -> true | _ -> false
    let isSync = function Asynchronous _ -> true | _ -> false
    
    let inline toAsync av =
        match av with
        | Synchronous v  -> async.Return v
        | Asynchronous a -> a

    let inline get av =
        match av with
        | Synchronous v  -> v
        | Asynchronous a -> Async.RunSynchronously(a)

    let inline empty<'T> = AsyncVal<'T>.Zero

    let map fn =
        function
        | Synchronous v -> Synchronous(fn v)
        | Asynchronous a -> 
            async {
                let! v = a
                return fn v
            } |> Asynchronous

    let bind (binder: 'T -> AsyncVal<'U>) =
        function
        | Synchronous v -> binder v
        | Asynchronous a -> async {
            let! v = a
            let bound = binder v
            match bound with
            | Synchronous v'  -> return v'
            | Asynchronous a' -> return! a' } |> Asynchronous
            
    /// Applies rescue fn in case when contained Async value throws an exception.
    let rescue (fn: exn -> 'T) (x: AsyncVal<'T>) =
        match x with
        | Synchronous v -> x
        | Asynchronous a ->
            async {
                try return! a
                with e -> return fn e
            } |> Asynchronous
            
    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are 
    /// executed asynchronously, one by one with regard to their order in array.
    /// Returned array maintain order of values.
    /// If the array contains a Failure, then the entire array will not resolve
    let collectSequential (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        if values.Length = 0 then Synchronous [||]
        elif values |> Array.exists isAsync then
            Asynchronous(async {
                let results = Array.zeroCreate values.Length
                for i = 0 to values.Length - 1 do
                    let v = values.[i]
                    match v with
                    | Synchronous v -> results.[i] <- v
                    | Asynchronous a ->
                        let! r = a
                        results.[i] <- r
                return results })
        else Synchronous (values |> Array.map (fun (Synchronous v) -> v))

            

    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are 
    /// executed all in parallel, in unordered fashion. Order of values
    /// inside returned array is maintained.
    /// If the array contains a Failure, then the entire array will not resolve
    let collectParallel (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        if values.Length = 0 then Synchronous [||]
        else
            let indexes = List<_>(0)
            let continuations = List<_>(0)
            let results = Array.zeroCreate values.Length
            for i = 0 to values.Length - 1 do
                let value = values.[i]
                match value with
                | Synchronous v -> results.[i] <- v
                | Asynchronous a ->
                    indexes.Add i
                    continuations.Add a
            if indexes.Count = 0
            then Synchronous(results)
            else Asynchronous(async {
                let! vals = continuations |> Async.Parallel
                for i = 0 to indexes.Count - 1 do
                    results.[indexes.[i]] <- vals.[i]
                return results })

 
 type AsyncValBuilder() =
    member __.Zero () = AsyncVal.empty
    member __.Return v = Synchronous v
    member __.ReturnFrom (v: AsyncVal<_>) = v
    member __.ReturnFrom (a: Async<_>) = Asynchronous a
    member __.Bind (v: AsyncVal<'T>, binder: 'T -> AsyncVal<'U>) = 
        AsyncVal.bind binder v
    member x.Bind (a: Async<'T>, binder: 'T -> AsyncVal<'U>) = 
        async {
            let! value = a
            let bound = binder value
            match bound with
            | Synchronous v'  -> return v'
            | Asynchronous a' -> return! a' } |> Asynchronous

[<AutoOpen>]
module AsyncValEx =

    let asyncVal = AsyncValBuilder()
    
    type Microsoft.FSharp.Control.AsyncBuilder with

        member x.ReturnFrom (v: AsyncVal<'T>) =
            match v with
            | Synchronous v  -> async.Return v
            | Asynchronous a -> async.ReturnFrom a

        member x.Bind (v: AsyncVal<'T>, binder) =
            match v with
            | Synchronous v  -> async.Bind(async.Return v, binder)
            | Asynchronous a -> async.Bind(a, binder)
