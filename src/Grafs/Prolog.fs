/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace Grafs

open System
open System.Reactive
open System.Reactive.Linq
open System.Collections.Generic

/// Common base class for all GraphQL related exceptions.
type GQLException(msg) = inherit Exception(msg)

/// Common base class for all GraphQL related exceptions.
type MalformedQueryException(msg) = inherit GQLException(msg)

module internal Array =

    /// <summary>
    /// Returns a new array with unique elements. Uniqueness is determined by
    /// output of the <paramref name="keyf"/> function.
    /// </summary>
    /// <param name="keyf">Function, which output is used to determine uniqueness of input elements.</param>
    /// <param name="array">Array of elements.</param>
    let distinctBy keyf (array:'T[]) =
            let temp = Array.zeroCreate array.Length
            let mutable i = 0 
            let hashSet = HashSet<_>(HashIdentity.Structural<_>)
            for v in array do
                if hashSet.Add(keyf v) then
                    temp.[i] <- v
                    i <- i + 1
            Array.sub temp 0 i

module internal List =
    
    /// <summary>
    /// Merges elements of two lists, returning a new list without duplicates.
    /// </summary>
    /// <param name="f">Function used to determine if any two given elements are considered equal.</param>
    /// <param name="listx">First list with elements to merge.</param>
    /// <param name="listy">Second list with elements to merge.</param>
    let mergeBy f listx listy =
        let uniqx = 
            listx
            |> List.filter (fun x -> not <| List.exists(fun y -> f(x) = f(y)) listy)
        uniqx @ listy

module internal Set =

    /// <summary>
    /// Maps over each of the <paramref name="set"/> elements, applying function
    /// over each one of them to generate new Set. Sets generated this way are
    /// then flattened into single output set.
    /// </summary>
    /// <param name="f">Function used to generate Set from each of the input's elements.</param>
    /// <param name="set">Input set.</param>
    let collect f set = set |> Set.fold (fun acc e -> acc + f e) Set.empty
    
module internal Map =

    /// <summary>
    /// Merges the entries of two maps by their key, returning new map in result.
    /// </summary>
    /// <param name="mergeFn">
    /// Function, which takes key shared by entries in both maps, first entry's value,
    /// second entry's value to produce a result value used in newly generated map.
    /// </param>
    /// <param name="mapx">First map with elements to merge.</param>
    /// <param name="mapy">Second map with elements to merge.</param>
    let merge mergeFn mapx mapy =
        mapy
        |> Map.fold (fun acc ky vy -> 
            match Map.tryFind ky acc with
            | Some vx -> Map.add ky (mergeFn ky vx vy) acc
            | None -> Map.add ky vy acc) mapx

module internal Observable =
    
    let bind (f: 'T -> IObservable<'U>) (o: IObservable<'T>) = o.SelectMany(f)

    let ofAsync asyncOp = Observable.FromAsync(fun token -> Async.StartAsTask(asyncOp,cancellationToken = token))

    let ofSeq<'Item>(items:'Item seq) : IObservable<'Item> = {   
        new IObservable<_> with
            member __.Subscribe( observer:IObserver<_> ) =
                for item in items do observer.OnNext item      
                observer.OnCompleted()     
                {   new IDisposable with member __.Dispose() = ()   }
    }