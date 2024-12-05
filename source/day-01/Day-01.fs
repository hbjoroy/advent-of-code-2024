module Day01

open System
open System.Text.RegularExpressions
open Xunit

[<Fact>]
let ``part 1`` () =
    let pattern= @"\s+"

    let parse(x:String) =
        let r = Regex.Split(x, pattern)        
        (Int32.Parse (r[0]), Int32.Parse(r[1]))        

    task {
        let! list = 
            IO.File.ReadAllLinesAsync("test02.txt")
        return 
            list
            |> Array.toList
            |> List.map (fun x -> parse(x))
            |> List.unzip
            |> fun (l1,l2) -> List.sort(l1), List.sort(l2)
            ||> List.zip
            |> List.map (fun (v1, v2) -> abs(v1 - v2))
            |> List.sum
    } 
    |> Async.AwaitTask 
    |> Async.RunSynchronously
    |> fun (length) ->
        printf "Totalen er: %i" length
        Assert.Equal(2970687, length)

[<Fact>]
let ``part 2`` () =
    let pattern= @"\s+"

    let parse(x:String) =
        let r = Regex.Split(x, pattern)        
        (Int32.Parse (r[0]), Int32.Parse(r[1]))        

    let similarity(l1:int list, l2:int list) =
        l1
        |> List.map (
            fun e -> l2 |> Seq.filter (fun x -> x=e)|> Seq.length |> ((*) e)
        )
        |> List.sum

    task {
        let! list = 
            IO.File.ReadAllLinesAsync("test02.txt")
        return 
            list
            |> Array.toList
            |> List.map (fun x -> parse(x))
            |> List.unzip
            |> similarity



    } 
    |> Async.AwaitTask 
    |> Async.RunSynchronously
    |> fun (length) ->
        printf "Totalen er: %i" length
        Assert.Equal(23963899, length)
