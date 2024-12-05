module Day02

open System
open System.IO
open System.Text.RegularExpressions
open Xunit

type Direction = 
    | Up
    | Down
    | Undetermined

type ReportState = 
    | Safe
    | Unsafe

let parseReport (line:string) =
    Regex.Split(line, @"\s+")
    |> Array.toList
    |> List.map Int32.Parse

let processReport grace (report:int list)  =
    let rec checkNext direction subReport grace =
        let directionOk dir val1 val2 =
            match dir with
            | Up -> val1 < val2
            | Down -> val1 > val2
            | Undetermined -> true

        let distanceOk val1 val2 =
            let d= abs (val1-val2)
            0<d && d<=3

        let determineDirection val1 val2 =
            if val1<val2 then Up else Down // ignore equal

        match subReport with
        | [] -> raise (Exception())
        | _::[] -> Safe
        | e1::e2::tail -> 
            let dir = 
                if direction=Undetermined then 
                    determineDirection e1 e2
                else 
                    direction

            if distanceOk e1 e2 && directionOk dir e1 e2 then
                checkNext dir (e2::tail) grace
            else
                if grace>0 then 
                    checkNext dir (e1::tail) (grace-1) 
                else 
                    Unsafe

    let check1 = checkNext Undetermined report grace

    if check1=Unsafe then
        checkNext Undetermined (List.rev report) grace
    else
        check1


[<Fact>]
let ``Part 1`` () = task {
    let file = "test02.txt"
    let! lines = File.ReadAllLinesAsync(file)
    
    let safeCount = 
        lines
        |> Seq.filter (fun x -> x[0]<>'#')
        |> Seq.map parseReport
        |> Seq.map (processReport 0)
        |> Seq.filter ((=) Safe) 
        |> Seq.length
    
    Assert.Equal(670, safeCount)
}

[<Fact>]
let ``Part 2`` () = task {
    let file = "test02.txt"
    let! lines = File.ReadAllLinesAsync(file)
    
    let safeCount = 
        lines
        |> Seq.filter (fun x -> x[0]<>'#')
        |> Seq.map parseReport
        |> Seq.map (processReport 1)
        |> Seq.filter ((=) Safe)
        |> Seq.length
    Assert.Equal(700, safeCount)
}