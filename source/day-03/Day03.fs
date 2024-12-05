module Day03

open System
open Xunit
open System.Text.RegularExpressions
[<Fact>]
let ``Part 1`` () =

    let testString= IO.File.ReadAllText("test02.txt")
    //let testString = @"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    let p = @"mul\([0-9]{1,3},[0-9]{1,3}\)"
    Regex.Matches(testString, p)
    |> Seq.map (fun x -> Regex.Matches(x.Value, @"[0-9]{1,3}") |> Seq.map (fun x -> int32 x.Value))
    |> Seq.map (fun ps -> Seq.fold (fun acc v -> acc*v ) 1 ps)
    |> Seq.sum
    |> fun sum -> Assert.Equal(175015740,sum) // 161 for liten tekst

type Instruction =
    | Mul of int*int
    | Do
    | Dont
    | Unknown

[<Fact>]
let ``Part 2`` () =

    let rec processInstructions enabled instructions  =
        let parseInstruction instruction =
            let muls = 
                Regex.Match(instruction, @"mul\(([0-9]{1,3}),([0-9]{1,3})\)")
            let dos = Regex.Match(instruction, @"do\(\)")
            let donts = Regex.Match(instruction, @"don't\(\)")

            if dos.Success then
                Do
            else if donts.Success then
                Dont
            else if muls.Success then
                let v1 = muls.Groups[1].Value |> int32
                let v2 = muls.Groups[2].Value |> int32
                Mul (v1,v2)
            else 
                Unknown


        match instructions with
        | [] -> 0
        | h::t -> 
            parseInstruction h
            |> function 
            | Do -> processInstructions true t 
            | Dont -> processInstructions false t 
            | Mul (v1,v2) -> 
                if enabled then
                    v1*v2 + processInstructions enabled t 
                else
                    processInstructions enabled t 
            | Unknown -> processInstructions enabled t 

    //let testString = @"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    let testString= IO.File.ReadAllText("test02.txt")
    let p = @"(mul\([0-9]{1,3},[0-9]{1,3}\)|don't\(\)|do\(\))"
    Regex.Matches(testString, p)
    |> Seq.map (fun x -> x.Value )
    |> Seq.toList
    |> (processInstructions true)
    |> fun sum -> Assert.Equal(112272912,sum) // 48 for liten tekst
