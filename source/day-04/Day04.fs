module Day04

open System
open Xunit
open System.Text

type Direction = {
    dx: int
    dy: int
}

let directions = [
    { dx=  1; dy=  0}
    { dx=  1; dy=  1}
    { dx=  0; dy=  1}
    { dx= -1; dy=  1}
]
let directions2 = [
    { dx=  1; dy= -1}
    { dx=  1; dy=  1}
]

let countWords directions (file:string array) wordLength (cmp:string->bool) (position:int*int) =
    let findWord direction =
        let x,y = position
        // let xskew = if direction.dx<0 && x-wordLength+1<0 then wordLength-1 else 0
        // let yskew = if direction.dy<0 && y-wordLength+1<0 then wordLength-1 else 0
        // let xstop = if direction.dx>0 && x+wordLength-1>file[0].Length then wordLength-1 else wordLength-1
        let xr= seq { for i=0 to wordLength - 1 do i*direction.dx + x}
        let yr = seq { for i=0 to wordLength - 1 do i*direction.dy + y}
        (xr,yr)
        ||> Seq.zip
        |> Seq.map (fun (px,py) -> 
            if py<file.Length && py>=0 && px<file[0].Length && px>=0 then
                file[py][px]
            else
                ' '
        )
        |> Seq.fold (fun (acc:StringBuilder) v -> acc.Append(v)) (new StringBuilder())
        |> _.ToString()
    directions
    |> Seq.map findWord
    |> Seq.filter cmp
    |> Seq.length

let doAssert<'a> (target:'a) (actual:'a) =
    Assert.Equal(target,actual) 

let positions xmax ymax = seq {
    for x in 0..xmax do
        for y in 0..ymax do
            yield (x,y)
}

let extractSubBoard (file:string array) (xpos, ypos)=
    seq { 
        for y=ypos to ypos + 2 do 
            seq { for x=xpos to xpos + 2 do file[y][x] }
            |> Seq.fold (fun (acc:StringBuilder) v -> acc.Append(v)) (new StringBuilder())
            |> _.ToString()
    } |> Seq.toArray

[<Theory>]
[<InlineData("day04-01.txt",18)>]
[<InlineData("day04-02.txt",2427)>]
let ``Part 1`` (filename, (count:int)) = task {
    let! file = IO.File.ReadAllLinesAsync(filename)
    let xmax = String.length file[0]-1
    let ymax = file.Length-1
    
    positions xmax ymax    
    |> Seq.map (countWords directions file 4 (fun w ->  w="XMAS"||w="SAMX"))
    |> Seq.sum
    |> doAssert count
}

[<Theory>]
[<InlineData("day04-01.txt",9)>]
[<InlineData("day04-02.txt",1900)>]
let ``Part 2`` (filename, (count:int)) = task {
    let! file = IO.File.ReadAllLinesAsync(filename)
    let xmax = String.length file[0]-3
    let ymax = file.Length-3
    
    let isXmas subBoard =
        positions 0 2
        |> Seq.map (countWords directions2 subBoard 3 (fun w -> w="MAS" || w="SAM"))
        |> Seq.sum
        |> ((=) 2)

    positions xmax ymax    
    |> Seq.map (extractSubBoard file)
    |> Seq.map isXmas
    |> Seq.filter ((=) true)
    |> Seq.length
    |> doAssert count
}
