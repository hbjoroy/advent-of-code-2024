module Day05

open System
open System.Text.RegularExpressions
open Xunit



[<Theory>]
[<InlineData("day05-01.txt",143)>]
let ``My test`` (file, (medianSum:int)) = task {
    let! file = IO.File.ReadAllLinesAsync(file)
    let defs,jobs = Array.splitAt(Array.findIndex ((=) "")file ) file
    let jobs = Array.skip 1 jobs

    let m = Map<int,int[]>


    Assert.Equal(21, defs.Length)
    Assert.Equal(6, jobs.Length)
}

