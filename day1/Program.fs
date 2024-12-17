// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO

type Pair = int * int

let getDistance (pair: Pair) =
    let x, y = pair
    abs (x - y)

let getTotalDistance (pairs: Pair list) =
    pairs |> List.sumBy getDistance

let text2lists (text: string) =
    let left, right =
        text.Split('\n')
        |> List.ofArray
        |> List.filter (fun line -> not (String.IsNullOrWhiteSpace(line)))
        |> List.map (fun line ->
            let locationIds = line.Split("   ")
            int locationIds[0], int locationIds[1]) // return left and right
        |> List.unzip
    left, right

let rec filterSingle criteria list =
    match list with
    | [] -> []
    | x::xs when x = criteria -> xs
    | x::xs -> x :: filterSingle criteria xs

let rec findPairs (left: int list, right: int list, accumulatorPairs: Pair list) : Pair list =
    match left, right with
    | [], [] -> accumulatorPairs
    | _ ->
        let minLeft = List.min left
        let minRight = List.min right
        let newLeft = filterSingle minLeft left
        let newRight = filterSingle minRight right
        findPairs (newLeft, newRight, Pair(minLeft, minRight) :: accumulatorPairs)

let getSimilarityScore (numbers, distribution) =
    numbers
    |> List.map (fun x ->
            if Map.containsKey x distribution then
                x * distribution[x]
            else
                x * 0
        )
    |> List.sum

let input = File.ReadAllText("input.txt")
// let input = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
let left, right = text2lists input

// Part One
let pairs = findPairs (left, right, [])
let totalDistance = getTotalDistance pairs

printfn "Part One"
printfn $"Total Distance = {totalDistance}"

// Part Two
let rightDistribution =
    right
    |> List.fold (fun accumulatorDistributionMap x ->
        if Map.containsKey x accumulatorDistributionMap then
            Map.add x (accumulatorDistributionMap[x] + 1) accumulatorDistributionMap
        else
            Map.add x 1 accumulatorDistributionMap) Map.empty
let similarityScore = getSimilarityScore (left, rightDistribution)

printfn "Part Two"
printfn $"Similarity Score = {similarityScore}"
