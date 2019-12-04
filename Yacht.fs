module Yacht

type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One 
    | Two 
    | Three
    | Four 
    | Five 
    | Six

    with 
    member x.Number =
        match x with
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6


let allDieTheSame (dice : Die list) =
    dice 
    |> Seq.distinct
    |> Seq.length
    |> fun l -> l = 1
    
let scoreOnes (dice : Die list) =
    dice
    |> Seq.filter((=) Die.One)
    |> Seq.length

let scoreTwos (dice : Die list) =
    let count =
        dice
        |> Seq.filter((=) Die.Two)
        |> Seq.length
    count * 2 

let scoreThrees (dice : Die list) =
    let count =
        dice
        |> Seq.filter((=) Die.Three)
        |> Seq.length
    count * 3

let scoreFours (dice : Die list) =
    let count =
        dice
        |> Seq.filter((=) Die.Four)
        |> Seq.length
    count * 4

let scoreFives (dice : Die list) =
    let count =
        dice
        |> Seq.filter((=) Die.Five)
        |> Seq.length
    count * 5

let scoreSixes (dice : Die list) =
    let count =
        dice
        |> Seq.filter((=) Die.Six)
        |> Seq.length
    count * 6

let scoreFullHouse (dice : Die list) =
    match dice |> List.countBy(id) with
    | [(x,y); (z,q)] when y = 3 || q = 3 -> 
        x.Number * y + z.Number * q
    | _ -> 0

let scoreFourOfAKind (dice : Die list) =
    match dice |> List.countBy(id) with
    | [(x,y); (z,q)] when y = 4 -> 
        x.Number * y
    | [(x,y); (z,q)] when q = 4 -> 
        z.Number * y
    | _ -> 0

let score (category : Category) (dice : Die list) = 
    match category with
    | Yacht when allDieTheSame dice -> 50
    | Ones -> scoreOnes dice
    | Twos -> scoreTwos dice
    | Threes -> scoreThrees dice
    | Fours -> scoreFours dice
    | Fives -> scoreFives dice
    | Sixes -> scoreSixes dice
    | FullHouse -> scoreFullHouse dice
    | FourOfAKind -> scoreFourOfAKind dice
    | _ -> 0