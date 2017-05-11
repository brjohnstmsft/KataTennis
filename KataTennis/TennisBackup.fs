﻿module Ploeh.Katas.TennisBackup

type Player = PlayerOne | PlayerTwo
type Point = Love | Fifteen | Thirty

type PointsData = { PlayerOnePoint : Point; PlayerTwoPoint : Point }
type FortyData = { Player : Player; OtherPlayerPoint : Point }

type Score =
| Points of PointsData
| Forty of FortyData
| Deuce
| Advantage of Player
| Game of Player

let other = function PlayerOne -> PlayerTwo | PlayerTwo -> PlayerOne

let incrementPoint = function
    | Love -> Some Fifteen
    | Fifteen -> Some Thirty
    | Thirty -> None

let pointTo player point current =
    match player with
    | PlayerOne -> { current with PlayerOnePoint = point }
    | PlayerTwo -> { current with PlayerTwoPoint = point }

let pointFor player current =
    match player with
    | PlayerOne -> current.PlayerOnePoint
    | PlayerTwo -> current.PlayerTwoPoint

// Transitions

// State machine

let score current winner = 
    let scoreWhenGame winner = Game winner

    let scoreWhenAdvantage advantagedPlayer winner =
        if advantagedPlayer = winner
        then Game winner
        else Deuce

    let scoreWhenDeuce winner = Advantage winner

    let scoreWhenForty current winner =
        if current.Player = winner
        then Game winner
        else
            match incrementPoint current.OtherPlayerPoint with
            | Some p -> Forty { current with OtherPlayerPoint = p }
            | None -> Deuce

    let scoreWhenPoints current winner =
        match pointFor winner current |> incrementPoint with
        | Some np -> pointTo winner np current |> Points
        | None -> Forty {
            Player = winner
            OtherPlayerPoint = pointFor (other winner) current }

    match current with
    | Points p -> scoreWhenPoints p winner
    | Forty f -> scoreWhenForty f winner
    | Deuce -> scoreWhenDeuce winner
    | Advantage a -> scoreWhenAdvantage a winner
    | Game g -> scoreWhenGame g

let newGame = Points { PlayerOnePoint = Love; PlayerTwoPoint = Love }