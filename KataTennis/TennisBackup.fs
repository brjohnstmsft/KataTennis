module Ploeh.Katas.TennisBackup

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

// State machine

let score current winner = 
    let scoreWhenGame winner = Game winner

    let scoreWhenAdvantage advantagedPlayer =
        if advantagedPlayer = winner then Game winner
        else Deuce

    let scoreWhenDeuce = Advantage winner

    let scoreWhenForty current =
        if current.Player = winner then Game winner
        else
            match incrementPoint current.OtherPlayerPoint with
            | Some p -> Forty { current with OtherPlayerPoint = p }
            | None -> Deuce

    let scoreWhenPoints current =
        match pointFor winner current |> incrementPoint with
        | Some np -> pointTo winner np current |> Points
        | None -> Forty {
            Player = winner
            OtherPlayerPoint = pointFor (other winner) current }

    match current with
    | Points p -> scoreWhenPoints p
    | Forty f -> scoreWhenForty f
    | Deuce -> scoreWhenDeuce
    | Advantage a -> scoreWhenAdvantage a
    | Game g -> scoreWhenGame g

let newGame = Points { PlayerOnePoint = Love; PlayerTwoPoint = Love }
