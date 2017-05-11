module Ploeh.Katas.Tennis

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

let incrementPoint =
    function
    | Love -> Some Fifteen 
    | Fifteen -> Some Thirty
    | Thirty -> None

let pointTo player points current =
    match player with
    | PlayerOne -> { current with PlayerOnePoint = points }
    | PlayerTwo -> { current with PlayerTwoPoint = points }

let pointFor player current =
    match player with
    | PlayerOne -> current.PlayerOnePoint
    | PlayerTwo -> current.PlayerTwoPoint

let score (current : Score) (player : Player) =
    let scoreWhenForty { Player = p; OtherPlayerPoint = otherPoint } =
        if p = player then Game p
        else
            match incrementPoint otherPoint with
            | Some np -> Forty { Player = p; OtherPlayerPoint = np }
            | None -> Deuce

    let scoreWhenPoints current =
        let newPoints = pointFor player current |> incrementPoint
        match newPoints with
        | Some p -> Points (pointTo player p current)
        | None -> Forty { Player = player; OtherPlayerPoint = pointFor (other player) current }

    match current with
    | Game _ -> current
    | Advantage p when p = player -> Game p
    | Advantage p -> Deuce
    | Deuce -> Advantage player
    | Forty f -> scoreWhenForty f
    | Points p -> scoreWhenPoints p
    | _ -> current

let newGame = Points { PlayerOnePoint = Love; PlayerTwoPoint = Love }

let scoreSeq wins = Seq.fold score newGame wins
