module Ploeh.Katas.TennisProperties

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open Tennis

[<Property>]
let ``Given game is over then it stays over`` (winner : Player) =
    let actual : Score = score (Game winner) (other winner)

    let expected = Game winner
    expected =! actual

[<Property>]
let ``Given advantage when advantaged player wins then score is correct``
    (advantagedPlayer : Player) =

    let actual : Score = score (Advantage advantagedPlayer) advantagedPlayer

    let expected = Game advantagedPlayer
    expected =! actual

[<Property>]
let ``Given advantage when other player wins then score is correct``
    (advantagedPlayer : Player) =

    let actual = score (Advantage advantagedPlayer) (other advantagedPlayer)
    Deuce =! actual

[<Property>]
let ``Given deuce when player wins then score is correct``
    (winner : Player) =

    let actual : Score = score Deuce winner

    let expected = Advantage winner
    expected =! actual

[<Property>]
let ``Given player: 40 when player wins then score is correct``
    (current : FortyData) =

    let actual = score (Forty current) current.Player

    let expected = Game current.Player
    expected =! actual

[<Property>]
let ``Given player: 40 - other: 30 when other wins then score is correct``
    (current : FortyData) =

    let current = { current with OtherPlayerPoint = Thirty }
    let actual = score (Forty current) (other current.Player)
    Deuce =! actual

[<Property>]
let ``Given player: 40 - other: < 30 when other wins then score is correct``
    (current : FortyData) =

    Gen.elements [Love; Fifteen]
    |> Arb.fromGen
    |> Prop.forAll <| fun otherPlayerPoint ->
        let current = { current with OtherPlayerPoint = otherPlayerPoint }

        let actual = score (Forty current) (other current.Player)

        let expected =
            incrementPoint current.OtherPlayerPoint
            |> Option.map (fun np -> Forty { current with OtherPlayerPoint = np })
        expected =! Some actual

[<Property>]
let ``Given player: 30 when player wins then score is correct``
    (current : PointsData)
    (winner : Player) = 

    let current = (Thirty |> pointTo winner) current

    let actual : Score = score (Points current) winner

    let expected = Forty {
        Player = winner
        OtherPlayerPoint = pointFor (other winner) current }
    expected =! actual

[<Property>]
let ``Given player: <30 when player wins then score is correct``
    (current : PointsData)
    (winner : Player) =

    Gen.elements [Love; Fifteen]
    |> Arb.fromGen
    |> Prop.forAll <| fun playerPoint ->
        let current = pointTo winner playerPoint current

        let actual = score (Points current) winner

        let expectedPlayerPoint =
            current
            |> pointFor winner
            |> incrementPoint
        let expected =
            expectedPlayerPoint
            |> Option.map (fun p -> current |> pointTo winner p |> Points)
        expected =! Some actual
