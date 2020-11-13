module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random.List exposing (shuffle)
import Svg exposing (Svg, circle, path, polyline, rect, svg, text_)
import Svg.Attributes as SA



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type GameMode
    = Playing
    | End


type CardType
    = Heart
    | Clover
    | Spade
    | Diamond


type alias PlayingCard =
    { cardType : CardType
    , number : Int
    }


type alias Model =
    { deck : List PlayingCard
    , dealer : List PlayingCard
    , player : List PlayingCard
    , status : GameMode
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] [] [] Playing
    , Random.generate CardSetUp (shufflePlayingCardsList createPlayingCardsList)
    )



-- UPDATE


type Msg
    = Hit
    | Stand
    | GameStart
    | CardSetUp (List PlayingCard)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStart ->
            ( model
            , Random.generate CardSetUp (shufflePlayingCardsList createPlayingCardsList)
            )

        CardSetUp playingCards ->
            ( { model
                | dealer = List.take 2 playingCards
                , player = List.take 2 (List.drop 2 playingCards)
                , deck = List.drop 4 playingCards
                , status = Playing
              }
            , Cmd.none
            )

        Hit ->
            ( { model
                | player = model.player ++ List.take 1 model.deck
                , deck = List.drop 1 model.deck
              }
            , Cmd.none
            )

        Stand ->
            let
                newModel =
                    dealerDrawCards model
            in
            ( { newModel
                | status = End
              }
            , Cmd.none
            )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "root" ]
        [ div [ class "header" ] [ text "elm で ブラックジャック" ]
        , case model.status of
            Playing ->
                div [ class "game-screen" ]
                    [ div []
                        [ div [] [ text "ディーラー" ]
                        , div []
                            (List.map convertCardDesignSvg (List.take 1 model.dealer))
                        ]
                    , div []
                        [ div [] [ text "プレイヤー" ]
                        , div []
                            (List.map convertCardDesignSvg model.player)
                        , div []
                            [ button [ onClick Hit, disabled (checkBust (calculatePoint model.player)) ] [ text "Hit" ]
                            , button [ onClick Stand ] [ text "Stand" ]
                            ]
                        ]
                    ]

            End ->
                div [ class "end-screen" ]
                    [ text
                        ("プレイヤーの"
                            ++ (if judgment (calculatePoint model.player) (calculatePoint model.dealer) then
                                    "勝ち！"

                                else
                                    "負け"
                               )
                        )
                    , div []
                        [ div [] [ text "ディーラー" ]
                        , div []
                            (List.map convertCardDesignSvg model.dealer)
                        ]
                    , div []
                        [ div [] [ text "プレイヤー" ]
                        , div []
                            (List.map convertCardDesignSvg model.player)
                        , div []
                            [ button [ onClick GameStart ] [ text "もう一回" ] ]
                        ]
                    ]
        ]



-- Module


createPlayingCardsList : List PlayingCard
createPlayingCardsList =
    List.map (\n -> PlayingCard Heart n) (List.range 1 13)
        ++ List.map (\n -> PlayingCard Clover n) (List.range 1 13)
        ++ List.map (\n -> PlayingCard Diamond n) (List.range 1 13)
        ++ List.map (\n -> PlayingCard Spade n) (List.range 1 13)


shufflePlayingCardsList : List PlayingCard -> Random.Generator (List PlayingCard)
shufflePlayingCardsList cards =
    shuffle cards


calculatePoint : List PlayingCard -> Int
calculatePoint cards =
    let
        cardNumbers =
            List.map getCardNumber cards
                |> List.map convertOverEleven

        noAceTotal =
            List.filter (\n -> n /= 1) cardNumbers
                |> List.foldl (+) 0

        aceCount =
            List.filter (\n -> n == 1) cardNumbers
                |> List.foldl (+) 0

        isAce =
            List.member 1 cardNumbers
    in
    if isAce then
        if noAceTotal > 10 then
            noAceTotal + aceCount

        else
            noAceTotal + 10 + aceCount

    else
        List.foldl (+) 0 cardNumbers


convertOverEleven : Int -> Int
convertOverEleven number =
    if number >= 11 then
        10

    else
        number


getCardNumber : PlayingCard -> Int
getCardNumber card =
    card.number


dealerDrawCards : Model -> Model
dealerDrawCards model =
    if calculatePoint model.dealer >= 17 then
        model

    else
        dealerDrawCards
            { model
                | dealer = model.dealer ++ List.take 1 model.deck
                , deck = List.drop 1 model.deck
            }


convertCardDesignSvg : PlayingCard -> Html msg
convertCardDesignSvg playingCard =
    let
        number =
            case playingCard.number of
                1 ->
                    "A"

                11 ->
                    "J"

                12 ->
                    "Q"

                13 ->
                    "K"

                _ ->
                    String.fromInt playingCard.number
    in
    case playingCard.cardType of
        Heart ->
            svg
                [ SA.width "90"
                , SA.height "120"
                , SA.viewBox "0 0 90 120"
                ]
                [ rect
                    [ SA.x "5"
                    , SA.y "5"
                    , SA.width "80"
                    , SA.height "110"
                    , SA.rx "15"
                    , SA.ry "15"
                    , SA.stroke "black"
                    , SA.fill "white"
                    ]
                    []
                , path
                    [ SA.d "M10 20 A10 10 0 1 1 30 20 A10 10 0 1 1 50 20 L30,50 Z"
                    , SA.fill "red"
                    ]
                    []
                , text_
                    [ SA.x "50"
                    , SA.y "100"
                    , SA.fontSize "35"
                    , SA.stroke "none"
                    , SA.fill "Red"
                    , SA.textAnchor "middle"
                    ]
                    [ text number ]
                ]

        Diamond ->
            svg
                [ SA.width "90"
                , SA.height "120"
                , SA.viewBox "0 0 90 120"
                ]
                [ rect
                    [ SA.x "5"
                    , SA.y "5"
                    , SA.width "80"
                    , SA.height "110"
                    , SA.rx "15"
                    , SA.ry "15"
                    , SA.stroke "black"
                    , SA.fill "white"
                    ]
                    []
                , polyline
                    [ SA.points "30,10,10,30 30,50 50,30"
                    , SA.fill "red"
                    ]
                    []
                , text_
                    [ SA.x "50"
                    , SA.y "100"
                    , SA.fontSize "35"
                    , SA.stroke "none"
                    , SA.fill "Red"
                    , SA.textAnchor "middle"
                    ]
                    [ text number ]
                ]

        Clover ->
            svg
                [ SA.width "90"
                , SA.height "120"
                , SA.viewBox "0 0 90 120"
                ]
                [ rect
                    [ SA.x "5"
                    , SA.y "5"
                    , SA.width "80"
                    , SA.height "110"
                    , SA.rx "15"
                    , SA.ry "15"
                    , SA.stroke "black"
                    , SA.fill "white"
                    ]
                    []
                , path
                    [ SA.d "M10 30 A10 10 0 1 0 30 30 A10 10 0 1 0 50 30 L30,10 Z"
                    , SA.fill "black"
                    ]
                    []
                , polyline
                    [ SA.points "30,30 25,50 35,50"
                    , SA.fill "black"
                    ]
                    []
                , text_
                    [ SA.x "50"
                    , SA.y "100"
                    , SA.fontSize "35"
                    , SA.stroke "none"
                    , SA.fill "Black"
                    , SA.textAnchor "middle"
                    ]
                    [ text number ]
                ]

        Spade ->
            svg
                [ SA.width "90"
                , SA.height "120"
                , SA.viewBox "0 0 90 120"
                ]
                [ rect
                    [ SA.x "5"
                    , SA.y "5"
                    , SA.width "80"
                    , SA.height "110"
                    , SA.rx "15"
                    , SA.ry "15"
                    , SA.stroke "black"
                    , SA.fill "white"
                    ]
                    []
                , circle
                    [ SA.cx "30"
                    , SA.cy "20"
                    , SA.r "10"
                    , SA.fill "black"
                    ]
                    []
                , circle
                    [ SA.cx "20"
                    , SA.cy "30"
                    , SA.r "10"
                    , SA.fill "black"
                    ]
                    []
                , circle
                    [ SA.cx "40"
                    , SA.cy "30"
                    , SA.r "10"
                    , SA.fill "black"
                    ]
                    []
                , polyline
                    [ SA.points "30,30 25,50 35,50"
                    , SA.fill "black"
                    ]
                    []
                , text_
                    [ SA.x "50"
                    , SA.y "100"
                    , SA.fontSize "35"
                    , SA.stroke "none"
                    , SA.fill "Black"
                    , SA.textAnchor "middle"
                    ]
                    [ text number ]
                ]


checkBust : Int -> Bool
checkBust point =
    if point > 21 then
        True

    else
        False


judgment : Int -> Int -> Bool
judgment playerPoint dealerPoint =
    if playerPoint > 21 then
        False

    else if dealerPoint > 21 then
        True

    else if dealerPoint < playerPoint then
        True

    else
        False
