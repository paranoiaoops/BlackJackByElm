module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random.List exposing (shuffle)



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
    = Menu
    | Playing
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
    ( Model [] [] [] Menu
    , Cmd.none
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
    div []
        [ header [] [ text "elm で ブラックジャック" ]
        , case model.status of
            Menu ->
                div []
                    [ button [ onClick GameStart ] [ text "Game Start" ]
                    ]

            Playing ->
                div []
                    [ div []
                        [ div [] [ text "ディーラー" ]
                        , ul []
                            (List.map (\l -> li [] [ text (String.fromInt l.number) ]) model.dealer)
                        ]
                    , div []
                        [ div [] [ text "プレイヤー" ]
                        , ul []
                            (List.append
                                (List.map (\l -> li [] [ text (String.fromInt l.number) ]) model.player)
                                [ li [] [ text ("Total : " ++ String.fromInt (calculatePoint model.player)) ] ]
                            )
                        , div []
                            [ button [ onClick Hit ] [ text "Hit" ]
                            , button [ onClick Stand ] [ text "Stand" ]
                            ]
                        ]
                    ]

            End ->
                div []
                    [ text ("プレイヤーの" ++ judgment model.player model.dealer)
                    , div []
                        [ text ("ディラー：" ++ String.fromInt (calculatePoint model.dealer)) ]
                    , div []
                        [ text ("プレイヤー：" ++ String.fromInt (calculatePoint model.player)) ]
                    , div []
                        [ button [ onClick GameStart ] [ text "もう一回" ] ]
                    ]
        ]



-- Module
-- 山札の生成


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


judgment : List PlayingCard -> List PlayingCard -> String
judgment playerCards dealerCards =
    let
        playerPoint =
            calculatePoint playerCards

        dealerPoint =
            calculatePoint dealerCards
    in
    if playerPoint > 21 then
        "プレイヤーの負け"

    else if dealerPoint > 21 then
        "プレイヤーの負け"

    else if dealerPoint < playerPoint then
        "プレイヤーの勝ち"

    else
        "プレイヤーの負け"
