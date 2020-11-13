module Example exposing (..)

import Expect exposing (Expectation)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "Card control module"
        [ describe "convert over eleven card"
            [ test "convert" <|
                \_ ->
                    Main.convertOverEleven 11
                        |> Expect.equal 10
            , test "not concert" <|
                \_ ->
                    Main.convertOverEleven 10
                        |> Expect.equal 10
            ]
        , describe "calculate black jack point"
            [ test "no ace and no eleven over no 1" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 3
                            , Main.PlayingCard Main.Heart 5
                            , Main.PlayingCard Main.Heart 10
                            , Main.PlayingCard Main.Heart 2
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 20
            , test "no ace and no eleven over no 2" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 3
                            , Main.PlayingCard Main.Heart 5
                            , Main.PlayingCard Main.Heart 10
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 18
            , test "ace equal eleven no 1" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 1
                            , Main.PlayingCard Main.Heart 5
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 16
            , test "ace equal eleven no 2" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 1
                            , Main.PlayingCard Main.Heart 5
                            , Main.PlayingCard Main.Heart 5
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 21
            , test "ace equal eleven no 3" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 1
                            , Main.PlayingCard Main.Heart 2
                            , Main.PlayingCard Main.Heart 4
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 17
            , test "ace equal eleven no 4" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 1
                            , Main.PlayingCard Main.Heart 2
                            , Main.PlayingCard Main.Heart 4
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 17
            , test "ace two card" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 1
                            , Main.PlayingCard Main.Heart 2
                            , Main.PlayingCard Main.Clover 1
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 14
            , test "Black Jack" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 1
                            , Main.PlayingCard Main.Clover 13
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 21
            , test "no ace, eleven over no 1" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 13
                            , Main.PlayingCard Main.Clover 5
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 15
            , test "no ace, eleven over no 2" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 13
                            , Main.PlayingCard Main.Clover 2
                            , Main.PlayingCard Main.Clover 5
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 17
            , test "ace, eleven over no 1" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 11
                            , Main.PlayingCard Main.Clover 2
                            , Main.PlayingCard Main.Clover 1
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 13
            , test "ace, eleven over no 2" <|
                \_ ->
                    let
                        testCardList =
                            [ Main.PlayingCard Main.Heart 12
                            , Main.PlayingCard Main.Clover 5
                            , Main.PlayingCard Main.Clover 8
                            , Main.PlayingCard Main.Clover 1
                            ]
                    in
                    Main.calculatePoint testCardList
                        |> Expect.equal 24
            ]
        , describe "dealer logic"
            [ test "not draw pattern no 1" <|
                \_ ->
                    let
                        dummyModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 10
                                , Main.PlayingCard Main.Heart 3
                                ]
                                [ Main.PlayingCard Main.Diamond 7
                                , Main.PlayingCard Main.Diamond 10
                                ]
                                []
                                Main.End

                        expectModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 10
                                , Main.PlayingCard Main.Heart 3
                                ]
                                [ Main.PlayingCard Main.Diamond 7
                                , Main.PlayingCard Main.Diamond 10
                                ]
                                []
                                Main.End
                    in
                    Main.dealerDrawCards dummyModel
                        |> Expect.equal expectModel
            , test "not draw pattern no 2" <|
                \_ ->
                    let
                        dummyModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 10
                                , Main.PlayingCard Main.Heart 3
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 10
                                , Main.PlayingCard Main.Diamond 3
                                ]
                                []
                                Main.End

                        expectModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 10
                                , Main.PlayingCard Main.Heart 3
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 10
                                , Main.PlayingCard Main.Diamond 3
                                ]
                                []
                                Main.End
                    in
                    Main.dealerDrawCards dummyModel
                        |> Expect.equal expectModel
            , test "draw pattern no 1" <|
                \_ ->
                    let
                        dummyModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 13
                                , Main.PlayingCard Main.Heart 11
                                , Main.PlayingCard Main.Heart 3
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 3
                                ]
                                []
                                Main.End

                        expectModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 11
                                , Main.PlayingCard Main.Heart 3
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 3
                                , Main.PlayingCard Main.Heart 13
                                ]
                                []
                                Main.End
                    in
                    Main.dealerDrawCards dummyModel
                        |> Expect.equal expectModel
            , test "draw pattern no 2" <|
                \_ ->
                    let
                        dummyModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 3
                                , Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 6
                                , Main.PlayingCard Main.Heart 4
                                , Main.PlayingCard Main.Heart 8
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 3
                                ]
                                []
                                Main.End

                        expectModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 4
                                , Main.PlayingCard Main.Heart 8
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 3
                                , Main.PlayingCard Main.Heart 3
                                , Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 6
                                ]
                                []
                                Main.End
                    in
                    Main.dealerDrawCards dummyModel
                        |> Expect.equal expectModel
            , test "draw pattern no 3" <|
                \_ ->
                    let
                        dummyModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 1
                                , Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 6
                                , Main.PlayingCard Main.Heart 4
                                , Main.PlayingCard Main.Heart 8
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 3
                                ]
                                []
                                Main.End

                        expectModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 5
                                , Main.PlayingCard Main.Heart 6
                                , Main.PlayingCard Main.Heart 4
                                , Main.PlayingCard Main.Heart 8
                                ]
                                [ Main.PlayingCard Main.Diamond 4
                                , Main.PlayingCard Main.Diamond 3
                                , Main.PlayingCard Main.Heart 1
                                ]
                                []
                                Main.End
                    in
                    Main.dealerDrawCards dummyModel
                        |> Expect.equal expectModel
            , test "draw pattern no 4" <|
                \_ ->
                    let
                        dummyModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 1
                                , Main.PlayingCard Main.Heart 2
                                , Main.PlayingCard Main.Heart 6
                                , Main.PlayingCard Main.Heart 4
                                , Main.PlayingCard Main.Heart 8
                                ]
                                [ Main.PlayingCard Main.Diamond 3
                                , Main.PlayingCard Main.Diamond 2
                                ]
                                []
                                Main.End

                        expectModel =
                            Main.Model
                                [ Main.PlayingCard Main.Heart 6
                                , Main.PlayingCard Main.Heart 4
                                , Main.PlayingCard Main.Heart 8
                                ]
                                [ Main.PlayingCard Main.Diamond 3
                                , Main.PlayingCard Main.Diamond 2
                                , Main.PlayingCard Main.Heart 1
                                , Main.PlayingCard Main.Heart 2
                                ]
                                []
                                Main.End
                    in
                    Main.dealerDrawCards dummyModel
                        |> Expect.equal expectModel
            ]
        , describe "judgment test"
            [ test "player win" <|
                \_ ->
                    Main.judgment 15 13
                        |> Expect.equal True
            , test "player bust" <|
                \_ ->
                    Main.judgment 22 23
                        |> Expect.equal False
            , test "dealer bust" <|
                \_ ->
                    Main.judgment 13 22
                        |> Expect.equal True
            , test "dealer win" <|
                \_ ->
                    Main.judgment 15 17
                        |> Expect.equal False
            ]
        ]
