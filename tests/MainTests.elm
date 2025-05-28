module MainTests exposing (..)

import Expect
import Test exposing (..)
import Main exposing (Msg(..), Difficulty(..), PuzzleResult(..), initialModel, update, stringToDifficulty, difficultyToString, calculatePotentialPayout) -- Add other necessary imports


suite : Test
suite =
    describe "Interactive Honeycomb Wordle Tests"
        [ describe "Initial Model State"
            [ test "Initial betAmount is 0.01" <|
                \_ ->
                    Expect.within (Expect.Absolute 0.0001) initialModel.betAmount 0.01
            , test "Initial betAmountString is \"0.01\"" <|
                \_ ->
                    Expect.equal initialModel.betAmountString "0.01"
            , test "Initial difficulty is Easy" <|
                \_ ->
                    Expect.equal initialModel.difficulty Easy
            , test "Initial difficultyString is \"Easy\"" <|
                \_ ->
                    Expect.equal initialModel.difficultyString "Easy"
            , test "Initial walletAddress is Nothing" <|
                \_ ->
                    Expect.equal initialModel.walletAddress Nothing
            , test "Initial puzzleResult is Nothing" <|
                \_ ->
                    Expect.equal initialModel.puzzleResult Nothing
            , test "Initial isBetModalVisible is False" <|
                \_ ->
                    Expect.equal initialModel.isBetModalVisible False
            ]
        , describe "Difficulty Conversion Functions"
            [ test "stringToDifficulty \"Easy\" -> Easy" <|
                \_ ->
                    Expect.equal (stringToDifficulty "Easy") Easy
            , test "stringToDifficulty \"Medium\" -> Medium" <|
                \_ ->
                    Expect.equal (stringToDifficulty "Medium") Medium
            , test "stringToDifficulty \"Hard\" -> Hard" <|
                \_ ->
                    Expect.equal (stringToDifficulty "Hard") Hard
            , test "stringToDifficulty \"invalid\" -> Easy (default)" <|
                \_ ->
                    Expect.equal (stringToDifficulty "invalid") Easy
            , test "difficultyToString Easy -> \"Easy\"" <|
                \_ ->
                    Expect.equal (difficultyToString Easy) "Easy"
            , test "difficultyToString Medium -> \"Medium\"" <|
                \_ ->
                    Expect.equal (difficultyToString Medium) "Medium"
            , test "difficultyToString Hard -> \"Hard\"" <|
                \_ ->
                    Expect.equal (difficultyToString Hard) "Hard"
            ]
        , describe "Update Function - Betting Logic"
            [ test "UpdateBetAmountString updates betAmount and betAmountString" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (UpdateBetAmountString "0.5") initialModel
                    in
                    Expect.all
                        [ \m -> Expect.equal m.betAmountString "0.5"
                        , \m -> Expect.within (Expect.Absolute 0.0001) m.betAmount 0.5
                        ] newModel
            , test "UpdateBetAmountString handles invalid float string" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (UpdateBetAmountString "abc") initialModel
                    in
                    Expect.all
                        [ \m -> Expect.equal m.betAmountString "abc"
                        , \m -> Expect.within (Expect.Absolute 0.0001) m.betAmount initialModel.betAmount
                        ] newModel
            , test "UpdateDifficultyString updates difficulty and difficultyString" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            update (UpdateDifficultyString "Hard") initialModel
                    in
                    Expect.all
                        [ \m -> Expect.equal m.difficultyString "Hard"
                        , \m -> Expect.equal m.difficulty Hard
                        ] newModel
            , test "BetAgain resets betting fields but keeps walletAddress and config" <|
                \_ ->
                    let
                        modifiedModel =
                            { initialModel
                                | betAmount = 0.5
                                , difficulty = Medium
                                , puzzleResult = Just (Win 1.0)
                                , walletAddress = Just "0x123"
                            }
                        ( newModel, _ ) =
                            update BetAgain modifiedModel
                    in
                    Expect.all
                        [ \m -> Expect.within (Expect.Absolute 0.0001) m.betAmount initialModel.betAmount
                        , \m -> Expect.equal m.difficulty initialModel.difficulty
                        , \m -> Expect.equal m.puzzleResult Nothing
                        , \m -> Expect.equal m.walletAddress (Just "0x123")
                        , \m -> Expect.equal m.isBetModalVisible initialModel.isBetModalVisible
                        , \m -> Expect.equal m.config initialModel.config
                        , \m -> Expect.equal m.grid initialModel.grid
                        ] newModel
            , test "BetResultReceived (Win) updates puzzleResult and modal" <|
                \_ ->
                    let
                        betAmountForTest = 0.1
                        payout = 0.15
                        modelWithBet = { initialModel | betAmount = betAmountForTest }
                        ( newModel, _ ) =
                            update (BetResultReceived True payout) modelWithBet
                    in
                    Expect.all
                        [ \m -> Expect.equal m.puzzleResult (Just (Win payout))
                        , \m -> Expect.equal m.isModalVisible True
                        , \m -> Expect.equal (String.contains "You won 0.15 ETH!" m.modalMessage) True
                        ] newModel
            , test "BetResultReceived (Loss) updates puzzleResult and modal" <|
                \_ ->
                    let
                         betAmountForTest = 0.25
                         modelWithBet = { initialModel | betAmount = betAmountForTest }
                         ( newModel, _ ) =
                            update (BetResultReceived False 0.0) modelWithBet
                    in
                    Expect.all
                        [ \m -> Expect.equal m.puzzleResult (Just Loss)
                        , \m -> Expect.equal m.isModalVisible True
                        , \m -> Expect.equal (String.contains ("You lost " ++ String.fromFloat betAmountForTest ++ " ETH.") m.modalMessage) True
                        ] newModel
            ]
        , describe "CalculatePotentialPayout"
            [ test "Easy difficulty (1.5x multiplier)" <|
                \_ ->
                    let
                        model = { initialModel | betAmount = 0.1, difficulty = Easy }
                        expectedPayout = (0.1 * 1.5) * 0.95 -- 5% fee
                    in
                    Expect.equal (calculatePotentialPayout model) (String.fromFloat expectedPayout)
            , test "Medium difficulty (2x multiplier)" <|
                \_ ->
                    let
                        model = { initialModel | betAmount = 0.1, difficulty = Medium }
                        expectedPayout = (0.1 * 2.0) * 0.95 -- 5% fee
                    in
                    Expect.equal (calculatePotentialPayout model) (String.fromFloat expectedPayout)
            , test "Hard difficulty (3x multiplier)" <|
                \_ ->
                    let
                        model = { initialModel | betAmount = 0.1, difficulty = Hard }
                        expectedPayout = (0.1 * 3.0) * 0.95 -- 5% fee
                    in
                    Expect.equal (calculatePotentialPayout model) (String.fromFloat expectedPayout)
             , test "Zero bet amount" <|
                \_ ->
                    let
                        model = { initialModel | betAmount = 0.0, difficulty = Medium }
                        expectedPayout = (0.0 * 2.0) * 0.95 -- 5% fee
                    in
                    Expect.equal (calculatePotentialPayout model) (String.fromFloat expectedPayout)
            ]
        ] 