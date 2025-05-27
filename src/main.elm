port module Main exposing (main, Model, Difficulty(..), PuzzleResult(..), Msg(..), initialModel, update, stringToDifficulty, difficultyToString, calculatePotentialPayout)

import Browser
import Browser.Events
import Debug
import Html exposing (Html, Attribute, div, h3, p, button, span, text) -- text is already imported
import Html.Attributes exposing (class, style, id)
import Html.Events exposing (keyCode, onClick, on, onMouseEnter, onMouseLeave, stopPropagationOn, onInput)
import Json.Decode as Decode
import Task exposing (Task, succeed, perform)
import Platform.Sub


-- MODEL


type alias Model =
    { grid : List (List HexagonData)
    , hoveredHexId : Maybe String
    , focusedHexId : Maybe String
    , config : Config
    , isModalVisible : Bool
    , modalMessage : String
    -- Betting related fields
    , betAmount : Float
    , betAmountString : String -- For handling input field
    , difficulty : Difficulty
    , difficultyString : String -- For handling input field
    , walletAddress : Maybe String
    , puzzleResult : Maybe PuzzleResult
    , isBetModalVisible : Bool -- To control the visibility of the new bet modal
    }


type Difficulty
    = Easy
    | Medium
    | Hard


type PuzzleResult
    = Win Float
    | Loss


type alias HexagonData =
    { id : String
    , letter : Maybe Char
    , state : HexState
    }


type HexState
    = Empty
    | Correct
    | Present
    | Absent


type alias Config =
    { hexOuterWidth : Float
    , hexOuterHeight : Float
    , rowStaggerOffset : Float
    , rowVerticalOffset : Float
    , colorOutline : String
    , colorOutlineFocused : String
    , hexFillScale : Float
    , colorBgPage : String
    , colorFillDefault : String
    , colorTextDefault : String
    , colorFillCorrect : String
    , colorTextFilled : String
    , colorFillPresent : String
    , colorFillAbsent : String
    , letterFontSize : Float
    , hoverShadowColor : String
    , hoverShadowX : Int
    , hoverShadowY : Int
    , hoverShadowBlur : Int
    , hoverScale : Float
    , transitionSpeedMs : Int
    }


initialConfig : Config
initialConfig =
    let
        hexOuterWidth =
            70.0

        hexOuterHeight =
            hexOuterWidth * 1.15470053838
    in
    { hexOuterWidth = hexOuterWidth
    , hexOuterHeight = hexOuterHeight
    , rowStaggerOffset = hexOuterWidth * 0.5
    , rowVerticalOffset = hexOuterHeight * -0.25
    , colorOutline = "#f5f7f7"
    , colorOutlineFocused = "#add8e6"
    , hexFillScale = 0.94
    , colorBgPage = "#ffffff"
    , colorFillDefault = "#dcdcdc"
    , colorTextDefault = "#333333"
    , colorFillCorrect = "#6aaa64"
    , colorTextFilled = "#ffffff"
    , colorFillPresent = "#c9b458"
    , colorFillAbsent = "#787c7e"
    , letterFontSize = hexOuterWidth * 0.40
    , hoverShadowColor = "#c0c0c0"
    , hoverShadowX = 3
    , hoverShadowY = 5
    , hoverShadowBlur = 7
    , hoverScale = 1.05
    , transitionSpeedMs = 200
    }


initialGrid : List (List HexagonData)
initialGrid =
    let
        createHex r c letter state =
            { id = "hex-" ++ String.fromInt r ++ "-" ++ String.fromInt c, letter = letter, state = state }

        row0 =
            [ createHex 0 0 (Just 'W') Correct
            , createHex 0 1 (Just 'O') Present
            , createHex 0 2 (Just 'R') Absent
            , createHex 0 3 (Just 'D') Empty
            , createHex 0 4 (Just 'L') Correct
            ]

        emptyHex r c =
            createHex r c Nothing Empty

        row1 =
            List.indexedMap (\c _ -> emptyHex 1 c) (List.repeat 5 ())

        otherRows r =
            List.indexedMap (\c _ -> emptyHex r c) (List.repeat 5 ())
    in
    [ row0
    , row1
    , otherRows 2
    , otherRows 3
    , otherRows 4
    , otherRows 5
    ]


initialModel : Model
initialModel =
    { grid = initialGrid
    , hoveredHexId = Nothing
    , focusedHexId = Nothing
    , config = initialConfig
    , isModalVisible = False
    , modalMessage = ""
    -- Initialize betting fields
    , betAmount = 0.01 -- Default minimum bet
    , betAmountString = "0.01"
    , difficulty = Easy -- Default difficulty
    , difficultyString = "Easy"
    , walletAddress = Nothing
    , puzzleResult = Nothing
    , isBetModalVisible = False -- Bet modal is not visible initially
    }



-- UPDATE


type Msg
    = NoOp
    | MouseEnterHex String
    | MouseLeaveHex String
    | FocusHex String
    | KeyPressed Int
    | SubmitAttempt
    | CloseModal
    -- Betting related messages
    | ConnectWallet
    | ShowBetModal
    | HideBetModal
    | UpdateBetAmountString String
    | UpdateDifficultyString String
    | PlaceBet -- Takes amount and difficulty from the model
    | BetResultReceived Bool Float -- Bool for success, Float for payout amount
    | BetAgain


parseHexId : String -> Maybe ( Int, Int )
parseHexId idStr =
    case String.split "-" idStr of
        [ "hex", rStr, cStr ] ->
            case ( String.toInt rStr, String.toInt cStr ) of
                ( Just r, Just c ) ->
                    Just ( r, c )

                _ ->
                    Nothing

        _ ->
            Nothing


findNextHexIdInRow : Model -> String -> Maybe String
findNextHexIdInRow model currentHexId =
    case parseHexId currentHexId of
        Nothing ->
            Nothing

        Just ( r, c ) ->
            if r < 0 || r >= List.length model.grid then
                Nothing
            else
                case List.drop r model.grid |> List.head of
                    Nothing ->
                        Nothing

                    Just targetRow ->
                        let
                            numColsInRow =
                                List.length targetRow

                            nextC =
                                c + 1
                        in
                        if nextC < numColsInRow then
                            Just ("hex-" ++ String.fromInt r ++ "-" ++ String.fromInt nextC)
                        else
                            Nothing


difficultyToString : Difficulty -> String
difficultyToString difficulty =
    case difficulty of
        Easy ->
            "Easy"

        Medium ->
            "Medium"

        Hard ->
            "Hard"


stringToDifficulty : String -> Difficulty
stringToDifficulty str =
    case String.toLower str of
        "easy" ->
            Easy

        "medium" ->
            Medium

        "hard" ->
            Hard

        _ ->
            Easy -- Default to Easy if parsing fails


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseEnterHex hexId ->
            ( { model | hoveredHexId = Just hexId }, Cmd.none )

        MouseLeaveHex hexId ->
            let
                newHoveredId =
                    if model.hoveredHexId == Just hexId then
                        Nothing
                    else
                        model.hoveredHexId
            in
            ( { model | hoveredHexId = newHoveredId }, Cmd.none )

        FocusHex hexId ->
            ( { model | focusedHexId = Just hexId }, Cmd.none )

        KeyPressed code ->
            let
                enterCode = 13
                backspaceCode = 8
                isLetter = code >= 65 && code <= 90
                isNumber = code >= 48 && code <= 57
                isAlphanumeric = isLetter || isNumber
            in
            if code == enterCode then
                ( model, Task.perform (\_ -> SubmitAttempt) (Task.succeed ()) )

            else
                case model.focusedHexId of
                    Nothing ->
                        ( model, Cmd.none )

                    Just focusedId ->
                        if code == backspaceCode then
                            let
                                newGrid =
                                    updateHexInGrid focusedId (\hex -> { hex | letter = Nothing, state = Empty }) model.grid
                            in
                            ( { model | grid = newGrid }, Cmd.none )

                        else if isAlphanumeric then
                            let
                                charValue =
                                    Char.fromCode code
                                updatedGrid =
                                    updateHexInGrid focusedId (\hex -> { hex | letter = Just charValue, state = Empty }) model.grid
                                newModelFocusedHexId : Maybe String
                                newModelFocusedHexId =
                                    case findNextHexIdInRow model focusedId of
                                        Just nextActualHexId ->
                                            Just nextActualHexId
                                        Nothing ->
                                            Just focusedId
                            in
                            ( { model | grid = updatedGrid, focusedHexId = newModelFocusedHexId }, Cmd.none )

                        else
                            ( model, Cmd.none )

        SubmitAttempt ->
            let
                ( newModalUserMessage, logForConsole ) =
                    case model.focusedHexId of
                        Nothing ->
                            ( "No hex focused. Cannot determine row to submit."
                            , "SubmitAttempt: No hex focused. Cannot determine row to submit."
                            )

                        Just focusedId ->
                            case parseHexId focusedId of
                                Nothing ->
                                    ( "Error processing your submission. Invalid hex ID."
                                    , "SubmitAttempt: Error parsing focused hex ID (" ++ focusedId ++ ")."
                                    )

                                Just ( rowIndex, _ ) ->
                                    case List.drop rowIndex model.grid |> List.head of
                                        Nothing ->
                                            ( "Error processing your submission. Row not found."
                                            , "SubmitAttempt: Row " ++ String.fromInt rowIndex ++ " not found in grid."
                                            )

                                        Just rowData ->
                                            let
                                                lettersInRow =
                                                    rowData
                                                        |> List.map (\hex -> Maybe.withDefault '_' hex.letter)
                                                        |> String.fromList
                                            in
                                            ( "Submission for row " ++ String.fromInt rowIndex ++ ": [" ++ lettersInRow ++ "]"
                                            , "SubmitAttempt: Content of row " ++ String.fromInt rowIndex ++ " is: [" ++ lettersInRow ++ "]"
                                            )
                _ = Debug.log logForConsole
            in
            ( { model | isModalVisible = True, modalMessage = newModalUserMessage }, Cmd.none )

        CloseModal ->
            ( { model | isModalVisible = False, modalMessage = "", puzzleResult = Nothing }, Cmd.none )

        ConnectWallet ->
            ( model, connectWallet () )

        ShowBetModal ->
            ( { model | isBetModalVisible = True, puzzleResult = Nothing }, Cmd.none )

        HideBetModal ->
            ( { model | isBetModalVisible = False }, Cmd.none )

        UpdateBetAmountString amountStr ->
            let
                newAmount =
                    String.toFloat amountStr |> Maybe.withDefault model.betAmount
            in
            ( { model | betAmountString = amountStr, betAmount = newAmount }, Cmd.none )

        UpdateDifficultyString diffStr ->
            ( { model | difficultyString = diffStr, difficulty = stringToDifficulty diffStr }, Cmd.none )

        PlaceBet ->
            ( { model | isBetModalVisible = False }
            , placeBet { amount = model.betAmount, difficulty = difficultyToString model.difficulty }
            )

        BetResultReceived success payout ->
            let
                result =
                    if success then
                        Win payout

                    else
                        Loss

                message =
                    if success then
                        "You won " ++ String.fromFloat payout ++ " ETH!"

                    else
                        "Puzzle incorrect. You lost " ++ String.fromFloat model.betAmount ++ " ETH."
            in
            ( { model | puzzleResult = Just result, isModalVisible = True, modalMessage = message }, Cmd.none )

        BetAgain ->
            ( { initialModel | walletAddress = model.walletAddress, config = model.config } -- Keep wallet address and config
            , Cmd.none
            )


updateHexInGrid : String -> (HexagonData -> HexagonData) -> List (List HexagonData) -> List (List HexagonData)
updateHexInGrid targetId updateFn grid =
    List.map
        (\row ->
            List.map
                (\hex ->
                    if hex.id == targetId then
                        updateFn hex
                    else
                        hex
                )
                row
        )
        grid



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Platform.Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyPressed (Decode.field "keyCode" Decode.int))
        , transactionResult (\txData -> BetResultReceived (parseTxSuccess txData) (parseTxPayout txData))
        ]


parseTxSuccess : String -> Bool
parseTxSuccess txData =
    String.contains "\"success\": true" txData


parseTxPayout : String -> Float
parseTxPayout txData =
    if String.contains "\"success\": true" txData then
        let
            extract numStr = String.toFloat (String.trim numStr) |> Maybe.withDefault 0.0
        in
        case String.split ":" txData of
             _::val::_ -> case String.split "}" val of
                num::_ -> extract num
                _ -> 0.01
             _ -> 0.01
    else
        0.0


-- PORTS


port connectWallet : () -> Cmd msg
port placeBet : { amount : Float, difficulty : String } -> Cmd msg
port transactionResult : (String -> msg) -> Sub msg



-- VIEW


viewModal : Model -> Html Msg
viewModal model =
    if model.isModalVisible then
        let
            titleText =
                case model.puzzleResult of
                    Just (Win _) ->
                        "Bet Won!"

                    Just Loss ->
                        "Bet Lost"

                    Nothing ->
                        "Submission Attempt"

            messageText =
                model.modalMessage

            buttonText =
                case model.puzzleResult of
                    Just _ ->
                        "Bet Again"

                    Nothing ->
                        "OK"

            buttonMsg =
                case model.puzzleResult of
                    Just _ ->
                        BetAgain

                    Nothing ->
                        CloseModal
        in
        div -- Backdrop
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "background-color" "rgba(0, 0, 0, 0.6)"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "z-index" "1000"
            , onClick CloseModal
            ]
            [ div -- Modal content box
                [ style "background-color" "#fff"
                , style "padding" "25px 35px"
                , style "border-radius" "10px"
                , style "box-shadow" "0 5px 15px rgba(0,0,0,0.3)"
                , style "min-width" "320px"
                , style "max-width" "90%"
                , style "text-align" "center"
                , stopPropagationOn "click" (Decode.succeed (NoOp, True))
                ]
                [ h3 [ style "margin-top" "0", style "color" "#333" ] [ text titleText ]
                , p [ style "color" "#555", style "font-size" "16px", style "line-height" "1.5" ] [ text messageText ]
                , button
                    [ onClick buttonMsg
                    , style "padding" "12px 24px"
                    , style "margin-top" "25px"
                    , style "cursor" "pointer"
                    , style "background-color" model.config.colorFillCorrect
                    , style "color" model.config.colorTextFilled
                    , style "border" "none"
                    , style "border-radius" "5px"
                    , style "font-size" "16px"
                    , style "font-weight" "bold"
                    ]
                    [ text buttonText ]
                ]
            ]
    else
        Html.text ""


viewBetModal : Model -> Html Msg
viewBetModal model =
    if model.isBetModalVisible then
        div -- Backdrop for bet modal
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "background-color" "rgba(0, 0, 0, 0.7)"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "z-index" "1001"
            ]
            [ div -- Bet Modal content box
                [ style "background-color" "#fff"
                , style "padding" "30px 40px"
                , style "border-radius" "12px"
                , style "box-shadow" "0 8px 20px rgba(0,0,0,0.35)"
                , style "min-width" "350px"
                , style "max-width" "90%"
                , style "text-align" "left"
                , stopPropagationOn "click" (Decode.succeed (NoOp, True))
                ]
                [ h3 [ style "margin-top" "0", style "margin-bottom" "25px", style "color" "#333", style "text-align" "center" ] [ text "Place Your Bet" ]
                , div [ style "margin-bottom" "20px" ]
                    [ Html.label [ Html.Attributes.for "betAmountInput", style "display" "block", style "margin-bottom" "8px", style "font-weight" "bold", style "color" "#555" ] [ text "Bet Amount (ETH):" ]
                    , Html.input
                        [ id "betAmountInput"
                        , Html.Attributes.type_ "number"
                        , Html.Attributes.min "0.01"
                        , Html.Attributes.max "1"
                        , Html.Attributes.step "0.01"
                        , Html.Attributes.value model.betAmountString
                        , onInput UpdateBetAmountString
                        , style "width" "calc(100% - 22px)"
                        , style "padding" "10px"
                        , style "border" "1px solid #ccc"
                        , style "border-radius" "5px"
                        , style "font-size" "16px"
                        ]
                        []
                    ]
                , div [ style "margin-bottom" "25px" ]
                    [ Html.label [ Html.Attributes.for "difficultySelect", style "display" "block", style "margin-bottom" "8px", style "font-weight" "bold", style "color" "#555" ] [ text "Difficulty:" ]
                    , Html.select
                        [ id "difficultySelect"
                        , onInput UpdateDifficultyString
                        , Html.Attributes.value (difficultyToString model.difficulty)
                        , style "width" "100%"
                        , style "padding" "10px"
                        , style "border" "1px solid #ccc"
                        , style "border-radius" "5px"
                        , style "font-size" "16px"
                        , style "background-color" "white"
                        ]
                        [ Html.option [ Html.Attributes.value "Easy" ] [ text "Easy (1.5x)" ]
                        , Html.option [ Html.Attributes.value "Medium" ] [ text "Medium (2x)" ]
                        , Html.option [ Html.Attributes.value "Hard" ] [ text "Hard (3x)" ]
                        ]
                    ]
                , div [ style "margin-bottom" "25px", style "font-size" "15px", style "color" "#444" ]
                    [ text "Potential Payout: "
                    , Html.b [] [ text (calculatePotentialPayout model ++ " ETH (after 5% fee)") ]
                    ]
                , div [ style "display" "flex", style "justify-content" "space-between" ]
                    [ button
                        [ onClick HideBetModal
                        , style "padding" "12px 24px"
                        , style "cursor" "pointer"
                        , style "background-color" "#aaa"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "5px"
                        , style "font-size" "16px"
                        ]
                        [ text "Cancel" ]
                    , button
                        [ onClick PlaceBet
                        , style "padding" "12px 24px"
                        , style "cursor" "pointer"
                        , style "background-color" model.config.colorFillCorrect
                        , style "color" model.config.colorTextFilled
                        , style "border" "none"
                        , style "border-radius" "5px"
                        , style "font-size" "16px"
                        , style "font-weight" "bold"
                        ]
                        [ text "Confirm Bet & Start" ]
                    ]
                ]
            ]
    else
        Html.text ""


calculatePotentialPayout : Model -> String
calculatePotentialPayout model =
    let
        multiplier =
            case model.difficulty of
                Easy ->
                    1.5

                Medium ->
                    2.0

                Hard ->
                    3.0

        rawPayout =
            model.betAmount * multiplier

        fee =
            rawPayout * 0.05

        finalPayout =
            rawPayout - fee
    in
    String.fromFloat finalPayout


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "Arial, sans-serif"
        , style "background-color" model.config.colorBgPage
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "padding" "20px"
        , style "box-sizing" "border-box"
        , style "min-height" "100vh"
        ]
        [ viewWalletControls model
        , viewHoneycombGrid model
        , viewRules
        , viewModal model
        , viewBetModal model
        ]


viewWalletControls : Model -> Html Msg
viewWalletControls model =
    div
        [ style "width" "100%"
        , style "max-width" "600px"
        , style "display" "flex"
        , style "justify-content" (if model.walletAddress == Nothing then "flex-end" else "space-between")
        , style "align-items" "center"
        , style "padding" "10px 0"
        , style "margin-bottom" "20px"
        , style "box-sizing" "border-box"
        ]
        (let
            connectButton =
                if model.walletAddress == Nothing then
                    button
                        [ onClick ConnectWallet
                        , style "padding" "10px 18px"
                        , style "cursor" "pointer"
                        , style "background-color" "#007bff"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "5px"
                        , style "font-size" "15px"
                        ]
                        [ text "Connect Wallet" ]

                else
                    span [ style "font-size" "14px", style "color" "#333" ] [ text ("Wallet: " ++ (Maybe.withDefault "N/A" model.walletAddress |> truncateWalletAddress)) ]

            placeBetButton =
                if model.walletAddress /= Nothing && not model.isBetModalVisible then
                    [ button
                        [ onClick ShowBetModal
                        , style "padding" "10px 18px"
                        , style "cursor" "pointer"
                        , style "background-color" model.config.colorFillCorrect
                        , style "color" model.config.colorTextFilled
                        , style "border" "none"
                        , style "border-radius" "5px"
                        , style "font-size" "15px"
                        , style "font-weight" "bold"
                        ]
                        [ text "Place Bet" ]
                    ]

                else
                    []
         in
         [ connectButton ] ++ placeBetButton
        )

truncateWalletAddress : String -> String
truncateWalletAddress addr =
    if String.length addr > 10 then
        String.slice 0 6 addr ++ "..." ++ String.slice (String.length addr - 4) (String.length addr) addr

    else
        addr


viewRules : Html Msg
viewRules =
    div
        [ style "max-width" "600px"
        , style "margin-top" "30px"
        , style "padding" "15px"
        , style "border" "1px solid #eee"
        , style "border-radius" "8px"
        , style "background-color" "#f9f9f9"
        ]
        [ h3 [ style "margin-top" "0", style "color" "#333", style "font-size" "18px" ] [ text "Game Rules & Betting" ]
        , p [ style "font-size" "14px", style "line-height" "1.6", style "color" "#555" ]
            [ text "Place a bet on your ability to solve the puzzle! Connect your Ethereum wallet (Polygon network) to start." ]
        , Html.ul [ style "font-size" "14px", style "line-height" "1.6", style "color" "#555", style "padding-left" "20px" ]
            [ Html.li [] [ text "Select your bet amount (0.01 - 1 ETH) and difficulty." ]
            , Html.li [] [ text "Difficulty Multipliers: Easy (1.5x), Medium (2x), Hard (3x)." ]
            , Html.li [] [ text "A 5% platform fee applies to winnings." ]
            , Html.li [] [ text "Solve the puzzle within 6 attempts to win." ]
            , Html.li [] [ text "Puzzle words are selected provably fair using Chainlink VRF (details on-chain)." ]
            ]
        ]


viewHoneycombGrid : Model -> Html Msg
viewHoneycombGrid model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        (List.indexedMap (viewHoneycombRow model) model.grid)


viewHoneycombRow : Model -> Int -> List HexagonData -> Html Msg
viewHoneycombRow model rowIndex rowData =
    let
        rowStyle =
            [ style "display" "flex"
            ]

        marginTopStyle =
            if rowIndex > 0 then
                style "margin-top" (String.fromFloat model.config.rowVerticalOffset ++ "px")

            else
                style "margin-top" "0"

        marginLeftStyle =
            if modBy 2 rowIndex == 1 then
                style "margin-left" (String.fromFloat model.config.rowStaggerOffset ++ "px")

            else
                style "margin-left" "0"
    in
    div
        (rowStyle ++ [ marginTopStyle, marginLeftStyle ])
        (List.map (viewHexagon model) rowData)


viewHexagon : Model -> HexagonData -> Html Msg
viewHexagon model hexData =
    let
        cfg =
            model.config

        isHovered =
            model.hoveredHexId == Just hexData.id

        isFocused =
            model.focusedHexId == Just hexData.id

        ( fillCol, textCol ) =
            case hexData.state of
                Empty ->
                    ( cfg.colorFillDefault, cfg.colorTextDefault )

                Correct ->
                    ( cfg.colorFillCorrect, cfg.colorTextFilled )

                Present ->
                    ( cfg.colorFillPresent, cfg.colorTextFilled )

                Absent ->
                    ( cfg.colorFillAbsent, cfg.colorTextFilled )

        outlineColor =
            if isFocused then
                cfg.colorOutlineFocused

            else
                cfg.colorOutline

        outerHexStyles =
            [ style "width" (String.fromFloat cfg.hexOuterWidth ++ "px")
            , style "height" (String.fromFloat cfg.hexOuterHeight ++ "px")
            , style "background-color" outlineColor
            , style "clip-path" "polygon(50% 0%, 100% 25%, 100% 75%, 50% 100%, 0% 75%, 0% 25%)"
            , style "position" "relative"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "box-sizing" "border-box"
            , style "cursor" "pointer"
            , style "transition" ("filter " ++ String.fromInt cfg.transitionSpeedMs ++ "ms ease-out, transform " ++ String.fromInt cfg.transitionSpeedMs ++ "ms ease-out, background-color " ++ String.fromInt cfg.transitionSpeedMs ++ "ms ease-out, z-index 0s " ++ (if isHovered then "" else String.fromInt cfg.transitionSpeedMs ++ "ms"))
            , style "z-index" (if isHovered then "10" else "1")
            ]

        hoverSpecificStyles =
            if isHovered && not isFocused then
                [ style "filter" ("drop-shadow(" ++ String.fromInt cfg.hoverShadowX ++ "px " ++ String.fromInt cfg.hoverShadowY ++ "px " ++ String.fromInt cfg.hoverShadowBlur ++ "px " ++ cfg.hoverShadowColor ++ ")")
                , style "transform" ("scale(" ++ String.fromFloat cfg.hoverScale ++ ")")
                ]

            else if isFocused then
                 [ style "transform" ("scale(" ++ String.fromFloat (cfg.hoverScale * 0.98) ++ ")") ]


            else
                []

        innerFillStyles =
            [ style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "background-color" fillCol
            , style "clip-path" "inherit"
            , style "transform" ("scale(" ++ String.fromFloat cfg.hexFillScale ++ ")")
            , style "z-index" "0"
            , style "box-sizing" "border-box"
            ]

        letterStyles =
            [ style "font-size" (String.fromFloat cfg.letterFontSize ++ "px")
            , style "font-weight" "bold"
            , style "color" textCol
            , style "text-transform" "uppercase"
            , style "position" "relative"
            , style "z-index" "1"
            , style "user-select" "none"
            , style "line-height" "1"
            ]

        letterContent =
            case hexData.letter of
                Just l ->
                    String.fromChar l

                Nothing ->
                    ""
    in
    div
        (outerHexStyles
            ++ hoverSpecificStyles
            ++ [ onMouseEnter (MouseEnterHex hexData.id)
               , onMouseLeave (MouseLeaveHex hexData.id)
               , onClick (FocusHex hexData.id)
               ]
        )
        [ div innerFillStyles []
        , span letterStyles [ text letterContent ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , view = \model -> { title = "Interactive Honeycomb Wordle", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }
