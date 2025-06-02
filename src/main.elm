port module Main exposing (main, Model, PuzzleResult(..), Msg(..), initialModel, update, calculatePotentialPayout)

import Browser
import Browser.Events
import Dict
import Html exposing (Html, div, h3, p, button, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, stopPropagationOn)
import Json.Decode as Decode exposing (Decoder)
import Task
import Platform.Sub
import Random
import Array
import Process
import Http


-- Simple fallback word list when external files fail to load
fallbackTargetWords : List String
fallbackTargetWords =
    [ "speed", "house", "world", "music", "ocean", "beach", "plant", "water", "light", "sound" ]


-- VALID GUESSES DATABASE TYPES


type alias ValidGuessesDB =
    { version : String
    , metadata : ValidGuessesMetadata
    , validGuesses : List String
    , targetWords : List String
    , features : SolutionsFeatures
    }


type alias ValidGuessesMetadata =
    { totalValidGuesses : Int
    , totalTargetWords : Int
    , lastUpdated : String
    , source : String
    , format : String
    , description : String
    }


type alias SolutionsFeatures =
    { hasVowels : Bool
    , allowsRepeatedLetters : Bool
    , caseSensitive : Bool
    }


-- TARGET WORDS DATABASE TYPES


type alias TargetWordsDB =
    { version : String
    , metadata : TargetWordsMetadata
    , targetWords : List String
    , features : SolutionsFeatures
    }


type alias TargetWordsMetadata =
    { totalTargetWords : Int
    , lastUpdated : String
    , source : String
    , format : String
    , description : String
    }


-- VALID GUESSES DATABASE DECODERS




-- Simple decoder for validguess.json file (only contains validGuesses list)
simpleValidGuessesDecoder : Decoder (List String)
simpleValidGuessesDecoder =
    Decode.field "validGuesses" (Decode.list Decode.string)


-- TARGET WORDS DATABASE DECODERS



-- VALID GUESSES DATABASE FUNCTIONS


getRandomWordGenerator : List String -> Random.Generator (Maybe String)
getRandomWordGenerator targetWords =
    let
        wordArray =
            Array.fromList targetWords
    in
    if Array.isEmpty wordArray then
        Random.constant Nothing
    else
        Random.map (\index -> Array.get index wordArray) (Random.int 0 (Array.length wordArray - 1))



-- For validation, check if word exists in valid guesses list OR target words list
-- If not found, fall back to basic English word validation
validateWord : List String -> List String -> String -> Bool
validateWord validGuesses targetWords word =
    let
        cleanWord = String.toLower (String.trim word)
        isValidLength = String.length cleanWord == 5
        isAllLetters = String.all Char.isAlpha cleanWord
        isInValidGuessesList = List.member cleanWord validGuesses

        -- Check if word is in target words (answerlist.json)
        isInTargetWordsList = List.member cleanWord (List.map String.toLower targetWords)

        -- Basic validation for reasonable English words
        passesBasicValidation = isValidLength && isAllLetters && not (isRepeatingPattern cleanWord) && not (isObviousNonsense cleanWord)
    in
    if isInValidGuessesList || isInTargetWordsList then
        -- Word is in our curated databases - definitely valid
        True
    else if List.isEmpty validGuesses then
        -- Fallback to basic validation if valid guesses not loaded
        passesBasicValidation
    else
        -- Word not in curated list, but check if it passes basic validation
        -- This allows reasonable English words that might not be in our database
        passesBasicValidation


-- Helper function to detect obvious non-words like "AAAAA", "BBBBB", etc.
isRepeatingPattern : String -> Bool
isRepeatingPattern word =
    let
        chars = String.toList word
        firstChar = List.head chars
    in
    case firstChar of
        Nothing -> False
        Just char -> List.all (\c -> c == char) chars


-- Helper function to detect obvious nonsense words
isObviousNonsense : String -> Bool
isObviousNonsense word =
    let
        cleanWord = String.toLower word
        chars = String.toList cleanWord
        vowels = ['a', 'e', 'i', 'o', 'u', 'y']  -- Include 'y' as it can act as a vowel
        consonants = ['b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z']

        -- Count vowels and consonants
        vowelCount = List.length (List.filter (\c -> List.member c vowels) chars)
        consonantCount = List.length (List.filter (\c -> List.member c consonants) chars)

        -- Patterns that suggest nonsense
        hasNoVowels = vowelCount == 0
        allVowels = consonantCount == 0
        tooManyConsecutiveConsonants = hasConsecutiveConsonants cleanWord 4
        tooManyConsecutiveVowels = hasConsecutiveVowels cleanWord 3
    in
    hasNoVowels || allVowels || tooManyConsecutiveConsonants || tooManyConsecutiveVowels


-- Helper to check for too many consecutive consonants
hasConsecutiveConsonants : String -> Int -> Bool
hasConsecutiveConsonants word maxCount =
    let
        chars = String.toList word
        vowels = ['a', 'e', 'i', 'o', 'u', 'y']  -- Include 'y' as it can act as a vowel

        checkConsecutive : List Char -> Int -> Int -> Bool
        checkConsecutive remainingChars currentCount maxAllowed =
            case remainingChars of
                [] -> currentCount >= maxAllowed
                char :: rest ->
                    if List.member char vowels then
                        checkConsecutive rest 0 maxAllowed
                    else
                        let newCount = currentCount + 1
                        in
                        if newCount >= maxAllowed then
                            True
                        else
                            checkConsecutive rest newCount maxAllowed
    in
    checkConsecutive chars 0 maxCount


-- Helper to check for too many consecutive vowels
hasConsecutiveVowels : String -> Int -> Bool
hasConsecutiveVowels word maxCount =
    let
        chars = String.toList word
        vowels = ['a', 'e', 'i', 'o', 'u', 'y']  -- Include 'y' as it can act as a vowel

        checkConsecutive : List Char -> Int -> Int -> Bool
        checkConsecutive remainingChars currentCount maxAllowed =
            case remainingChars of
                [] -> currentCount >= maxAllowed
                char :: rest ->
                    if List.member char vowels then
                        let newCount = currentCount + 1
                        in
                        if newCount >= maxAllowed then
                            True
                        else
                            checkConsecutive rest newCount maxAllowed
                    else
                        checkConsecutive rest 0 maxAllowed
    in
    checkConsecutive chars 0 maxCount


-- MODEL


type alias Model =
    { grid : List (List HexagonData)
    , hoveredHexId : Maybe String
    , focusedHexId : Maybe String
    , config : Config
    , isModalVisible : Bool
    , modalMessage : String
    , betAmount : Float
    , betAmountString : String
    , walletAddress : Maybe String
    , puzzleResult : Maybe PuzzleResult
    , isBetModalVisible : Bool
    , validGuessesDB : Maybe ValidGuessesDB
    , targetWordsDB : Maybe TargetWordsDB
    , currentWord : Maybe String
    , loadingError : Maybe String
    , gameMessage : Maybe String -- New field for game messages
    , gameMessageType : String -- New field for message type (error, success, info)
    , submittedRows : List Int -- Track which rows have been submitted and are locked
    , currentActiveRow : Int -- Track which row is currently active for submissions
    , isHintModalVisible : Bool -- Track if hint modal is visible
    , isGameOverModalVisible : Bool -- Track if game over modal is visible
    , isLettersModalVisible : Bool -- Track if letters modal is visible
    }


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
            List.indexedMap (\c _ -> emptyHex 0 c) (List.repeat 5 ())

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
    , focusedHexId = Just "hex-0-0"
    , config = initialConfig
    , isModalVisible = False
    , modalMessage = ""
    , betAmount = 0.01
    , betAmountString = "0.01"
    , walletAddress = Nothing
    , puzzleResult = Nothing
    , isBetModalVisible = False
    , validGuessesDB = Nothing -- Will be loaded from external file
    , targetWordsDB = Nothing -- Will be loaded from external file
    , currentWord = Nothing -- Will be set by initial word selection
    , loadingError = Nothing
    , gameMessage = Nothing
    , gameMessageType = ""
    , submittedRows = []
    , currentActiveRow = 0
    , isHintModalVisible = False
    , isGameOverModalVisible = False
    , isLettersModalVisible = False
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
    | ConnectWallet
    | ShowBetModal
    | HideBetModal
    | UpdateBetAmountString String
    | PlaceBet
    | BetResultReceived Bool Float
    | BetAgain
    | RandomWordGenerated (Maybe String)
    | StartNewGame
    | RefreshPage
    | ClearGameMessage
    | LoadValidGuesses
    | SimpleValidGuessesLoaded (Result Http.Error (List String))
    | ShowSuccessModal
    | ShowGameOverModal
    | ShowHintModal
    | CloseHintModal
    | ShowLettersModal
    | CloseLettersModal


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


findPreviousHexIdInRow : Model -> String -> Maybe String
findPreviousHexIdInRow model currentHexId =
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
                            prevC =
                                c - 1
                        in
                        if prevC >= 0 then
                            Just ("hex-" ++ String.fromInt r ++ "-" ++ String.fromInt prevC)
                        else
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


isRowSubmitted : Model -> Int -> Bool
isRowSubmitted model rowIndex =
    List.member rowIndex model.submittedRows


-- UPDATE


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
            case parseHexId hexId of
                Nothing ->
                    ( model, Cmd.none )

                Just ( rowIndex, _ ) ->
                    if rowIndex /= model.currentActiveRow then
                        -- Only allow focusing on the current active row
                        ( model, Cmd.none )
                    else
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
                        case parseHexId focusedId of
                            Nothing ->
                                ( model, Cmd.none )

                            Just ( r, c ) ->
                                if r /= model.currentActiveRow then
                                    -- Only allow editing on the current active row
                                    ( model, Cmd.none )
                                else if code == backspaceCode then
                                    -- Handle backspace for unlocked rows
                                    case List.drop r model.grid |> List.head of
                                        Nothing ->
                                            ( model, Cmd.none )

                                        Just targetRow ->
                                            case List.drop c targetRow |> List.head of
                                                Nothing ->
                                                    ( model, Cmd.none )

                                                Just currentHex ->
                                                    if currentHex.letter /= Nothing then
                                                        -- Current hex has content, clear it and stay here
                                                        let
                                                            newGrid =
                                                                updateHexInGrid focusedId (\hex -> { hex | letter = Nothing, state = Empty }) model.grid
                                                        in
                                                        ( { model | grid = newGrid }, Cmd.none )
                                                    else
                                                        -- Current hex is empty, go to previous hex and clear it
                                                        case findPreviousHexIdInRow model focusedId of
                                                            Nothing ->
                                                                ( model, Cmd.none )

                                                            Just prevHexId ->
                                                                let
                                                                    newGrid =
                                                                        updateHexInGrid prevHexId (\hex -> { hex | letter = Nothing, state = Empty }) model.grid
                                                                in
                                                                ( { model | grid = newGrid, focusedHexId = Just prevHexId }, Cmd.none )
                                else if isAlphanumeric then
                                    -- Handle letter input for unlocked rows
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
                result =
                    case model.focusedHexId of
                        Nothing ->
                            { gameMessage = Just "No hex focused. Cannot determine row to submit."
                            , gameMessageType = "error"
                            , logMessage = "SubmitAttempt: No hex focused. Cannot determine row to submit."
                            , grid = model.grid
                            , submittedRows = model.submittedRows
                            , nextFocus = Nothing
                            , currentActiveRow = model.currentActiveRow
                            }

                        Just focusedId ->
                            case parseHexId focusedId of
                                Nothing ->
                                    { gameMessage = Just "Error processing your submission. Invalid hex ID."
                                    , gameMessageType = "error"
                                    , logMessage = "SubmitAttempt: Error parsing focused hex ID (" ++ focusedId ++ ")."
                                    , grid = model.grid
                                    , submittedRows = model.submittedRows
                                    , nextFocus = Nothing
                                    , currentActiveRow = model.currentActiveRow
                                    }

                                Just ( rowIndex, _ ) ->
                                    if rowIndex /= model.currentActiveRow then
                                        { gameMessage = Just ("You can only submit row " ++ String.fromInt (model.currentActiveRow + 1) ++ ". Complete rows in order.")
                                        , gameMessageType = "error"
                                        , logMessage = "SubmitAttempt: Attempted to submit non-active row " ++ String.fromInt rowIndex ++ ", current active row is " ++ String.fromInt model.currentActiveRow
                                        , grid = model.grid
                                        , submittedRows = model.submittedRows
                                        , nextFocus = Nothing
                                        , currentActiveRow = model.currentActiveRow
                                        }
                                    else if isRowSubmitted model rowIndex then
                                        { gameMessage = Just "This row has already been submitted."
                                        , gameMessageType = "error"
                                        , logMessage = "SubmitAttempt: Attempted to submit already locked row " ++ String.fromInt rowIndex
                                        , grid = model.grid
                                        , submittedRows = model.submittedRows
                                        , nextFocus = Nothing
                                        , currentActiveRow = model.currentActiveRow
                                        }
                                    else
                                    case List.drop rowIndex model.grid |> List.head of
                                        Nothing ->
                                                { gameMessage = Just "Error processing your submission. Row not found."
                                                , gameMessageType = "error"
                                                , logMessage = "SubmitAttempt: Row " ++ String.fromInt rowIndex ++ " not found in grid."
                                                , grid = model.grid
                                                , submittedRows = model.submittedRows
                                                , nextFocus = Nothing
                                                , currentActiveRow = model.currentActiveRow
                                                }

                                        Just rowData ->
                                                case getWordFromRow rowData of
                                                    Nothing ->
                                                        { gameMessage = Just "Please complete the word before submitting."
                                                        , gameMessageType = "error"
                                                        , logMessage = "SubmitAttempt: Incomplete word in row " ++ String.fromInt rowIndex
                                                        , grid = model.grid
                                                        , submittedRows = model.submittedRows
                                                        , nextFocus = Nothing
                                                        , currentActiveRow = model.currentActiveRow
                                                        }

                                                    Just submittedWord ->
                                                        let
                                                            targetWord =
                                                                case model.currentWord of
                                                                    Just word -> word
                                                                    Nothing -> "speed" -- Default fallback word

                                                            -- Check the word and get hex states
                                                            hexStates = checkWordAgainstTarget submittedWord targetWord
                                                            updatedGrid = updateRowWithStates rowIndex hexStates model.grid

                                                            isCorrect = String.toLower submittedWord == String.toLower targetWord
                                                            isValidWord =
                                                                case (model.validGuessesDB, model.targetWordsDB) of
                                                                    (Just validDB, Just targetDB) ->
                                                                        validateWord validDB.validGuesses targetDB.targetWords submittedWord
                                                                    (Just validDB, Nothing) ->
                                                                        validateWord validDB.validGuesses fallbackTargetWords submittedWord
                                                                    (Nothing, Just targetDB) ->
                                                                        validateWord [] targetDB.targetWords submittedWord
                                                                    (Nothing, Nothing) ->
                                                                        validateWord [] fallbackTargetWords submittedWord
                                                        in
                                                        if isCorrect then
                                                            { gameMessage = Just "🎉 Correct! You solved the puzzle!"
                                                            , gameMessageType = "success"
                                                            , logMessage = "SubmitAttempt: Correct word submitted"
                                                            , grid = updatedGrid
                                                            , submittedRows = rowIndex :: model.submittedRows
                                                            , nextFocus = Nothing -- Game is won, no next focus needed
                                                            , currentActiveRow = model.currentActiveRow -- Game is won, don't advance
                                                            }
                                                        else if isValidWord then
                                                            let
                                                                nextRow = model.currentActiveRow + 1
                                                                maxRows = List.length model.grid
                                                                hasNextRow = nextRow < maxRows
                                                            in
                                                            if hasNextRow then
                                                                { gameMessage = Just "Valid word, but not correct. Try again!"
                                                                , gameMessageType = "info"
                                                                , logMessage = "SubmitAttempt: Valid but incorrect word submitted"
                                                                , grid = updatedGrid
                                                                , submittedRows = rowIndex :: model.submittedRows
                                                                , nextFocus = Just ("hex-" ++ String.fromInt nextRow ++ "-0")
                                                                , currentActiveRow = nextRow
                                                                }
                                                            else
                                                                -- No more rows - show game over modal
                                                                { gameMessage = Nothing -- No message needed, modal will show
                                                                , gameMessageType = "gameover"
                                                                , logMessage = "SubmitAttempt: Game over - no more rows available"
                                                                , grid = updatedGrid
                                                                , submittedRows = rowIndex :: model.submittedRows
                                                                , nextFocus = Nothing
                                                                , currentActiveRow = model.currentActiveRow
                                                                }
                                                        else
                                                            let
                                                                -- Clear the invalid word from the current row
                                                                clearedGrid = clearRow rowIndex model.grid
                                                                -- Set focus to first hex of current active row
                                                                firstHexId = "hex-" ++ String.fromInt model.currentActiveRow ++ "-0"
                                                            in
                                                            { gameMessage = Just "Invalid word. Try again!"
                                                            , gameMessageType = "error"
                                                            , logMessage = "SubmitAttempt: Invalid word submitted - row cleared"
                                                            , grid = clearedGrid -- Clear the invalid word
                                                            , submittedRows = model.submittedRows -- Don't lock row for invalid words
                                                            , nextFocus = Just firstHexId -- Reset focus to first hex of current row
                                                            , currentActiveRow = model.currentActiveRow -- Don't advance for invalid words
                                                            }

                clearMessageCmd =
                    if result.gameMessage /= Nothing then
                        if result.gameMessageType == "success" then
                            -- Show success modal instead of auto-clearing message
                            Task.perform (\_ -> ShowSuccessModal) (Process.sleep 1000)
                        else
                            Task.perform (\_ -> ClearGameMessage) (Process.sleep 3000)
                    else if result.gameMessageType == "gameover" then
                        -- Show game over modal for final row incorrect word
                        Task.perform (\_ -> ShowGameOverModal) (Process.sleep 1000)
                    else
                        Cmd.none
                newFocusedHexId =
                    case result.nextFocus of
                        Just nextHexId -> Just nextHexId
                        Nothing -> model.focusedHexId
            in
            ( { model
                | gameMessage = result.gameMessage
                , gameMessageType = result.gameMessageType
                , grid = result.grid
                , submittedRows = result.submittedRows
                , focusedHexId = newFocusedHexId
                , currentActiveRow = result.currentActiveRow
              }, clearMessageCmd )

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

        PlaceBet ->
            let
                ( newModel, wordCmd ) =
                    selectNewWord model
            in
            ( { newModel | isBetModalVisible = False }
            , Cmd.batch
                [ placeBet { amount = model.betAmount }
                , wordCmd
                ]
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
            let
                -- Generate a new word immediately to avoid race conditions
                wordGenerator = getRandomWordGenerator fallbackTargetWords
                seedValue =
                    case model.currentWord of
                        Just word -> String.length word * 11 + model.currentActiveRow * 17
                        Nothing -> 67
                newWordSeed = Random.initialSeed seedValue
                (maybeNewWord, _) = Random.step wordGenerator newWordSeed
            in
            ( { initialModel
              | walletAddress = model.walletAddress
              , config = model.config
              , currentWord = maybeNewWord -- Set the word immediately
              }
            , Cmd.none
            )

        RandomWordGenerated word ->
            case word of
                Just w ->
                    ( { model | currentWord = Just w, loadingError = Nothing }, Cmd.none )
                Nothing ->
                    ( { model | loadingError = Just "No words available for selected difficulty" }, Cmd.none )

        StartNewGame ->
            let
                -- Generate a new word immediately to avoid race conditions
                wordGenerator = getRandomWordGenerator fallbackTargetWords
                -- Use a semi-random seed based on current state to maintain some variety
                seedValue =
                    case model.currentWord of
                        Just word -> String.length word * 7 + model.currentActiveRow * 13
                        Nothing -> 42
                newWordSeed = Random.initialSeed seedValue
                (maybeNewWord, _) = Random.step wordGenerator newWordSeed

                modelWithClearedGrid =
                    { initialModel
                    | validGuessesDB = model.validGuessesDB
                    , targetWordsDB = model.targetWordsDB
                    , config = model.config
                    , submittedRows = []
                    , currentActiveRow = 0
                    , isModalVisible = False
                    , modalMessage = ""
                    , isHintModalVisible = False
                    , isGameOverModalVisible = False
                    , isLettersModalVisible = False
                    , currentWord = maybeNewWord -- Set the word immediately
                    }
            in
            ( modelWithClearedGrid, Cmd.none ) -- No need for async command since word is set immediately

        RefreshPage ->
            ( model, refreshPage () )

        ClearGameMessage ->
            ( { model | gameMessage = Nothing, gameMessageType = "" }, Cmd.none )

        LoadValidGuesses ->
            ( model, loadValidGuessesCmd )

        SimpleValidGuessesLoaded result ->
            case result of
                Ok validGuessesList ->
                    -- Create a new ValidGuessesDB using the loaded validGuesses list and fallback data for other fields
                    let
                        updatedValidGuessesDB =
                            { version = "2.0"
                            , metadata =
                                { totalValidGuesses = List.length validGuessesList
                                , totalTargetWords = List.length fallbackTargetWords
                                , lastUpdated = "2024-03-21"
                                , source = "validguess.json"
                                , format = "5-letter-words"
                                , description = "Valid guess words database for Honeycomb puzzle"
                                }
                            , validGuesses = validGuessesList
                            , targetWords = fallbackTargetWords
                            , features =
                                { hasVowels = True
                                , allowsRepeatedLetters = True
                                , caseSensitive = False
                                }
                            }
                    in
                    ( { model | validGuessesDB = Just updatedValidGuessesDB, loadingError = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                Http.BadUrl url -> "Bad URL: " ++ url
                                Http.Timeout -> "Request timeout"
                                Http.NetworkError -> "Network error"
                                Http.BadStatus status -> "Bad status: " ++ String.fromInt status
                                Http.BadBody body -> "Bad response body: " ++ body
                    in
                    -- Keep the embedded database as fallback when external loading fails
                    ( { model | loadingError = Just ("Failed to load external valid guesses, using embedded list: " ++ errorMessage) }, Cmd.none )

        ShowSuccessModal ->
            ( { model | isModalVisible = True, gameMessage = Nothing }, Cmd.none )

        ShowGameOverModal ->
            ( { model | isGameOverModalVisible = True, gameMessage = Nothing }, Cmd.none )

        ShowHintModal ->
            ( { model | isHintModalVisible = True }, Cmd.none )

        CloseHintModal ->
            ( { model | isHintModalVisible = False }, Cmd.none )

        ShowLettersModal ->
            ( { model | isLettersModalVisible = True }, Cmd.none )

        CloseLettersModal ->
            ( { model | isLettersModalVisible = False }, Cmd.none )


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


-- Clear all letters from a specific row and reset their states to Empty
clearRow : Int -> List (List HexagonData) -> List (List HexagonData)
clearRow targetRowIndex grid =
    List.indexedMap (\rowIndex row ->
        if rowIndex == targetRowIndex then
            List.map (\hex -> { hex | letter = Nothing, state = Empty }) row
        else
            row
    ) grid


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
port placeBet : { amount : Float } -> Cmd msg
port refreshPage : () -> Cmd msg
port transactionResult : (String -> msg) -> Sub msg



-- VIEW





viewBetModal : Model -> Html Msg
viewBetModal model =
    -- Hidden for now - betting modal will be implemented later
    Html.text ""


calculatePotentialPayout : Model -> String
calculatePotentialPayout model =
    let
        multiplier = 2.0 -- Fixed multiplier since we removed difficulty levels

        rawPayout =
            model.betAmount * multiplier

        fee =
            rawPayout * 0.05

        finalPayout =
            rawPayout - fee
    in
    String.fromFloat (toFloat (round (finalPayout * 100)) / 100)


-- Function to get the best state for each letter from the grid
getLetterStates : List (List HexagonData) -> Dict.Dict Char HexState
getLetterStates grid =
    let
        allHexes = List.concat grid
        hexesWithLetters = List.filterMap (\hex ->
            case hex.letter of
                Just char -> Just (Char.toLower char, hex.state)
                Nothing -> Nothing
            ) allHexes

        -- Group by letter and find the best state for each
        letterGroups = List.foldl (\(char, state) dict ->
            case Dict.get char dict of
                Nothing -> Dict.insert char [state] dict
                Just states -> Dict.insert char (state :: states) dict
            ) Dict.empty hexesWithLetters

        -- Determine best state (Correct > Present > Absent > Empty)
        getBestState states =
            if List.member Correct states then Correct
            else if List.member Present states then Present
            else if List.member Absent states then Absent
            else Empty
    in
    Dict.map (\_ states -> getBestState states) letterGroups



-- Add this function before the view function
viewGameMessage : Model -> Html Msg
viewGameMessage model =
    case model.gameMessage of
        Nothing ->
            Html.text ""

        Just message ->
            let
                (bgColor, textColor, borderColor) =
                    case model.gameMessageType of
                        "success" ->
                            ("#d4edda", "#155724", "#c3e6cb")
                        "error" ->
                            ("#f8d7da", "#721c24", "#f5c6cb")
                        "info" ->
                            ("#d1ecf1", "#0c5460", "#bee5eb")
                        _ ->
                            ("#e2e3e5", "#383d41", "#d6d8db")
            in
            div
                [ style "width" "100%"
                , style "max-width" "600px"
                , style "margin-bottom" "15px"
                , style "padding" "12px 16px"
                , style "background-color" bgColor
                , style "color" textColor
                , style "border" ("1px solid " ++ borderColor)
                , style "border-radius" "6px"
                , style "font-size" "14px"
                , style "text-align" "center"
                , style "font-weight" "500"
                ]
                [ text message ]


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
        , viewGameControls model
        , viewGameMessage model
        , viewHoneycombGrid model
        , viewRules
        , viewBetModal model
        , if model.isModalVisible then viewSuccessModalSimple model else Html.text ""
        , if model.isHintModalVisible then viewHintModalSimple model else Html.text ""
        , if model.isGameOverModalVisible then viewGameOverModalSimple model else Html.text ""
        , if model.isLettersModalVisible then viewLettersModalSimple model else Html.text ""
        ]


viewGameControls : Model -> Html Msg
viewGameControls model =
    div
        [ style "width" "100%"
        , style "max-width" "600px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "padding" "10px 0"
        , style "margin-bottom" "20px"
        , style "gap" "15px"
        ]
        [ button
            [ onClick RefreshPage
            , style "padding" "10px 18px"
            , style "cursor" "pointer"
            , style "background-color" "#28a745"
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "5px"
            , style "font-size" "15px"
            , style "font-weight" "bold"
            ]
            [ text "New Game" ]
        , case model.currentWord of
            Just word ->
                div [ style "display" "flex", style "gap" "10px" ]
                    [ button
                        [ onClick ShowHintModal
                        , style "padding" "10px 18px"
                        , style "cursor" "pointer"
                        , style "background-color" "#007bff"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "5px"
                        , style "font-size" "15px"
                        , style "font-weight" "bold"
                        ]
                        [ text "Hint" ]
                    , button
                        [ onClick ShowLettersModal
                        , style "padding" "10px 18px"
                        , style "cursor" "pointer"
                        , style "background-color" "#ffc107"
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "5px"
                        , style "font-size" "15px"
                        , style "font-weight" "bold"
                        ]
                        [ text "Letters" ]
                    ]
            Nothing ->
                div
                    [ style "font-size" "14px"
                    , style "color" "#999"
                    ]
                    [ text "Loading puzzle..." ]
        ]


viewWalletControls : Model -> Html Msg
viewWalletControls model =
    -- Hidden for now - wallet and betting functionality will be implemented later
    Html.text ""

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
        [ h3 [ style "margin-top" "0", style "color" "#333", style "font-size" "18px" ] [ text "Game Rules" ]
        , p [ style "font-size" "14px", style "line-height" "1.6", style "color" "#555" ]
            [ text "Solve the 5-letter word puzzle! Guess the hidden word in 6 tries or less." ]
        , Html.ul [ style "font-size" "14px", style "line-height" "1.6", style "color" "#555", style "padding-left" "20px" ]
            [ Html.li [] [ text "Click on hexagons to focus and type letters." ]
            , Html.li [] [ text "Press Enter to submit your guess." ]
            , Html.li [] [ text "Green = correct letter in correct position." ]
            , Html.li [] [ text "Yellow = correct letter in wrong position." ]
            , Html.li [] [ text "Gray = letter not in the word." ]
            , Html.li [] [ text "Complete rows in order - you can only submit the current active row." ]
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

        -- Check if this hex is in a submitted row or not the active row
        isInSubmittedRow =
            case parseHexId hexData.id of
                Just ( rowIndex, _ ) -> isRowSubmitted model rowIndex
                Nothing -> False

        isInActiveRow =
            case parseHexId hexData.id of
                Just ( rowIndex, _ ) -> rowIndex == model.currentActiveRow
                Nothing -> False

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
            , style "cursor" (if not isInActiveRow then "not-allowed" else "pointer")
            , style "transition" ("filter " ++ String.fromInt cfg.transitionSpeedMs ++ "ms ease-out, transform " ++ String.fromInt cfg.transitionSpeedMs ++ "ms ease-out, background-color " ++ String.fromInt cfg.transitionSpeedMs ++ "ms ease-out, z-index 0s " ++ (if isHovered then "" else String.fromInt cfg.transitionSpeedMs ++ "ms"))
            , style "z-index" (if isHovered then "10" else "1")
            , style "opacity" (if isInSubmittedRow then "0.8" else if not isInActiveRow then "0.5" else "1.0")
            ]

        hoverSpecificStyles =
            if not isInActiveRow then
                -- No hover effects for inactive rows
                []
            else if isHovered && not isFocused then
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
        { init = \_ -> ( initialModel, Cmd.batch [ selectNewWordCmd, loadValidGuessesCmd ] ) -- Load word and valid guesses
        , view = \model -> { title = "Interactive Honeycomb Puzzle", body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        }


-- COMMANDS


selectNewWord : Model -> ( Model, Cmd Msg )
selectNewWord model =
    case model.targetWordsDB of
        Nothing ->
            -- This should never happen now since we embed the database
            ( { model | loadingError = Just "Target words database not available" }, Cmd.none )

        Just db ->
            ( model, selectNewWordCmd )


-- Add this helper function
getWordFromRow : List HexagonData -> Maybe String
getWordFromRow row =
    let
        letters =
            List.filterMap .letter row
    in
    if List.length letters == 5 then
        Just (String.fromList letters)
    else
        Nothing


-- Add a helper function to generate word selection command
selectNewWordCmd : Cmd Msg
selectNewWordCmd =
    Random.generate RandomWordGenerated (getRandomWordGenerator fallbackTargetWords)


-- HTTP command to load valid guesses from JSON file
loadValidGuessesCmd : Cmd Msg
loadValidGuessesCmd =
    Http.get
        { url = "validguess.json"
        , expect = Http.expectJson SimpleValidGuessesLoaded simpleValidGuessesDecoder
        }


-- Proper puzzle-style word checking that handles duplicates correctly
checkWordAgainstTarget : String -> String -> List HexState
checkWordAgainstTarget guess target =
    let
        guessChars = String.toList (String.toLower guess)
        targetChars = String.toList (String.toLower target)

        -- First pass: mark exact matches (green)
        exactMatches =
            List.indexedMap (\i guessChar ->
                case List.drop i targetChars |> List.head of
                    Just targetChar ->
                        if guessChar == targetChar then
                            Just i
                        else
                            Nothing
                    Nothing ->
                        Nothing
            ) guessChars
            |> List.filterMap identity

        -- Get remaining target chars (excluding exact matches)
        remainingTargetChars =
            List.indexedMap (\i char ->
                if List.member i exactMatches then
                    Nothing
                else
                    Just char
            ) targetChars
            |> List.filterMap identity

        -- Second pass: mark present letters (yellow) for non-exact matches
        (finalStates, _) =
            List.indexedMap (\i guessChar ->
                if List.member i exactMatches then
                    (Correct, remainingTargetChars)
                else
                    case List.filter (\c -> c == guessChar) remainingTargetChars of
                        [] -> (Absent, remainingTargetChars)
                        _ :: rest ->
                            let
                                newRemaining = removeFirstOccurrence guessChar remainingTargetChars
                            in
                            (Present, newRemaining)
            ) guessChars
            |> List.foldl (\(state, remaining) (states, currentRemaining) ->
                (state :: states, remaining)
            ) ([], remainingTargetChars)
    in
    List.reverse finalStates


-- Helper function to remove first occurrence of a character
removeFirstOccurrence : Char -> List Char -> List Char
removeFirstOccurrence target chars =
    case chars of
        [] -> []
        first :: rest ->
            if first == target then
                rest
            else
                first :: removeFirstOccurrence target rest


-- Update hex states in a specific row
updateRowWithStates : Int -> List HexState -> List (List HexagonData) -> List (List HexagonData)
updateRowWithStates rowIndex states grid =
    List.indexedMap (\r row ->
        if r == rowIndex then
            List.indexedMap (\c hex ->
                case List.drop c states |> List.head of
                    Just state -> { hex | state = state }
                    Nothing -> hex
            ) row
        else
            row
    ) grid

-- SIMPLE MODAL FUNCTIONS (avoiding import cycle)

viewSuccessModalSimple : Model -> Html Msg
viewSuccessModalSimple model =
    modalBackdrop CloseModal
        [ modalContent
            [ div [ style "font-size" "32px", style "margin-bottom" "10px" ]
                [ text "🎉 Congratulations! 🎉" ]
            , div [ style "font-size" "20px", style "color" "#28a745", style "margin-bottom" "10px" ]
                [ text ("You solved the puzzle!") ]
            , div [ style "font-size" "32px", style "font-weight" "bold", style "color" "#2e7d32", style "margin-bottom" "25px" ]
                [ text (Maybe.withDefault "UNKNOWN" model.currentWord) ]
            , modalButtons
                [ closeButton CloseModal
                , newGameButton
                ]
            ]
        ]

viewHintModalSimple : Model -> Html Msg
viewHintModalSimple model =
    modalBackdrop CloseHintModal
        [ modalContent
            [ div [ style "font-size" "18px", style "font-weight" "bold", style "margin-bottom" "15px" ]
                [ text "💡 Hint" ]
            , div [ style "font-size" "24px", style "font-weight" "bold", style "color" "#007bff", style "margin-bottom" "20px" ]
                [ text ("Target: " ++ String.toUpper (Maybe.withDefault "UNKNOWN" model.currentWord)) ]
            , modalButtons [ closeButton CloseHintModal ]
            ]
        ]

viewGameOverModalSimple : Model -> Html Msg
viewGameOverModalSimple model =
    modalBackdrop RefreshPage
        [ modalContent
            [ div [ style "font-size" "24px", style "font-weight" "bold", style "color" "#d32f2f", style "margin-bottom" "15px" ]
                [ text "😞 Game Over" ]
            , div [ style "font-size" "16px", style "color" "#666", style "margin-bottom" "20px" ]
                [ text "Sorry, the word was:" ]
            , div [ style "font-size" "32px", style "font-weight" "bold", style "color" "#2e7d32", style "margin-bottom" "25px" ]
                [ text (Maybe.withDefault "UNKNOWN" model.currentWord) ]
            , modalButtons [ newGameButton ]
            ]
        ]

viewLettersModalSimple : Model -> Html Msg
viewLettersModalSimple model =
    modalBackdrop CloseLettersModal
        [ modalContent
            [ div [ style "font-size" "18px", style "font-weight" "bold", style "margin-bottom" "20px" ]
                [ text "🔤 Letter Status" ]
            , keyboardDisplayAlphabetical (getLetterStates model.grid)
            , legendDisplay
            , modalButtons [ closeButton CloseLettersModal ]
            ]
        ]

-- Alphabetical keyboard display
keyboardDisplayAlphabetical : Dict.Dict Char HexState -> Html Msg
keyboardDisplayAlphabetical letterStates =
    let
        topRow = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']
        middleRow = ['j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r']
        bottomRow = ['s', 't', 'u', 'v', 'w', 'x', 'y', 'z']

        renderKey char =
            let
                state = Maybe.withDefault Empty (Dict.get char letterStates)
                (bgColor, textColor) =
                    case state of
                        Correct -> ("#6aaa64", "#ffffff")
                        Present -> ("#c9b458", "#ffffff")
                        Absent -> ("#787c7e", "#ffffff")
                        Empty -> ("#d3d6da", "#1a1a1b")
            in
            div
                [ style "display" "inline-block"
                , style "width" "40px"
                , style "height" "40px"
                , style "margin" "2px"
                , style "background-color" bgColor
                , style "color" textColor
                , style "border-radius" "4px"
                , style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , style "font-weight" "bold"
                , style "font-size" "16px"
                , style "text-transform" "uppercase"
                ]
                [ text (String.fromChar char) ]

        renderRow chars =
            div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "margin" "4px 0"
                ]
                (List.map renderKey chars)
    in
    div [ style "margin-bottom" "20px" ]
        [ renderRow topRow
        , renderRow middleRow
        , renderRow bottomRow
        ]


-- Modal helper functions
modalBackdrop : Msg -> List (Html Msg) -> Html Msg
modalBackdrop closeMsg content =
    div
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
        , onClick closeMsg
        ]
        content

modalContent : List (Html Msg) -> Html Msg
modalContent content =
    div
        [ style "background-color" "#fff"
        , style "padding" "40px 50px"
        , style "border-radius" "15px"
        , style "box-shadow" "0 10px 25px rgba(0,0,0,0.4)"
        , style "min-width" "400px"
        , style "max-width" "90%"
        , style "text-align" "center"
        , stopPropagationOn "click" (Decode.succeed (NoOp, True))
        ]
        content

modalButtons : List (Html Msg) -> Html Msg
modalButtons buttons =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "gap" "15px"
        , style "margin-top" "30px"
        ]
        buttons

closeButton : Msg -> Html Msg
closeButton msg =
    button
        [ onClick msg
        , style "padding" "12px 24px"
        , style "cursor" "pointer"
        , style "background-color" "#6c757d"
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "8px"
        , style "font-size" "16px"
        , style "font-weight" "500"
        ]
        [ text "Close" ]

newGameButton : Html Msg
newGameButton =
    button
        [ onClick RefreshPage
        , style "padding" "12px 24px"
        , style "cursor" "pointer"
        , style "background-color" "#28a745"
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "8px"
        , style "font-size" "16px"
        , style "font-weight" "bold"
        ]
        [ text "New Game" ]

legendDisplay : Html Msg
legendDisplay =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "gap" "20px"
        , style "margin-bottom" "15px"
        , style "font-size" "12px"
        ]
        [ legendItem "#6aaa64" "Correct"
        , legendItem "#c9b458" "Wrong Position"
        , legendItem "#787c7e" "Not in Word"
        ]

legendItem : String -> String -> Html Msg
legendItem color label =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "5px"
        ]
        [ div
            [ style "width" "16px"
            , style "height" "16px"
            , style "background-color" color
            , style "border-radius" "2px"
            ]
            []
        , text label
        ]
