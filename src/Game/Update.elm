module Game.Update exposing (update)

import Array
import Set
import Dict
import Task
import Process
import Random
import Game.Model as Model exposing (..)
import Game.Database as Database exposing (ValidationError(..))
import Game.Word as Word


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseEnterHex hexId ->
            ( updateUIState (\ui -> { ui | hoveredHexId = Just hexId }) model, Cmd.none )

        MouseLeaveHex hexId ->
            let
                newHoveredId =
                    if model.uiState.hoveredHexId == Just hexId then
                        Nothing
                    else
                        model.uiState.hoveredHexId
            in
            ( updateUIState (\ui -> { ui | hoveredHexId = newHoveredId }) model, Cmd.none )

        FocusHex hexId ->
            case hexId of
                HexId rowIndex _ ->
                    if rowIndex /= model.gameData.currentActiveRow then
                        ( model, Cmd.none )
                    else
                        ( updateUIState (\ui -> { ui | focusedHexId = Just hexId }) model, Cmd.none )

        KeyPressed code ->
            handleKeyPress code model

        SubmitAttempt ->
            handleSubmission model

        StartNewGame ->
            startNewGame model

        ClearGameMessage ->
            ( updateUIState (\ui -> { ui | gameMessage = Nothing }) model, Cmd.none )

        DatabaseLoaded result ->
            handleDatabaseLoaded result model

        RandomWordGenerated word ->
            handleRandomWord word model

        ShowHintModal ->
            ( updateUIState (\ui -> { ui | isHintModalVisible = True }) model, Cmd.none )

        CloseHintModal ->
            ( updateUIState (\ui -> { ui | isHintModalVisible = False }) model, Cmd.none )

        ShowGameOverModal ->
            ( updateUIState (\ui -> { ui | isGameOverModalVisible = True }) model, Cmd.none )

        CloseGameOverModal ->
            ( updateUIState (\ui -> { ui | isGameOverModalVisible = False }) model, Cmd.none )

        ShowLettersModal ->
            ( updateUIState (\ui -> { ui | isLettersModalVisible = True }) model, Cmd.none )

        CloseLettersModal ->
            ( updateUIState (\ui -> { ui | isLettersModalVisible = False }) model, Cmd.none )

        ShowSuccessModal ->
            ( updateUIState (\ui -> { ui | isSuccessModalVisible = True, gameMessage = Nothing }) model, Cmd.none )

        CloseSuccessModal ->
            ( updateUIState (\ui -> { ui | isSuccessModalVisible = False }) model, Cmd.none )


-- UPDATE HELPERS


updateUIState : (UIState -> UIState) -> Model -> Model
updateUIState fn model =
    { model | uiState = fn model.uiState }


updateGameData : (GameData -> GameData) -> Model -> Model
updateGameData fn model =
    { model | gameData = fn model.gameData }


-- KEY HANDLING


handleKeyPress : Int -> Model -> ( Model, Cmd Msg )
handleKeyPress code model =
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
        case model.uiState.focusedHexId of
            Nothing ->
                ( model, Cmd.none )

            Just (HexId r c) ->
                if r /= model.gameData.currentActiveRow then
                    ( model, Cmd.none )
                else if code == backspaceCode then
                    handleBackspace model (HexId r c)
                else if isAlphanumeric then
                    handleLetterInput model (HexId r c) (Char.fromCode code)
                else
                    ( model, Cmd.none )


handleBackspace : Model -> HexId -> ( Model, Cmd Msg )
handleBackspace model (HexId r c) =
    case getHexFromGrid model.gameData.grid (HexId r c) of
        Nothing ->
            ( model, Cmd.none )
        
        Just currentHex ->
            if currentHex.letter /= Nothing then
                -- Clear current hex
                let
                    newGrid = updateHexInGrid (HexId r c) (\hex -> { hex | letter = Nothing, state = Empty }) model.gameData.grid
                in
                ( updateGameData (\game -> { game | grid = newGrid }) model, Cmd.none )
            else
                -- Move to previous hex and clear it
                case findPreviousHexInRow (HexId r c) of
                    Nothing ->
                        ( model, Cmd.none )
                    
                    Just prevHexId ->
                        let
                            newGrid = updateHexInGrid prevHexId (\hex -> { hex | letter = Nothing, state = Empty }) model.gameData.grid
                        in
                        ( model 
                            |> updateGameData (\game -> { game | grid = newGrid })
                            |> updateUIState (\ui -> { ui | focusedHexId = Just prevHexId })
                        , Cmd.none )


handleLetterInput : Model -> HexId -> Char -> ( Model, Cmd Msg )
handleLetterInput model (HexId r c) char =
    let
        updatedGrid = updateHexInGrid (HexId r c) (\hex -> { hex | letter = Just char, state = Empty }) model.gameData.grid
        nextFocus = findNextHexInRow (HexId r c) |> Maybe.withDefault (HexId r c)
    in
    ( model
        |> updateGameData (\game -> { game | grid = updatedGrid })
        |> updateUIState (\ui -> { ui | focusedHexId = Just nextFocus })
    , Cmd.none )


-- SUBMISSION HANDLING


handleSubmission : Model -> ( Model, Cmd Msg )
handleSubmission model =
    case validateSubmission model of
        Err submissionError ->
            handleSubmissionError submissionError model
        
        Ok word ->
            processValidSubmission word model


validateSubmission : Model -> Result SubmissionError String
validateSubmission model =
    case model.uiState.focusedHexId of
        Nothing ->
            Err InvalidRow
            
        Just (HexId rowIndex _) ->
            if rowIndex /= model.gameData.currentActiveRow then
                Err InvalidRow
            else if Set.member rowIndex model.gameData.submittedRows then
                Err AlreadySubmitted
            else
                case Word.getWordFromGrid model.gameData.grid rowIndex of
                    Nothing ->
                        Err IncompleteWord
                    
                    Just word ->
                        case Database.validateWord model.wordDatabase word of
                            Err validationError ->
                                Err (InvalidWord validationError)
                            
                            Ok validWord ->
                                Ok validWord


handleSubmissionError : SubmissionError -> Model -> ( Model, Cmd Msg )
handleSubmissionError error model =
    let
        (message, shouldClearRow) = 
            case error of
                InvalidRow ->
                    ("You can only submit the current active row.", False)
                
                AlreadySubmitted ->
                    ("This row has already been submitted.", False)
                
                GameOver ->
                    ("Game is over.", False)
                
                IncompleteWord ->
                    ("Please complete the word before submitting.", False)
                
                InvalidWord validationError ->
                    let
                        errorMsg = case validationError of
                            TooShort -> "Word is too short."
                            TooLong -> "Word is too long."
                            InvalidChars -> "Word contains invalid characters."
                            NotInDictionary -> "Word not found in dictionary."
                            RepeatingPattern -> "Invalid repeating pattern."
                            NoVowels -> "Word must contain vowels."
                    in
                    (errorMsg ++ " Try again!", True)
        
        updatedModel = 
            if shouldClearRow then
                let
                    clearedGrid = clearRow model.gameData.currentActiveRow model.gameData.grid
                    firstHexId = HexId model.gameData.currentActiveRow 0
                in
                model
                    |> updateGameData (\game -> { game | grid = clearedGrid })
                    |> updateUIState (\ui -> { ui | focusedHexId = Just firstHexId })
            else
                model
        
        finalModel = updateUIState (\ui -> { ui | gameMessage = Just (Error message) }) updatedModel
        
        clearCmd = Task.perform (\_ -> ClearGameMessage) (Process.sleep 3000)
    in
    ( finalModel, clearCmd )


processValidSubmission : String -> Model -> ( Model, Cmd Msg )
processValidSubmission word model =
    let
        targetWord = Maybe.withDefault "SPEED" model.gameData.currentWord
        hexStates = Word.checkWordAgainstTarget word targetWord
        updatedGrid = updateRowWithStates model.gameData.currentActiveRow hexStates model.gameData.grid
        updatedLetterStates = updateLetterStates model.gameData.letterStates word hexStates
        isCorrect = String.toLower word == String.toLower targetWord
        rowIndex = model.gameData.currentActiveRow
    in
    if isCorrect then
        handleWin model updatedGrid updatedLetterStates (rowIndex + 1)
    else
        handleIncorrectGuess model updatedGrid updatedLetterStates rowIndex


handleWin : Model -> Grid -> Dict.Dict Char HexState -> Int -> ( Model, Cmd Msg )
handleWin model newGrid newLetterStates guessCount =
    let
        updatedModel = model
            |> updateGameData (\game -> 
                { game 
                | grid = newGrid
                , letterStates = newLetterStates
                , gameState = Won guessCount
                , submittedRows = Set.insert model.gameData.currentActiveRow game.submittedRows
                })
        
        showSuccessCmd = Task.perform (\_ -> ShowSuccessModal) (Process.sleep 1000)
    in
    ( updatedModel, showSuccessCmd )


handleIncorrectGuess : Model -> Grid -> Dict.Dict Char HexState -> Int -> ( Model, Cmd Msg )
handleIncorrectGuess model newGrid newLetterStates rowIndex =
    let
        nextRow = model.gameData.currentActiveRow + 1
        maxRows = Array.length model.gameData.grid
        hasNextRow = nextRow < maxRows
        
        updatedModel = model
            |> updateGameData (\game -> 
                { game 
                | grid = newGrid
                , letterStates = newLetterStates
                , submittedRows = Set.insert rowIndex game.submittedRows
                , currentActiveRow = if hasNextRow then nextRow else game.currentActiveRow
                })
    in
    if hasNextRow then
        let
            nextFocusId = HexId nextRow 0
            finalModel = updateUIState (\ui -> 
                { ui 
                | focusedHexId = Just nextFocusId
                , gameMessage = Just (Info "Valid word, but not correct. Try again!")
                }) updatedModel
            clearCmd = Task.perform (\_ -> ClearGameMessage) (Process.sleep 3000)
        in
        ( finalModel, clearCmd )
    else
        let
            gameOverModel = updateGameData (\game -> 
                { game | gameState = Lost (Maybe.withDefault "UNKNOWN" model.gameData.currentWord) }) updatedModel
            showGameOverCmd = Task.perform (\_ -> ShowGameOverModal) (Process.sleep 1000)
        in
        ( gameOverModel, showGameOverCmd )


-- DATABASE AND WORD HANDLING


handleDatabaseLoaded : Result Http.Error Database.WordDatabase -> Model -> ( Model, Cmd Msg )
handleDatabaseLoaded result model =
    case result of
        Ok database ->
            ( { model | wordDatabase = database, loadingError = Nothing }, Cmd.none )
        
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
            ( { model | loadingError = Just ("Failed to load word database: " ++ errorMessage) }, Cmd.none )


handleRandomWord : Maybe String -> Model -> ( Model, Cmd Msg )
handleRandomWord maybeWord model =
    case maybeWord of
        Just word ->
            ( model 
                |> updateGameData (\game -> { game | currentWord = Just word, gameState = Playing })
            , Cmd.none )
        
        Nothing ->
            ( { model | loadingError = Just "No words available" }, Cmd.none )


startNewGame : Model -> ( Model, Cmd Msg )
startNewGame model =
    let
        newGameData = 
            { initialGameData 
            | gameState = Playing
            , currentWord = Nothing
            }
        
        newModel = 
            { model 
            | gameData = newGameData
            , uiState = { initialUIState | focusedHexId = Just (HexId 0 0) }
            }
    in
    ( newModel, selectNewWordCmd )


-- GRID MANIPULATION HELPERS


getHexFromGrid : Grid -> HexId -> Maybe HexagonData
getHexFromGrid grid (HexId row col) =
    Array.get row grid
        |> Maybe.andThen (Array.get col)


updateHexInGrid : HexId -> (HexagonData -> HexagonData) -> Grid -> Grid
updateHexInGrid (HexId targetRow targetCol) updateFn grid =
    Array.indexedMap (\rowIndex row ->
        if rowIndex == targetRow then
            Array.indexedMap (\colIndex hex ->
                if colIndex == targetCol then
                    updateFn hex
                else
                    hex
            ) row
        else
            row
    ) grid


clearRow : Int -> Grid -> Grid
clearRow targetRowIndex grid =
    Array.indexedMap (\rowIndex row ->
        if rowIndex == targetRowIndex then
            Array.map (\hex -> { hex | letter = Nothing, state = Empty }) row
        else
            row
    ) grid


updateRowWithStates : Int -> List HexState -> Grid -> Grid
updateRowWithStates rowIndex states grid =
    Array.indexedMap (\r row ->
        if r == rowIndex then
            Array.indexedMap (\c hex ->
                case List.drop c states |> List.head of
                    Just state -> { hex | state = state }
                    Nothing -> hex
            ) row
        else
            row
    ) grid


updateLetterStates : Dict.Dict Char HexState -> String -> List HexState -> Dict.Dict Char HexState
updateLetterStates currentStates word hexStates =
    let
        chars = String.toList (String.toLower word)
        charStatePairs = List.map2 Tuple.pair chars hexStates
        
        getBestState existingState newState =
            case (existingState, newState) of
                (_, Correct) -> Correct
                (Correct, _) -> Correct
                (_, Present) -> Present
                (Present, _) -> Present
                (_, Absent) -> Absent
                (Absent, _) -> Absent
                _ -> Empty
    in
    List.foldl (\(char, newState) dict ->
        case Dict.get char dict of
            Nothing -> Dict.insert char newState dict
            Just existingState -> Dict.insert char (getBestState existingState newState) dict
    ) currentStates charStatePairs


-- NAVIGATION HELPERS


findPreviousHexInRow : HexId -> Maybe HexId
findPreviousHexInRow (HexId row col) =
    if col > 0 then
        Just (HexId row (col - 1))
    else
        Nothing


findNextHexInRow : HexId -> Maybe HexId
findNextHexInRow (HexId row col) =
    if col < 4 then -- Assuming 5 columns (0-4)
        Just (HexId row (col + 1))
    else
        Nothing


-- COMMANDS


selectNewWordCmd : Cmd Msg
selectNewWordCmd =
    Random.generate RandomWordGenerated (Word.randomWordGenerator Database.fallbackTargetWords)