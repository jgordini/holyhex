module Game.Model exposing 
    ( Model
    , Msg(..)
    , HexId(..)
    , HexagonData
    , HexState(..)
    , GameState(..)
    , GameMessage(..)
    , ValidationError(..)
    , SubmissionError(..)
    , Grid
    , Config
    , initialModel
    , init
    , subscriptions
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Http
import Random
import Game.Database as Database
import Game.Word as Word


-- TYPES


type HexId = HexId Int Int  -- row, col


type alias HexagonData =
    { id : HexId
    , letter : Maybe Char
    , state : HexState
    }


type HexState
    = Empty
    | Correct
    | Present
    | Absent


type GameState
    = Loading
    | Playing
    | Won Int  -- number of guesses
    | Lost String  -- target word


type GameMessage
    = Success String
    | Error String
    | Info String


type ValidationError
    = TooShort
    | TooLong
    | InvalidChars
    | NotInDictionary
    | RepeatingPattern
    | NoVowels


type SubmissionError
    = InvalidRow
    | AlreadySubmitted
    | GameOver
    | IncompleteWord
    | InvalidWord ValidationError


type alias Grid = Array (Array HexagonData)


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


type alias UIState =
    { hoveredHexId : Maybe HexId
    , focusedHexId : Maybe HexId
    , gameMessage : Maybe GameMessage
    , isHintModalVisible : Bool
    , isGameOverModalVisible : Bool
    , isLettersModalVisible : Bool
    , isSuccessModalVisible : Bool
    }


type alias GameData =
    { grid : Grid
    , gameState : GameState
    , currentWord : Maybe String
    , submittedRows : Set Int
    , currentActiveRow : Int
    , letterStates : Dict Char HexState
    }


type alias Model =
    { gameData : GameData
    , uiState : UIState
    , config : Config
    , wordDatabase : Database.WordDatabase
    , loadingError : Maybe String
    }


-- MESSAGES


type Msg
    = NoOp
    | MouseEnterHex HexId
    | MouseLeaveHex HexId
    | FocusHex HexId
    | KeyPressed Int
    | SubmitAttempt
    | StartNewGame
    | ClearGameMessage
    | DatabaseLoaded (Result Http.Error Database.WordDatabase)
    | RandomWordGenerated (Maybe String)
    | ShowHintModal
    | CloseHintModal
    | ShowGameOverModal
    | CloseGameOverModal
    | ShowLettersModal
    | CloseLettersModal
    | ShowSuccessModal
    | CloseSuccessModal


-- CONSTANTS


gameConfig =
    { maxGuesses = 6
    , wordLength = 5
    , gridRows = 6
    , gridCols = 5
    }


-- INITIALIZATION


initialConfig : Config
initialConfig =
    let
        hexOuterWidth = 70.0
        hexOuterHeight = hexOuterWidth * 1.15470053838
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


initialGrid : Grid
initialGrid =
    let
        createHex row col =
            { id = HexId row col
            , letter = Nothing
            , state = Empty
            }
        
        createRow rowIndex =
            Array.initialize gameConfig.gridCols (createHex rowIndex)
    in
    Array.initialize gameConfig.gridRows createRow


initialUIState : UIState
initialUIState =
    { hoveredHexId = Nothing
    , focusedHexId = Just (HexId 0 0)
    , gameMessage = Nothing
    , isHintModalVisible = False
    , isGameOverModalVisible = False
    , isLettersModalVisible = False
    , isSuccessModalVisible = False
    }


initialGameData : GameData
initialGameData =
    { grid = initialGrid
    , gameState = Loading
    , currentWord = Nothing
    , submittedRows = Set.empty
    , currentActiveRow = 0
    , letterStates = Dict.empty
    }


initialModel : Model
initialModel =
    { gameData = initialGameData
    , uiState = initialUIState
    , config = initialConfig
    , wordDatabase = Database.empty
    , loadingError = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch
        [ Database.loadWordDatabase DatabaseLoaded
        , selectNewWordCmd
        ]
    )


-- HELPER FUNCTIONS


selectNewWordCmd : Cmd Msg
selectNewWordCmd =
    Random.generate RandomWordGenerated (Word.randomWordGenerator Database.fallbackTargetWords)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none  -- Transaction subscriptions would go here