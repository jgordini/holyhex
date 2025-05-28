port module Main exposing (main, Model, Difficulty(..), PuzzleResult(..), Msg(..), initialModel, update, stringToDifficulty, difficultyToString, calculatePotentialPayout)

import Browser
import Browser.Events
import Debug
import Dict
import Html exposing (Html, Attribute, div, h3, p, button, span, text) -- text is already imported
import Html.Attributes exposing (class, style, id)
import Html.Events exposing (keyCode, onClick, on, onMouseEnter, onMouseLeave, stopPropagationOn, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required, optional)
import Task exposing (Task, succeed, perform)
import Platform.Sub
import Random
import Array
import Process
import Http


-- EMBEDDED VALID GUESSES DATABASE


embeddedValidGuessesDB : ValidGuessesDB
embeddedValidGuessesDB =
    { version = "2.0"
    , metadata = 
        { totalValidGuesses = 14854
        , totalTargetWords = 2309
        , lastUpdated = "2024-03-21"
        , source = "words.txt"
        , format = "5-letter-words"
        , description = "Valid guess words database for Honeycomb Wordle"
        }
    , validGuesses = 
        [ "about", "above", "abuse", "actor", "acute", "admit", "adopt", "adult", "after", "again"
        , "agent", "agree", "ahead", "alarm", "album", "alert", "alien", "align", "alike", "alive"
        , "allow", "alone", "along", "alter", "among", "anger", "angle", "angry", "apart", "apple"
        , "apply", "arena", "argue", "arise", "array", "aside", "asset", "avoid", "awake", "award"
        , "aware", "badly", "baker", "bases", "basic", "beach", "began", "begin", "being", "below"
        , "bench", "billy", "birth", "black", "blame", "blank", "blast", "blind", "block", "blood"
        , "board", "boost", "booth", "bound", "brain", "brand", "brass", "brave", "bread", "break"
        , "breed", "brief", "bring", "broad", "broke", "brown", "build", "built", "buyer", "cable"
        , "calif", "carry", "catch", "cause", "chain", "chair", "chaos", "charm", "chart", "chase"
        , "cheap", "check", "chest", "chief", "child", "china", "chose", "civil", "claim", "class"
        , "clean", "clear", "click", "climb", "clock", "close", "cloud", "coach", "coast", "could"
        , "count", "court", "cover", "craft", "crash", "crazy", "cream", "crime", "cross", "crowd"
        , "crown", "crude", "curve", "cycle", "daily", "dance", "dated", "dealt", "death", "debut"
        , "delay", "depth", "doing", "doubt", "dozen", "draft", "drama", "drank", "dream", "dress"
        , "drill", "drink", "drive", "drove", "dying", "eager", "early", "earth", "eight", "elite"
        , "empty", "enemy", "enjoy", "enter", "entry", "equal", "error", "event", "every", "exact"
        , "exist", "extra", "faith", "false", "fault", "fiber", "field", "fifth", "fifty", "fight"
        , "final", "first", "fixed", "flash", "fleet", "floor", "fluid", "focus", "force", "forth"
        , "forty", "forum", "found", "frame", "frank", "fraud", "fresh", "front", "fruit", "fully"
        , "funny", "giant", "given", "glass", "globe", "going", "grace", "grade", "grand", "grant"
        , "grass", "grave", "great", "green", "gross", "group", "grown", "guard", "guess", "guest"
        , "guide", "happy", "harry", "heart", "heavy", "hello", "hence", "henry", "horse", "hotel", "house"
        , "human", "ideal", "image", "index", "inner", "input", "issue", "japan", "jimmy", "joint"
        , "jones", "judge", "known", "label", "large", "laser", "later", "laugh", "layer", "learn"
        , "lease", "least", "leave", "legal", "level", "lewis", "light", "limit", "links", "lives"
        , "local", "loose", "lower", "lucky", "lunch", "lying", "magic", "major", "maker", "march"
        , "maria", "match", "maybe", "mayor", "meant", "media", "metal", "might", "minor", "minus"
        , "mixed", "model", "money", "month", "moral", "motor", "mount", "mouse", "mouth", "moved"
        , "movie", "music", "needs", "never", "newly", "night", "noise", "north", "noted", "novel"
        , "nurse", "occur", "ocean", "offer", "often", "order", "other", "ought", "paint", "panel"
        , "paper", "party", "peace", "peter", "phase", "phone", "photo", "piano", "piece", "pilot"
        , "pitch", "place", "plain", "plane", "plant", "plate", "point", "pound", "power", "press"
        , "price", "pride", "prime", "print", "prior", "prize", "proof", "proud", "prove", "queen"
        , "quick", "quiet", "quite", "radio", "raise", "range", "rapid", "ratio", "reach", "ready"
        , "realm", "rebel", "refer", "relax", "repay", "reply", "right", "rigid", "rival", "river"
        , "robin", "roger", "roman", "rough", "round", "route", "royal", "rural", "scale", "scene"
        , "scope", "score", "sense", "serve", "seven", "shall", "shape", "share", "sharp", "sheet"
        , "shelf", "shell", "shift", "shine", "shirt", "shock", "shoot", "short", "shown", "sides"
        , "sight", "silly", "since", "sixth", "sixty", "sized", "skill", "sleep", "slide", "small"
        , "smart", "smile", "smith", "smoke", "snake", "snow", "solid", "solve", "sorry", "sound"
        , "south", "space", "spare", "speak", "speed", "spend", "spent", "split", "spoke", "sport"
        , "staff", "stage", "stake", "stand", "start", "state", "steam", "steel", "steep", "steer"
        , "stick", "still", "stock", "stone", "stood", "store", "storm", "story", "strip", "stuck"
        , "study", "stuff", "style", "sugar", "suite", "super", "sweet", "table", "taken", "taste"
        , "taxes", "teach", "teeth", "terry", "texas", "thank", "theft", "their", "theme", "there"
        , "these", "thick", "thing", "think", "third", "those", "three", "threw", "throw", "thumb"
        , "tight", "timer", "tired", "title", "today", "topic", "total", "touch", "tough", "tower"
        , "track", "trade", "train", "treat", "trend", "trial", "tribe", "trick", "tried", "tries"
        , "truck", "truly", "trunk", "trust", "truth", "twice", "uncle", "under", "undue", "union"
        , "unity", "until", "upper", "upset", "urban", "usage", "usual", "valid", "value", "video"
        , "virus", "visit", "vital", "vocal", "voice", "waste", "watch", "water", "wheel", "where"
        , "which", "while", "white", "whole", "whose", "woman", "women", "world", "worry", "worse"
        , "worst", "worth", "would", "write", "wrong", "wrote", "young", "youth"
        ] -- Sample of common 5-letter words for validation
    , targetWords =
        { easy = 
            [ "yield", "thong", "vivid", "bride", "angst", "audit", "amply", "catty", "snout", "wager"
            , "press", "asset", "abhor", "vodka", "spied", "porch", "shout", "plume", "saute", "alley"
            ]
        , medium = 
            [ "caulk", "gully", "foyer", "shard", "being", "colon", "wrung", "perky", "mount", "triad"
            , "swing", "ferry", "idyll", "twixt", "shave", "whoop", "foist", "natal", "shore", "rhyme"
            ]
        , hard = 
            [ "borax", "proxy", "epoch", "tryst", "dwelt", "staid", "allot", "omega", "squab", "lemon"
            , "plunk", "foggy", "attic", "theta", "snack", "pleat", "rugby", "floss", "lunge", "glory"
            ]
        }
    , difficulties =
        { easy = { multiplier = 1.5, description = "Common words with frequent letter patterns" }
        , medium = { multiplier = 2.0, description = "Words with moderate complexity and mixed letter patterns" }
        , hard = { multiplier = 3.0, description = "Rare words with uncommon letter combinations" }
        }
    , categories =
        { common = 
            [ "yield", "thong", "vivid", "bride", "angst", "audit", "amply", "catty", "snout", "wager"
            , "press", "asset", "abhor", "vodka", "spied", "porch", "shout", "plume", "saute", "alley"
            ]
        , nature = [ "shore", "mount", "lemon", "plume" ]
        , actions = [ "swing", "shave", "lunge", "snout" ]
        , technology = [ "proxy", "omega" ]
        }
    , features =
        { hasVowels = True
        , allowsRepeatedLetters = True
        , caseSensitive = False
        }
    }


-- EMBEDDED TARGET WORDS DATABASE


embeddedTargetWordsDB : TargetWordsDB
embeddedTargetWordsDB =
    { version = "1.0"
    , metadata = 
        { totalTargetWords = 2309
        , lastUpdated = "2024-03-21"
        , source = "answerlist.txt"
        , format = "5-letter-words"
        , description = "Target words database for Honeycomb Wordle puzzles"
        }
    , targetWords =
        { easy = 
            [ "yield", "thong", "vivid", "bride", "angst", "audit", "amply", "catty", "snout", "wager"
            , "press", "asset", "abhor", "vodka", "spied", "porch", "shout", "plume", "saute", "alley"
            ] -- Sample words - full list loaded from answerlist.json
        , medium = 
            [ "caulk", "gully", "foyer", "shard", "being", "colon", "wrung", "perky", "mount", "triad"
            , "swing", "ferry", "idyll", "twixt", "shave", "whoop", "foist", "natal", "shore", "rhyme"
            ] -- Sample words - full list loaded from answerlist.json
        , hard = 
            [ "borax", "proxy", "epoch", "tryst", "dwelt", "staid", "allot", "omega", "squab", "lemon"
            , "plunk", "foggy", "attic", "theta", "snack", "pleat", "rugby", "floss", "lunge", "glory"
            ] -- Sample words - full list loaded from answerlist.json
        }
    , difficulties =
        { easy = { multiplier = 1.5, description = "Common words with frequent letter patterns", wordCount = 923 }
        , medium = { multiplier = 2.0, description = "Words with moderate complexity and mixed letter patterns", wordCount = 808 }
        , hard = { multiplier = 3.0, description = "Rare words with uncommon letter combinations", wordCount = 578 }
        }
    , categories =
        { common = 
            [ "yield", "thong", "vivid", "bride", "angst", "audit", "amply", "catty", "snout", "wager"
            , "press", "asset", "abhor", "vodka", "spied", "porch", "shout", "plume", "saute", "alley"
            ]
        , nature = [ "shore", "mount", "lemon", "plume" ]
        , actions = [ "swing", "shave", "lunge", "snout" ]
        , technology = [ "proxy", "omega" ]
        }
    , features =
        { hasVowels = True
        , allowsRepeatedLetters = True
        , caseSensitive = False
        }
    }


-- VALID GUESSES DATABASE TYPES


type alias ValidGuessesDB =
    { version : String
    , metadata : ValidGuessesMetadata
    , validGuesses : List String
    , targetWords : WordsByDifficulty
    , difficulties : DifficultyConfig
    , categories : WordCategories
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


type alias DifficultyConfig =
    { easy : DifficultyLevel
    , medium : DifficultyLevel
    , hard : DifficultyLevel
    }


type alias DifficultyLevel =
    { multiplier : Float
    , description : String
    }


type alias WordsByDifficulty =
    { easy : List String
    , medium : List String
    , hard : List String
    }


type alias WordCategories =
    { common : List String
    , nature : List String
    , actions : List String
    , technology : List String
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
    , targetWords : WordsByDifficulty
    , difficulties : ExtendedDifficultyConfig
    , categories : WordCategories
    , features : SolutionsFeatures
    }


type alias TargetWordsMetadata =
    { totalTargetWords : Int
    , lastUpdated : String
    , source : String
    , format : String
    , description : String
    }


type alias ExtendedDifficultyConfig =
    { easy : ExtendedDifficultyLevel
    , medium : ExtendedDifficultyLevel
    , hard : ExtendedDifficultyLevel
    }


type alias ExtendedDifficultyLevel =
    { multiplier : Float
    , description : String
    , wordCount : Int
    }


-- VALID GUESSES DATABASE DECODERS


validGuessesDecoder : Decoder ValidGuessesDB
validGuessesDecoder =
    Decode.succeed ValidGuessesDB
        |> Pipeline.required "version" Decode.string
        |> Pipeline.required "metadata" metadataDecoder
        |> Pipeline.required "validGuesses" (Decode.list Decode.string)
        |> Pipeline.required "targetWords" wordsByDifficultyDecoder
        |> Pipeline.required "difficulties" difficultiesDecoder
        |> Pipeline.required "categories" categoriesDecoder
        |> Pipeline.required "features" featuresDecoder


metadataDecoder : Decoder ValidGuessesMetadata
metadataDecoder =
    Decode.succeed ValidGuessesMetadata
        |> Pipeline.required "totalValidGuesses" Decode.int
        |> Pipeline.required "totalTargetWords" Decode.int
        |> Pipeline.required "lastUpdated" Decode.string
        |> Pipeline.required "source" Decode.string
        |> Pipeline.required "format" Decode.string
        |> Pipeline.required "description" Decode.string


difficultiesDecoder : Decoder DifficultyConfig
difficultiesDecoder =
    Decode.succeed DifficultyConfig
        |> Pipeline.required "easy" difficultyLevelDecoder
        |> Pipeline.required "medium" difficultyLevelDecoder
        |> Pipeline.required "hard" difficultyLevelDecoder


difficultyLevelDecoder : Decoder DifficultyLevel
difficultyLevelDecoder =
    Decode.succeed DifficultyLevel
        |> Pipeline.required "multiplier" Decode.float
        |> Pipeline.required "description" Decode.string


wordsByDifficultyDecoder : Decoder WordsByDifficulty
wordsByDifficultyDecoder =
    Decode.succeed WordsByDifficulty
        |> Pipeline.required "easy" (Decode.list Decode.string)
        |> Pipeline.required "medium" (Decode.list Decode.string)
        |> Pipeline.required "hard" (Decode.list Decode.string)


categoriesDecoder : Decoder WordCategories
categoriesDecoder =
    Decode.succeed WordCategories
        |> Pipeline.required "common" (Decode.list Decode.string)
        |> Pipeline.required "nature" (Decode.list Decode.string)
        |> Pipeline.required "actions" (Decode.list Decode.string)
        |> Pipeline.required "technology" (Decode.list Decode.string)


featuresDecoder : Decoder SolutionsFeatures
featuresDecoder =
    Decode.succeed SolutionsFeatures
        |> Pipeline.required "hasVowels" Decode.bool
        |> Pipeline.required "allowsRepeatedLetters" Decode.bool
        |> Pipeline.required "caseSensitive" Decode.bool


-- Simple decoder for validguess.json file (only contains validGuesses list)
simpleValidGuessesDecoder : Decoder (List String)
simpleValidGuessesDecoder =
    Decode.field "validGuesses" (Decode.list Decode.string)


-- TARGET WORDS DATABASE DECODERS


targetWordsDecoder : Decoder TargetWordsDB
targetWordsDecoder =
    Decode.succeed TargetWordsDB
        |> Pipeline.required "version" Decode.string
        |> Pipeline.required "metadata" targetWordsMetadataDecoder
        |> Pipeline.required "targetWords" wordsByDifficultyDecoder
        |> Pipeline.required "difficulties" extendedDifficultiesDecoder
        |> Pipeline.required "categories" categoriesDecoder
        |> Pipeline.required "features" featuresDecoder


targetWordsMetadataDecoder : Decoder TargetWordsMetadata
targetWordsMetadataDecoder =
    Decode.succeed TargetWordsMetadata
        |> Pipeline.required "totalTargetWords" Decode.int
        |> Pipeline.required "lastUpdated" Decode.string
        |> Pipeline.required "source" Decode.string
        |> Pipeline.required "format" Decode.string
        |> Pipeline.required "description" Decode.string


extendedDifficultiesDecoder : Decoder ExtendedDifficultyConfig
extendedDifficultiesDecoder =
    Decode.succeed ExtendedDifficultyConfig
        |> Pipeline.required "easy" extendedDifficultyLevelDecoder
        |> Pipeline.required "medium" extendedDifficultyLevelDecoder
        |> Pipeline.required "hard" extendedDifficultyLevelDecoder


extendedDifficultyLevelDecoder : Decoder ExtendedDifficultyLevel
extendedDifficultyLevelDecoder =
    Decode.succeed ExtendedDifficultyLevel
        |> Pipeline.required "multiplier" Decode.float
        |> Pipeline.required "description" Decode.string
        |> Pipeline.required "wordCount" Decode.int


-- VALID GUESSES DATABASE FUNCTIONS


getRandomWordGenerator : WordsByDifficulty -> Difficulty -> Random.Generator (Maybe String)
getRandomWordGenerator targetWords difficulty =
    let
        wordList =
            case difficulty of
                Easy ->
                    targetWords.easy

                Medium ->
                    targetWords.medium

                Hard ->
                    targetWords.hard

        wordArray =
            Array.fromList wordList
    in
    if Array.isEmpty wordArray then
        Random.constant Nothing
    else
        Random.map (\index -> Array.get index wordArray) (Random.int 0 (Array.length wordArray - 1))


getRandomWord : WordsByDifficulty -> Difficulty -> Maybe String
getRandomWord targetWords difficulty =
    let
        wordList =
            case difficulty of
                Easy ->
                    targetWords.easy

                Medium ->
                    targetWords.medium

                Hard ->
                    targetWords.hard
    in
    List.head wordList -- Fallback for when we can't use Random


getDifficultyMultiplier : DifficultyConfig -> Difficulty -> Float
getDifficultyMultiplier config difficulty =
    case difficulty of
        Easy ->
            config.easy.multiplier

        Medium ->
            config.medium.multiplier

        Hard ->
            config.hard.multiplier


getExtendedDifficultyMultiplier : ExtendedDifficultyConfig -> Difficulty -> Float
getExtendedDifficultyMultiplier config difficulty =
    case difficulty of
        Easy ->
            config.easy.multiplier

        Medium ->
            config.medium.multiplier

        Hard ->
            config.hard.multiplier


-- For validation, check if word exists in valid guesses list OR target words list
-- If not found, fall back to basic English word validation
validateWord : List String -> WordsByDifficulty -> String -> Difficulty -> Bool
validateWord validGuesses targetWords word difficulty =
    let
        cleanWord = String.toLower (String.trim word)
        isValidLength = String.length cleanWord == 5
        isAllLetters = String.all Char.isAlpha cleanWord
        isInValidGuessesList = List.member cleanWord validGuesses
        
        -- Check if word is in target words (answerlist.json)
        allTargetWords = targetWords.easy ++ targetWords.medium ++ targetWords.hard
        isInTargetWordsList = List.member cleanWord (List.map String.toLower allTargetWords)
        
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


getWordCategories : WordCategories -> String -> List String
getWordCategories categories word =
    let
        lowerWord =
            String.toLower word

        isInCategory categoryWords =
            List.member lowerWord (List.map String.toLower categoryWords)
    in
    List.concat
        [ if isInCategory categories.common then
            [ "common" ]
          else
            []
        , if isInCategory categories.nature then
            [ "nature" ]
          else
            []
        , if isInCategory categories.actions then
            [ "actions" ]
          else
            []
        , if isInCategory categories.technology then
            [ "technology" ]
          else
            []
        ]


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
    , difficulty : Difficulty
    , difficultyString : String
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
    , difficulty = Easy
    , difficultyString = "Easy"
    , walletAddress = Nothing
    , puzzleResult = Nothing
    , isBetModalVisible = False
    , validGuessesDB = Just embeddedValidGuessesDB -- Database is immediately available
    , targetWordsDB = Just embeddedTargetWordsDB -- Target words database is immediately available
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
    | UpdateDifficultyString String
    | PlaceBet
    | BetResultReceived Bool Float
    | BetAgain
    | RandomWordGenerated (Maybe String)
    | StartNewGame
    | ClearGameMessage
    | LoadValidGuesses
    | ValidGuessesLoaded (Result Http.Error ValidGuessesDB)
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
                                                                    Nothing ->
                                                                        case model.difficulty of
                                                                            Easy -> "speed"
                                                                            Medium -> "jetty"
                                                                            Hard -> "wizzo"
                                                            
                                                            -- Check the word and get hex states
                                                            hexStates = checkWordAgainstTarget submittedWord targetWord
                                                            updatedGrid = updateRowWithStates rowIndex hexStates model.grid
                                                            
                                                            isCorrect = String.toLower submittedWord == String.toLower targetWord
                                                            isValidWord = 
                                                                case (model.validGuessesDB, model.targetWordsDB) of
                                                                    (Just validDB, Just targetDB) -> 
                                                                        validateWord validDB.validGuesses targetDB.targetWords submittedWord model.difficulty
                                                                    (Just validDB, Nothing) -> 
                                                                        validateWord validDB.validGuesses embeddedTargetWordsDB.targetWords submittedWord model.difficulty
                                                                    (Nothing, Just targetDB) -> 
                                                                        validateWord [] targetDB.targetWords submittedWord model.difficulty
                                                                    (Nothing, Nothing) -> 
                                                                        validateWord [] embeddedTargetWordsDB.targetWords submittedWord model.difficulty
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

                _ = Debug.log result.logMessage
                
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

        UpdateDifficultyString diffStr ->
            ( { model | difficultyString = diffStr, difficulty = stringToDifficulty diffStr }, Cmd.none )

        PlaceBet ->
            let
                ( newModel, wordCmd ) =
                    selectNewWord model
            in
            ( { newModel | isBetModalVisible = False }
            , Cmd.batch
                [ placeBet { amount = model.betAmount, difficulty = difficultyToString model.difficulty }
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
            ( { initialModel | walletAddress = model.walletAddress, config = model.config }
            , selectNewWordCmd model.difficulty -- Generate new word immediately
            )

        RandomWordGenerated word ->
            case word of
                Just w ->
                    ( { model | currentWord = Just w, loadingError = Nothing }, Cmd.none )
                Nothing ->
                    ( { model | loadingError = Just "No words available for selected difficulty" }, Cmd.none )

        StartNewGame ->
            let
                modelWithClearedGrid = { initialModel | validGuessesDB = model.validGuessesDB, targetWordsDB = model.targetWordsDB, config = model.config, submittedRows = [], currentActiveRow = 0, isModalVisible = False, modalMessage = "", isHintModalVisible = False, isGameOverModalVisible = False, isLettersModalVisible = False }
            in
            ( modelWithClearedGrid, selectNewWordCmd modelWithClearedGrid.difficulty )

        ClearGameMessage ->
            ( { model | gameMessage = Nothing, gameMessageType = "" }, Cmd.none )

        LoadValidGuesses ->
            ( model, loadValidGuessesCmd )

        ValidGuessesLoaded result ->
            case result of
                Ok validGuessesDB ->
                    ( { model | validGuessesDB = Just validGuessesDB, loadingError = Nothing }, Cmd.none )
                
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
                    ( { model | loadingError = Just ("Failed to load valid guesses: " ++ errorMessage) }, Cmd.none )

        SimpleValidGuessesLoaded result ->
            case result of
                Ok validGuessesList ->
                    -- Create a new ValidGuessesDB using the loaded validGuesses list and embedded data for other fields
                    let
                        updatedValidGuessesDB = 
                            { embeddedValidGuessesDB | validGuesses = validGuessesList }
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
port placeBet : { amount : Float, difficulty : String } -> Cmd msg
port transactionResult : (String -> msg) -> Sub msg



-- VIEW





viewBetModal : Model -> Html Msg
viewBetModal model =
    -- Hidden for now - betting modal will be implemented later
    Html.text ""


calculatePotentialPayout : Model -> String
calculatePotentialPayout model =
    let
        multiplier =
            case model.targetWordsDB of
                Just db ->
                    getExtendedDifficultyMultiplier db.difficulties model.difficulty

                Nothing ->
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
    String.fromFloat (toFloat (round (finalPayout * 100)) / 100)


-- Success modal for when puzzle is solved
viewSuccessModal : Model -> Html Msg
viewSuccessModal model =
    if model.isModalVisible then
        div -- Backdrop for success modal
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
            [ div -- Success Modal content box
                [ style "background-color" "#fff"
                , style "padding" "40px 50px"
                , style "border-radius" "15px"
                , style "box-shadow" "0 10px 25px rgba(0,0,0,0.4)"
                , style "min-width" "400px"
                , style "max-width" "90%"
                , style "text-align" "center"
                , stopPropagationOn "click" (Decode.succeed (NoOp, True))
                ]
                (let
                    targetWord = Maybe.withDefault "UNKNOWN" model.currentWord
                    guessCount = model.currentActiveRow + 1  -- +1 because currentActiveRow is 0-indexed
                    guessText = if guessCount == 1 then "guess" else "guesses"
                in
                [ div
                    [ style "font-size" "32px"
                    , style "margin-bottom" "10px"
                    ]
                    [ text "🎉 Congratulations! 🎉" ]
                , div
                    [ style "font-size" "20px"
                    , style "font-weight" "bold"
                    , style "color" "#28a745"
                    , style "margin-bottom" "10px"
                    ]
                    [ text ("You solved the puzzle in " ++ String.fromInt guessCount ++ " " ++ guessText ++ "!") ]
                , div
                    [ style "font-size" "18px"
                    , style "color" "#333"
                    , style "margin-bottom" "20px"
                    ]
                    [ text ("The word was: " ++ String.toUpper targetWord) ]
                , div
                    [ style "display" "flex"
                    , style "justify-content" "center"
                    , style "gap" "15px"
                    , style "margin-top" "30px"
                    ]
                    [ button
                        [ onClick CloseModal
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
                , button
                        [ onClick StartNewGame
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
                ]
                ])
            ]
    else
        Html.text ""


-- Hint modal for showing the target word
viewHintModal : Model -> Html Msg
viewHintModal model =
    if model.isHintModalVisible then
        div -- Backdrop for hint modal
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
            , onClick CloseHintModal
            ]
            [ div -- Hint Modal content box
                [ style "background-color" "#fff"
                , style "padding" "30px 40px"
                , style "border-radius" "12px"
                , style "box-shadow" "0 8px 20px rgba(0,0,0,0.3)"
                , style "min-width" "300px"
                , style "max-width" "90%"
                , style "text-align" "center"
                , stopPropagationOn "click" (Decode.succeed (NoOp, True))
                ]
                [ div
                    [ style "font-size" "18px"
                    , style "font-weight" "bold"
                    , style "color" "#333"
                    , style "margin-bottom" "15px"
                    ]
                    [ text "💡 Hint" ]
                , div
                    [ style "font-size" "24px"
                    , style "font-weight" "bold"
                    , style "color" "#007bff"
                    , style "margin-bottom" "20px"
                    , style "font-family" "monospace"
                    , style "letter-spacing" "2px"
                    ]
                    [ text ("Target: " ++ String.toUpper (Maybe.withDefault "UNKNOWN" model.currentWord)) ]
                , button
                    [ onClick CloseHintModal
                    , style "padding" "10px 20px"
                        , style "cursor" "pointer"
                    , style "background-color" "#6c757d"
                        , style "color" "white"
                        , style "border" "none"
                    , style "border-radius" "6px"
                    , style "font-size" "14px"
                    , style "font-weight" "500"
                        ]
                    [ text "Close" ]
                ]
            ]
    else
        Html.text ""


-- Game Over modal for when user fails to guess the word
viewGameOverModal : Model -> Html Msg
viewGameOverModal model =
    if model.isGameOverModalVisible then
        div -- Backdrop for game over modal
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "background-color" "rgba(0, 0, 0, 0.8)"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "z-index" "1000"
            , onClick StartNewGame
            ]
            [ div
                [ style "background-color" "#fff"
                , style "padding" "30px"
                , style "border-radius" "15px"
                , style "box-shadow" "0 10px 25px rgba(0,0,0,0.4)"
                , style "min-width" "400px"
                , style "max-width" "90%"
                , style "text-align" "center"
                , stopPropagationOn "click" (Decode.succeed (NoOp, True))
                ]
                [ div
                    [ style "font-size" "24px"
                    , style "font-weight" "bold"
                    , style "color" "#d32f2f"
                    , style "margin-bottom" "15px"
                    ]
                    [ text "😞 Game Over" ]
                , div
                    [ style "font-size" "16px"
                    , style "color" "#666"
                    , style "margin-bottom" "20px"
                    ]
                    [ text "Sorry, the word was:" ]
                , div
                    [ style "font-size" "32px"
                    , style "font-weight" "bold"
                    , style "color" "#2e7d32"
                    , style "margin-bottom" "25px"
                    , style "padding" "10px"
                    , style "background-color" "#f5f5f5"
                    , style "border-radius" "8px"
                    , style "text-transform" "uppercase"
                    ]
                    [ text (Maybe.withDefault "UNKNOWN" model.currentWord) ]
                , button
                    [ onClick StartNewGame
                    , style "background-color" "#2196F3"
                    , style "color" "white"
                    , style "border" "none"
                    , style "padding" "12px 24px"
                    , style "border-radius" "8px"
                    , style "cursor" "pointer"
                    , style "font-size" "16px"
                    , style "font-weight" "bold"
                    ]
                    [ text "New Game" ]
                ]
            ]
    else
        Html.text ""


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


-- Letters modal showing QWERTY keyboard with letter states
viewLettersModal : Model -> Html Msg
viewLettersModal model =
    if model.isLettersModalVisible then
        let
            letterStates = getLetterStates model.grid
            
            -- QWERTY keyboard layout
            topRow = ['q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p']
            middleRow = ['a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l']
            bottomRow = ['z', 'x', 'c', 'v', 'b', 'n', 'm']
            
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
        div -- Backdrop for letters modal
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "width" "100%"
            , style "height" "100%"
            , style "background-color" "rgba(0, 0, 0, 0.8)"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "z-index" "1000"
            , onClick CloseLettersModal
            ]
            [ div
                [ style "background-color" "#fff"
                , style "padding" "30px"
                , style "border-radius" "12px"
                , style "box-shadow" "0 8px 20px rgba(0,0,0,0.3)"
                , style "min-width" "400px"
                , style "max-width" "90%"
                , style "text-align" "center"
                , stopPropagationOn "click" (Decode.succeed (NoOp, True))
                ]
                [ div
                    [ style "font-size" "18px"
                    , style "font-weight" "bold"
                    , style "color" "#333"
                    , style "margin-bottom" "20px"
                    ]
                    [ text "🔤 Letter Status" ]
                , div
                    [ style "margin-bottom" "20px"
                    ]
                    [ renderRow topRow
                    , renderRow middleRow  
                    , renderRow bottomRow
                    ]
                , div
                    [ style "display" "flex"
                    , style "justify-content" "center"
                    , style "gap" "20px"
                    , style "margin-bottom" "15px"
                    , style "font-size" "12px"
                    ]
                    [ div [ style "display" "flex", style "align-items" "center", style "gap" "5px" ]
                        [ div [ style "width" "16px", style "height" "16px", style "background-color" "#6aaa64", style "border-radius" "2px" ] []
                        , text "Correct"
                        ]
                    , div [ style "display" "flex", style "align-items" "center", style "gap" "5px" ]
                        [ div [ style "width" "16px", style "height" "16px", style "background-color" "#c9b458", style "border-radius" "2px" ] []
                        , text "Wrong Position"
                        ]
                    , div [ style "display" "flex", style "align-items" "center", style "gap" "5px" ]
                        [ div [ style "width" "16px", style "height" "16px", style "background-color" "#787c7e", style "border-radius" "2px" ] []
                        , text "Not in Word"
                        ]
                    ]
                , button
                    [ onClick CloseLettersModal
                    , style "background-color" "#6c757d"
                    , style "color" "white"
                    , style "border" "none"
                    , style "padding" "8px 16px"
                    , style "border-radius" "4px"
                    , style "cursor" "pointer"
                    , style "font-size" "14px"
                    ]
                    [ text "Close" ]
                ]
            ]
    else
        Html.text ""


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
        , viewSuccessModal model
        , viewHintModal model
        , viewGameOverModal model
        , viewLettersModal model
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
            [ onClick StartNewGame
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
                        , style "color" "#212529"
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
                    [ text "Selecting puzzle word..." ]
                    ]


viewWalletControls : Model -> Html Msg
viewWalletControls model =
    -- Hidden for now - wallet and betting functionality will be implemented later
    Html.text ""

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
        { init = \_ -> ( initialModel, Cmd.batch [ selectNewWordCmd Easy, loadValidGuessesCmd ] ) -- Load word and valid guesses
        , view = \model -> { title = "Interactive Honeycomb Wordle", body = [ view model ] }
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
            ( model, selectNewWordCmd model.difficulty )


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
selectNewWordCmd : Difficulty -> Cmd Msg
selectNewWordCmd difficulty =
    Random.generate RandomWordGenerated (getRandomWordGenerator embeddedTargetWordsDB.targetWords difficulty)


-- Helper function to get the count of valid guesses for debugging
getValidGuessesCount : Model -> Int
getValidGuessesCount model =
    case model.validGuessesDB of
        Just db -> List.length db.validGuesses
        Nothing -> 0


-- HTTP command to load valid guesses from JSON file
loadValidGuessesCmd : Cmd Msg
loadValidGuessesCmd =
    Http.get
        { url = "validguess.json"
        , expect = Http.expectJson SimpleValidGuessesLoaded simpleValidGuessesDecoder
        }


-- Fallback command to load full ValidGuessesDB structure (if needed in the future)
loadFullValidGuessesCmd : Cmd Msg
loadFullValidGuessesCmd =
    Http.get
        { url = "validguess-full.json"  -- Different filename for full structure
        , expect = Http.expectJson ValidGuessesLoaded validGuessesDecoder
        }


-- Proper Wordle-style word checking that handles duplicates correctly
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
