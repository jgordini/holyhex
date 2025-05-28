module Game.Database exposing 
    ( WordDatabase
    , empty
    , fallbackTargetWords
    , loadWordDatabase
    , validateWord
    , ValidationError(..)
    )

import Set exposing (Set)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


-- TYPES


type ValidationError
    = TooShort
    | TooLong
    | InvalidChars
    | NotInDictionary
    | RepeatingPattern
    | NoVowels


type alias WordDatabase =
    { validGuesses : Set String
    , targetWords : Set String
    , version : String
    , lastUpdated : String
    }


-- CONSTANTS


fallbackTargetWords : List String
fallbackTargetWords = 
    [ "speed", "house", "world", "music", "ocean", "beach", "plant", "water", "light", "sound"
    , "chair", "table", "phone", "paper", "money", "heart", "smile", "dance", "dream", "peace"
    , "bread", "apple", "river", "stone", "cloud", "grass", "flame", "storm", "quiet", "brave"
    ]


-- INITIALIZATION


empty : WordDatabase
empty =
    { validGuesses = Set.empty
    , targetWords = Set.fromList fallbackTargetWords
    , version = "fallback"
    , lastUpdated = "2024-01-01"
    }


-- HTTP LOADING


loadWordDatabase : (Result Http.Error WordDatabase -> msg) -> Cmd msg
loadWordDatabase toMsg =
    Http.get
        { url = "validguess.json"
        , expect = Http.expectJson toMsg wordDatabaseDecoder
        }


-- JSON DECODERS


wordDatabaseDecoder : Decoder WordDatabase
wordDatabaseDecoder =
    Decode.succeed WordDatabase
        |> Pipeline.required "validGuesses" (Decode.map Set.fromList (Decode.list Decode.string))
        |> Pipeline.optional "targetWords" (Decode.map Set.fromList (Decode.list Decode.string)) (Set.fromList fallbackTargetWords)
        |> Pipeline.optional "version" Decode.string "1.0"
        |> Pipeline.optional "lastUpdated" Decode.string "unknown"


-- VALIDATION


validateWord : WordDatabase -> String -> Result ValidationError String
validateWord database word =
    let
        cleanWord = String.toLower (String.trim word)
        wordLength = String.length cleanWord
    in
    if wordLength < 5 then
        Err TooShort
    else if wordLength > 5 then
        Err TooLong
    else if not (String.all Char.isAlpha cleanWord) then
        Err InvalidChars
    else if isRepeatingPattern cleanWord then
        Err RepeatingPattern
    else if hasNoVowels cleanWord then
        Err NoVowels
    else if not (isInDatabase database cleanWord) && not (isReasonableWord cleanWord) then
        Err NotInDictionary
    else
        Ok cleanWord


isInDatabase : WordDatabase -> String -> Bool
isInDatabase database word =
    Set.member word database.validGuesses || Set.member word database.targetWords


isReasonableWord : String -> Bool
isReasonableWord word =
    not (isRepeatingPattern word) && 
    not (hasNoVowels word) && 
    not (hasTooManyConsecutiveConsonants word 4) &&
    not (hasTooManyConsecutiveVowels word 3)


-- WORD VALIDATION HELPERS


isRepeatingPattern : String -> Bool
isRepeatingPattern word =
    let
        chars = String.toList word
        firstChar = List.head chars
    in
    case firstChar of
        Nothing -> False
        Just char -> List.all (\c -> c == char) chars


hasNoVowels : String -> Bool
hasNoVowels word =
    let
        vowels = Set.fromList ['a', 'e', 'i', 'o', 'u', 'y']
        chars = String.toList (String.toLower word)
        vowelCount = List.length (List.filter (\c -> Set.member c vowels) chars)
    in
    vowelCount == 0


hasTooManyConsecutiveConsonants : String -> Int -> Bool
hasTooManyConsecutiveConsonants word maxCount =
    let
        vowels = Set.fromList ['a', 'e', 'i', 'o', 'u', 'y']
        chars = String.toList (String.toLower word)
        
        checkConsecutive remainingChars currentCount =
            case remainingChars of
                [] -> currentCount >= maxCount
                char :: rest ->
                    if Set.member char vowels then
                        checkConsecutive rest 0
                    else
                        let newCount = currentCount + 1
                        in
                        if newCount >= maxCount then
                            True
                        else
                            checkConsecutive rest newCount
    in
    checkConsecutive chars 0


hasTooManyConsecutiveVowels : String -> Int -> Bool
hasTooManyConsecutiveVowels word maxCount =
    let
        vowels = Set.fromList ['a', 'e', 'i', 'o', 'u', 'y']
        chars = String.toList (String.toLower word)
        
        checkConsecutive remainingChars currentCount =
            case remainingChars of
                [] -> currentCount >= maxCount
                char :: rest ->
                    if Set.member char vowels then
                        let newCount = currentCount + 1
                        in
                        if newCount >= maxCount then
                            True
                        else
                            checkConsecutive rest newCount
                    else
                        checkConsecutive rest 0
    in
    checkConsecutive chars 0