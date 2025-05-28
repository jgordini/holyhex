module Game.Word exposing 
    ( randomWordGenerator
    , checkWordAgainstTarget
    , getWordFromGrid
    , LetterState(..)
    )

import Array exposing (Array)
import Random
import Game.Model exposing (HexagonData, HexState(..), Grid, HexId(..))


-- TYPES


type LetterState
    = CorrectPosition
    | WrongPosition  
    | NotInWord


-- WORD GENERATION


randomWordGenerator : List String -> Random.Generator (Maybe String)
randomWordGenerator targetWords =
    let
        wordArray = Array.fromList targetWords
    in
    if Array.isEmpty wordArray then
        Random.constant Nothing
    else
        Random.map (\index -> Array.get index wordArray) 
            (Random.int 0 (Array.length wordArray - 1))


-- WORD EXTRACTION


getWordFromGrid : Grid -> Int -> Maybe String
getWordFromGrid grid rowIndex =
    case Array.get rowIndex grid of
        Nothing -> Nothing
        Just row ->
            let
                letters = Array.toList row
                    |> List.filterMap .letter
            in
            if List.length letters == 5 then
                Just (String.fromList letters)
            else
                Nothing


-- WORD CHECKING ALGORITHM


{-| Implements proper Wordle logic for checking words against targets.
    Handles duplicate letters correctly with a two-pass algorithm.
-}
checkWordAgainstTarget : String -> String -> List HexState
checkWordAgainstTarget guess target =
    let
        guessChars = String.toList (String.toLower guess)
        targetChars = String.toList (String.toLower target)
        
        -- First pass: mark exact matches (green/Correct)
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
        
        -- Second pass: mark present letters (yellow/Present) for non-exact matches
        (finalStates, _) = 
            List.indexedMap (\i guessChar ->
                if List.member i exactMatches then
                    (Correct, remainingTargetChars)
                else
                    case List.filter (\c -> c == guessChar) remainingTargetChars of
                        [] -> (Absent, remainingTargetChars)
                        _ :: _ -> 
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


-- HELPER FUNCTIONS


removeFirstOccurrence : Char -> List Char -> List Char
removeFirstOccurrence target chars =
    case chars of
        [] -> []
        first :: rest ->
            if first == target then
                rest
            else
                first :: removeFirstOccurrence target rest