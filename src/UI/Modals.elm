module UI.Modals exposing 
    ( successModal
    , gameOverModal
    , hintModal
    , lettersModal
    )

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Dict
import Game.Model exposing (..)


-- SUCCESS MODAL


successModal : Model -> Html Msg
successModal model =
    let
        targetWord = Maybe.withDefault "UNKNOWN" model.gameData.currentWord
        guessCount = 
            case model.gameData.gameState of
                Won count -> count
                _ -> 1
        guessText = if guessCount == 1 then "guess" else "guesses"
    in
    modalBackdrop CloseSuccessModal
        [ modalContent
            [ congratulationsHeader
            , successMessage guessCount guessText
            , targetWordDisplay targetWord
            , modalButtons
                [ closeButton CloseSuccessModal
                , newGameButton
                ]
            ]
        ]


-- GAME OVER MODAL


gameOverModal : Model -> Html Msg
gameOverModal model =
    let
        targetWord = 
            case model.gameData.gameState of
                Lost word -> word
                _ -> Maybe.withDefault "UNKNOWN" model.gameData.currentWord
    in
    modalBackdrop StartNewGame
        [ modalContent
            [ gameOverHeader
            , gameOverMessage
            , targetWordDisplay targetWord
            , modalButtons [ newGameButton ]
            ]
        ]


-- HINT MODAL


hintModal : Model -> Html Msg
hintModal model =
    let
        targetWord = Maybe.withDefault "UNKNOWN" model.gameData.currentWord
    in
    modalBackdrop CloseHintModal
        [ modalContent
            [ hintHeader
            , hintDisplay targetWord
            , modalButtons [ closeButton CloseHintModal ]
            ]
        ]


-- LETTERS MODAL


lettersModal : Model -> Html Msg
lettersModal model =
    modalBackdrop CloseLettersModal
        [ modalContent
            [ lettersHeader
            , keyboardDisplay model.gameData.letterStates
            , legendDisplay
            , modalButtons [ closeButton CloseLettersModal ]
            ]
        ]


-- MODAL BUILDING BLOCKS


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


-- HEADERS


congratulationsHeader : Html Msg
congratulationsHeader =
    div
        [ style "font-size" "32px"
        , style "margin-bottom" "10px"
        ]
        [ text "🎉 Congratulations! 🎉" ]


gameOverHeader : Html Msg
gameOverHeader =
    div
        [ style "font-size" "24px"
        , style "font-weight" "bold"
        , style "color" "#d32f2f"
        , style "margin-bottom" "15px"
        ]
        [ text "😞 Game Over" ]


hintHeader : Html Msg
hintHeader =
    div
        [ style "font-size" "18px"
        , style "font-weight" "bold"
        , style "color" "#333"
        , style "margin-bottom" "15px"
        ]
        [ text "💡 Hint" ]


lettersHeader : Html Msg
lettersHeader =
    div
        [ style "font-size" "18px"
        , style "font-weight" "bold"
        , style "color" "#333"
        , style "margin-bottom" "20px"
        ]
        [ text "🔤 Letter Status" ]


-- MESSAGES


successMessage : Int -> String -> Html Msg
successMessage guessCount guessText =
    div
        [ style "font-size" "20px"
        , style "font-weight" "bold"
        , style "color" "#28a745"
        , style "margin-bottom" "10px"
        ]
        [ text ("You solved the puzzle in " ++ String.fromInt guessCount ++ " " ++ guessText ++ "!") ]


gameOverMessage : Html Msg
gameOverMessage =
    div
        [ style "font-size" "16px"
        , style "color" "#666"
        , style "margin-bottom" "20px"
        ]
        [ text "Sorry, the word was:" ]


-- DISPLAYS


targetWordDisplay : String -> Html Msg
targetWordDisplay word =
    div
        [ style "font-size" "32px"
        , style "font-weight" "bold"
        , style "color" "#2e7d32"
        , style "margin-bottom" "25px"
        , style "padding" "10px"
        , style "background-color" "#f5f5f5"
        , style "border-radius" "8px"
        , style "text-transform" "uppercase"
        ]
        [ text word ]


hintDisplay : String -> Html Msg
hintDisplay word =
    div
        [ style "font-size" "24px"
        , style "font-weight" "bold"
        , style "color" "#007bff"
        , style "margin-bottom" "20px"
        , style "font-family" "monospace"
        , style "letter-spacing" "2px"
        ]
        [ text ("Target: " ++ String.toUpper word) ]


keyboardDisplay : Dict.Dict Char HexState -> Html Msg
keyboardDisplay letterStates =
    let
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
    div
        [ style "margin-bottom" "20px" ]
        [ renderRow topRow
        , renderRow middleRow  
        , renderRow bottomRow
        ]


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


-- BUTTONS


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