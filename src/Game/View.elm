module Game.View exposing (view)

import Html exposing (Html, div, h3, p, button, span, text)
import Html.Attributes exposing (class, style, id)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, stopPropagationOn)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode
import Array
import Dict
import Set
import Game.Model exposing (..)
import UI.Hexagon as Hexagon
import UI.Modals as Modals


-- MAIN VIEW


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
        [ viewGameControls model
        , viewGameMessage model
        , viewHoneycombGrid model
        , viewRules
        , viewModals model
        ]


-- GAME CONTROLS


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
        , case model.gameData.currentWord of
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
                    [ text "Loading puzzle..." ]
        ]


-- GAME MESSAGE


viewGameMessage : Model -> Html Msg
viewGameMessage model =
    case model.uiState.gameMessage of
        Nothing ->
            Html.text ""
        
        Just message ->
            let
                (bgColor, textColor, borderColor) =
                    case message of
                        Success _ ->
                            ("#d4edda", "#155724", "#c3e6cb")
                        Error _ ->
                            ("#f8d7da", "#721c24", "#f5c6cb")
                        Info _ ->
                            ("#d1ecf1", "#0c5460", "#bee5eb")
                
                messageText = 
                    case message of
                        Success text -> text
                        Error text -> text
                        Info text -> text
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
                [ text messageText ]


-- HONEYCOMB GRID


viewHoneycombGrid : Model -> Html Msg
viewHoneycombGrid model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        (Array.indexedMap (viewHoneycombRow model) model.gameData.grid
            |> Array.toList)


viewHoneycombRow : Model -> Int -> Array HexagonData -> Html Msg
viewHoneycombRow model rowIndex rowData =
    let
        rowStyle =
            [ style "display" "flex" ]

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
        (Array.map (Hexagon.view model) rowData |> Array.toList)


-- RULES


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
        [ h3 [ style "margin-top" "0", style "color" "#333", style "font-size" "18px" ] 
            [ text "Game Rules" ]
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


-- MODALS


viewModals : Model -> Html Msg
viewModals model =
    div []
        [ if model.uiState.isSuccessModalVisible then
            Modals.successModal model
          else
            Html.text ""
        , if model.uiState.isGameOverModalVisible then
            Modals.gameOverModal model
          else
            Html.text ""
        , if model.uiState.isHintModalVisible then
            Modals.hintModal model
          else
            Html.text ""
        , if model.uiState.isLettersModalVisible then
            Modals.lettersModal model
          else
            Html.text ""
        ]