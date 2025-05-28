module UI.Hexagon exposing (view)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Set
import Game.Model exposing (..)


-- HEXAGON VIEW


view : Model -> HexagonData -> Html Msg
view model hexData =
    let
        cfg = model.config
        isHovered = model.uiState.hoveredHexId == Just hexData.id
        isFocused = model.uiState.focusedHexId == Just hexData.id
        
        -- Check if this hex is in a submitted row or not the active row
        (isInSubmittedRow, isInActiveRow) = getHexRowStatus model hexData.id
        
        (fillColor, textColor) = getHexColors cfg hexData.state
        outlineColor = if isFocused then cfg.colorOutlineFocused else cfg.colorOutline
        
        outerHexStyles = buildOuterHexStyles cfg isHovered isFocused isInActiveRow isInSubmittedRow outlineColor
        hoverStyles = buildHoverStyles cfg isHovered isFocused isInActiveRow
        innerFillStyles = buildInnerFillStyles cfg fillColor
        letterStyles = buildLetterStyles cfg textColor
        
        letterContent = 
            case hexData.letter of
                Just l -> String.fromChar l
                Nothing -> ""
    in
    div
        (outerHexStyles
            ++ hoverStyles
            ++ [ onMouseEnter (MouseEnterHex hexData.id)
               , onMouseLeave (MouseLeaveHex hexData.id)
               , onClick (FocusHex hexData.id)
               ]
        )
        [ div innerFillStyles []
        , span letterStyles [ text letterContent ]
        ]


-- HELPER FUNCTIONS


getHexRowStatus : Model -> HexId -> (Bool, Bool)
getHexRowStatus model hexId =
    case hexId of
        HexId rowIndex _ -> 
            let
                isInSubmittedRow = Set.member rowIndex model.gameData.submittedRows
                isInActiveRow = rowIndex == model.gameData.currentActiveRow
            in
            (isInSubmittedRow, isInActiveRow)


getHexColors : Config -> HexState -> (String, String)
getHexColors cfg state =
    case state of
        Empty ->
            (cfg.colorFillDefault, cfg.colorTextDefault)
        Correct ->
            (cfg.colorFillCorrect, cfg.colorTextFilled)
        Present ->
            (cfg.colorFillPresent, cfg.colorTextFilled)
        Absent ->
            (cfg.colorFillAbsent, cfg.colorTextFilled)


buildOuterHexStyles : Config -> Bool -> Bool -> Bool -> Bool -> String -> List (Html.Attribute Msg)
buildOuterHexStyles cfg isHovered isFocused isInActiveRow isInSubmittedRow outlineColor =
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
    , style "transition" (buildTransition cfg isHovered)
    , style "z-index" (if isHovered then "10" else "1")
    , style "opacity" (getHexOpacity isInSubmittedRow isInActiveRow)
    ]


buildHoverStyles : Config -> Bool -> Bool -> Bool -> List (Html.Attribute Msg)
buildHoverStyles cfg isHovered isFocused isInActiveRow =
    if not isInActiveRow then
        []
    else if isHovered && not isFocused then
        [ style "filter" (buildShadowFilter cfg)
        , style "transform" ("scale(" ++ String.fromFloat cfg.hoverScale ++ ")")
        ]
    else if isFocused then
        [ style "transform" ("scale(" ++ String.fromFloat (cfg.hoverScale * 0.98) ++ ")") ]
    else
        []


buildInnerFillStyles : Config -> String -> List (Html.Attribute Msg)
buildInnerFillStyles cfg fillColor =
    [ style "position" "absolute"
    , style "top" "0"
    , style "left" "0"
    , style "width" "100%"
    , style "height" "100%"
    , style "background-color" fillColor
    , style "clip-path" "inherit"
    , style "transform" ("scale(" ++ String.fromFloat cfg.hexFillScale ++ ")")
    , style "z-index" "0"
    , style "box-sizing" "border-box"
    ]


buildLetterStyles : Config -> String -> List (Html.Attribute Msg)
buildLetterStyles cfg textColor =
    [ style "font-size" (String.fromFloat cfg.letterFontSize ++ "px")
    , style "font-weight" "bold"
    , style "color" textColor
    , style "text-transform" "uppercase"
    , style "position" "relative"
    , style "z-index" "1"
    , style "user-select" "none"
    , style "line-height" "1"
    ]


buildTransition : Config -> Bool -> String
buildTransition cfg isHovered =
    let
        transitionMs = String.fromInt cfg.transitionSpeedMs ++ "ms"
        zIndexDelay = if isHovered then "" else " " ++ transitionMs
    in
    "filter " ++ transitionMs ++ " ease-out, " ++
    "transform " ++ transitionMs ++ " ease-out, " ++
    "background-color " ++ transitionMs ++ " ease-out, " ++
    "z-index 0s" ++ zIndexDelay


buildShadowFilter : Config -> String
buildShadowFilter cfg =
    "drop-shadow(" ++
    String.fromInt cfg.hoverShadowX ++ "px " ++
    String.fromInt cfg.hoverShadowY ++ "px " ++
    String.fromInt cfg.hoverShadowBlur ++ "px " ++
    cfg.hoverShadowColor ++ ")"


getHexOpacity : Bool -> Bool -> String
getHexOpacity isInSubmittedRow isInActiveRow =
    if isInSubmittedRow then
        "0.8"
    else if not isInActiveRow then
        "0.5"
    else
        "1.0"