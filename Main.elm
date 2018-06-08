module Main exposing (..)

import Html exposing (Html, Attribute, table, tr, th, td, button, input, text)
import Html.Attributes exposing (class, colspan, type_, checked)
import Html.Events exposing (onClick, onWithOptions)
import Dict exposing (Dict)
import Json.Decode as Json
import List


type alias Quantity =
    Int


type alias Capacity =
    Int


type alias LabelType =
    String


type alias Selected =
    Bool


type alias LabelData =
    { labelType : LabelType
    , quantity : Quantity
    , capacity : Capacity
    , selected : Selected
    }


type alias Model =
    List LabelData


type Msg
    = Add LabelData Quantity
    | CheckboxToggled LabelData


target : Quantity
target =
    4928


addNatural : Int -> Int -> Int
addNatural a b =
    -- add natural numbers, i.e. not below zero.
    let
        total =
            a + b
    in
        if total < 0 then
            0
        else
            total


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault message =
    let
        config =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "click" config (Json.succeed message)


getTotalNumberSelected : Model -> Int
getTotalNumberSelected model =
    List.foldl
        (\labelData accumulator ->
            if labelData.selected then
                accumulator + 1
            else
                accumulator
        )
        0
        model


model : Model
model =
    [ { quantity = 0, capacity = 16, selected = False, labelType = "Herma" }
    , { quantity = 0, capacity = 16, selected = True, labelType = "Avery" }
    , { quantity = 0, capacity = 24, selected = True, labelType = "OfficeDepot" }
    , { quantity = 0, capacity = 20, selected = False, labelType = "Supermagnete" }
    ]


init : ( Model, Cmd msg )
init =
    ( model, Cmd.none )


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th [] [ text "sel" ]
        , th [] [ text "labelsoort" ]
        , th [ colspan 3 ] [ text "aantal vel" ]
        , th [] [ text "labels per vel" ]
        , th [] [ text "totaal # labels" ]
        ]


viewTableFooter : Model -> Html Msg
viewTableFooter model =
    let
        totalQuantity =
            List.map .quantity model
                -- foldl : (a -> b -> b) -> b -> List a -> b
                |> List.foldl (+) 0

        totalLabels =
            List.map (\labelData -> labelData.quantity * labelData.capacity) model
                -- foldl : (a -> b -> b) -> b -> List a -> b
                |> List.foldl (+) 0
    in
        tr [ class "footer" ]
            [ td [ class "transparent centered" ] [ text <| toString (getTotalNumberSelected model) ]
            , td [] []
            , td [] []
            , td [ class "number quantity" ] [ text <| toString totalQuantity ]
            , td [] []
            , td [] []
            , td [ class "number" ] [ text <| toString totalLabels ]
            ]


viewTableRow : LabelData -> Html Msg
viewTableRow labelData =
    tr []
        [ td [ class "centered" ] <|
            [ input
                ([ type_ "checkbox"
                 , onClickPreventDefault (CheckboxToggled labelData)
                 , checked labelData.selected
                 ]
                )
                []
            ]
        , td [] [ text labelData.labelType ]
        , td []
            [ button [ onClick (Add labelData -10) ] [ text "-10" ]
            , button [ onClick (Add labelData -1) ] [ text "-1" ]
            ]
        , td [ class "number quantity" ] [ text <| toString labelData.quantity ]
        , td []
            [ button [ onClick (Add labelData 1) ] [ text "+1" ]
            , button [ onClick (Add labelData 10) ] [ text "+10" ]
            ]
        , td [ class "number" ] [ text <| toString labelData.capacity ]
        , td [ class "number" ] [ text <| toString <| labelData.quantity * labelData.capacity ]
        ]


viewTableRows : Model -> List (Html Msg)
viewTableRows model =
    List.map viewTableRow model


view : Model -> Html Msg
view model =
    table [] <|
        [ viewTableHeader ]
            ++ viewTableRows model
            ++ [ viewTableFooter model ]


updateModelQuantity : Model -> LabelData -> Quantity -> Model
updateModelQuantity model labelData delta =
    List.map
        (\record ->
            if labelData.labelType == record.labelType then
                { record | quantity = addNatural delta record.quantity }
            else
                record
        )
        model


updateModelSelected : Model -> LabelData -> Model
updateModelSelected model labelData =
    let
        totalNrSelected =
            getTotalNumberSelected model
    in
        List.map
            (\record ->
                if labelData.labelType == record.labelType then
                    { record | selected = not record.selected && totalNrSelected < 2 }
                else
                    record
            )
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Add labelData num ->
                    updateModelQuantity model labelData num

                CheckboxToggled labelData ->
                    updateModelSelected model labelData
    in
        ( newModel, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
