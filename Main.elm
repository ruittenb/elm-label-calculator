module Main exposing (..)

import Html exposing (Html, Attribute, table, tr, th, td, button, input, text)
import Html.Attributes exposing (class, colspan, type_, checked, value, size, attribute)
import Html.Events exposing (onClick, onWithOptions, onInput)
import Dict exposing (Dict)
import Json.Decode as Json
import List.Extra exposing (getAt)
import List
import Debug


type alias Quantity =
    Int


type alias Capacity =
    Int


type alias LabelType =
    String


type alias LabelData =
    { labelType : LabelType
    , quantity : Quantity
    , capacity : Capacity
    , selected : Bool
    }


type alias Model =
    List LabelData


type Msg
    = Add LabelData Quantity
    | Set LabelData String
    | CheckboxToggled LabelData


target : Quantity
target =
    4928


maxNrSelected : Int
maxNrSelected =
    1


addNatural : Int -> Int -> Int
addNatural a b =
    -- add natural numbers, i.e. not below zero.
    if a + b < 0 then
        0
    else
        a + b


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


getTotalLabels : Model -> Int
getTotalLabels model =
    List.map (\labelData -> labelData.quantity * labelData.capacity) model
        -- foldl : (a -> b -> b) -> b -> List a -> b
        |> List.foldl (+) 0


model : Model
model =
    [ { quantity = 0, capacity = 16, selected = False, labelType = "Herma" }
    , { quantity = 0, capacity = 16, selected = False, labelType = "Avery" }
    , { quantity = 0, capacity = 24, selected = False, labelType = "OfficeDepot" }
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
            getTotalLabels model
    in
        tr [ class "footer" ]
            [ td [ class "transparent centered" ] [ text <| toString (getTotalNumberSelected model) ]
            , td [] []
            , td [] []
            , td [ class "number quantity" ] [ text <| toString totalQuantity ]
            , td [] []
            , td [] []
            , td [ class "number" ] [ text <| (toString totalLabels ++ " / " ++ toString target) ]
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

        --        , td [ class "number quantity" ] [ text <| toString labelData.quantity ]
        , td [ class "number quantity" ]
            [ input
                [ value <| toString labelData.quantity
                , size 3
                , type_ "number"
                , onInput <| Set labelData
                , attribute "min" "0"
                ]
                []
            ]
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


updateAimForTarget : List LabelData -> LabelData -> LabelData
updateAimForTarget noUpdateDataList toUpdateData =
    let
        -- total number of labels for the no-update list
        noUpdateTotal =
            getTotalLabels noUpdateDataList

        -- remainder target for the toUpdateData list
        toUpdateDataTarget =
            Debug.log "toUpdateDataTarget" <|
                if target - noUpdateTotal >= 0 then
                    target - noUpdateTotal
                else
                    0

        newQuantity =
            Debug.log "toUpdateDataTarget" <|
                (toFloat toUpdateDataTarget)
                    / (toFloat toUpdateData.capacity)
    in
        { toUpdateData | quantity = round newQuantity }


updateModelTotalQuantity : LabelType -> Model -> Model
updateModelTotalQuantity labelType model =
    let
        ( toUpdateDataList, staticList ) =
            List.partition (\record -> labelType /= record.labelType && record.selected) model

        newModel =
            if List.length toUpdateDataList /= 1 then
                model
            else
                let
                    maybeToUpdateData =
                        getAt 0 toUpdateDataList
                in
                    case maybeToUpdateData of
                        Just toUpdateData ->
                            List.map
                                (\record ->
                                    if toUpdateData.labelType == record.labelType then
                                        updateAimForTarget staticList toUpdateData
                                    else
                                        record
                                )
                                model

                        Nothing ->
                            model
    in
        newModel


{-| Update the 'quantity' field of a single labelData record.
-}
updateAddModelQuantity : LabelType -> Quantity -> Model -> Model
updateAddModelQuantity labelType delta model =
    List.map
        (\record ->
            if labelType == record.labelType then
                { record | quantity = addNatural delta record.quantity }
            else
                record
        )
        model


updateSetModelQuantity : LabelType -> Quantity -> Model -> Model
updateSetModelQuantity labelType value model =
    List.map
        (\record ->
            if labelType == record.labelType then
                { record
                    | quantity =
                        if value < 0 then
                            0
                        else
                            value
                }
            else
                record
        )
        model


{-| Update the 'selected' status of a single labelData record.
-}
updateModelSelected : LabelType -> Model -> Model
updateModelSelected labelType model =
    let
        totalNrSelected =
            getTotalNumberSelected model
    in
        List.map
            (\record ->
                if labelType == record.labelType then
                    { record | selected = not record.selected && totalNrSelected < maxNrSelected }
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
                    model
                        |> (updateAddModelQuantity labelData.labelType num)
                        |> (updateModelTotalQuantity labelData.labelType)

                Set labelData str ->
                    case String.toInt str of
                        Err _ ->
                            model

                        Ok num ->
                            model
                                |> (updateSetModelQuantity labelData.labelType num)
                                |> (updateModelTotalQuantity labelData.labelType)

                CheckboxToggled labelData ->
                    updateModelSelected labelData.labelType model
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
