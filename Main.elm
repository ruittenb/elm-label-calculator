module Main exposing (main)

import Html exposing (Html, table, tr, th, td, button, input, div, span, text, p)
import Html.Attributes exposing (class, colspan, type_, checked)
import Html.Events exposing (onClick, onCheck)
import Dict exposing (Dict)
import Tuple
import List


type alias Quantity =
    Int


type alias Capacity =
    Int


type alias Format =
    String


type alias Selected =
    Bool


type alias FormatData =
    { quantity : Quantity
    , capacity : Capacity
    , selected : Selected
    }


type alias Model =
    Dict Format FormatData


type Msg
    = Add Format Quantity
    | CheckboxToggled Format Selected


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


model : Model
model =
    Dict.fromList
        [ ( "Herma", { quantity = 0, capacity = 16, selected = False } )
        , ( "Avery", { quantity = 0, capacity = 16, selected = True } )
        , ( "OfficeDepot", { quantity = 0, capacity = 24, selected = True } )
        , ( "Supermagnete", { quantity = 0, capacity = 20, selected = False } )
        ]


init : ( Model, Cmd msg )
init =
    ( model, Cmd.none )


viewTableHeader : Model -> Html Msg
viewTableHeader model =
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
        quantity =
            Dict.toList model
                -- keep FormatData, then Quantity
                |> List.map (Tuple.second >> .quantity)
                -- foldl : (a -> b -> b) -> b -> List a -> b
                |> List.foldl (+) 0

        totalLabels =
            Dict.toList model
                -- keep FormatData
                |> List.map Tuple.second
                -- foldl : (a -> b -> b) -> b -> List a -> b
                |> List.foldl
                    (\formatData accumulator -> accumulator + formatData.quantity * formatData.capacity)
                    0
    in
        tr [ class "footer" ]
            [ td [] []
            , td [] []
            , td [] []
            , td [ class "number quantity" ] [ text <| toString quantity ]
            , td [] []
            , td [] []
            , td [ class "number" ] [ text <| toString totalLabels ]
            ]


viewFormatDataToTableRow : ( Format, FormatData ) -> Html Msg
viewFormatDataToTableRow ( format, formatData ) =
    let
        totalLabels =
            formatData.quantity * formatData.capacity
    in
        tr []
            [ td []
                [ input
                    [ type_ "checkbox"
                    , onCheck (CheckboxToggled format)
                    , checked formatData.selected
                    ]
                    []
                ]
            , td [] [ text format ]
            , td []
                [ button [ onClick (Add format -10) ] [ text "-10" ]
                , button [ onClick (Add format -1) ] [ text "-1" ]
                ]
            , td [ class "number quantity" ] [ text <| toString formatData.quantity ]
            , td []
                [ button [ onClick (Add format 1) ] [ text "+1" ]
                , button [ onClick (Add format 10) ] [ text "+10" ]
                ]
            , td [ class "number" ] [ text <| toString formatData.capacity ]
            , td [ class "number" ] [ text <| toString totalLabels ]
            ]


viewFormatDetailsAndButtons : Model -> List (Html Msg)
viewFormatDetailsAndButtons model =
    Dict.toList model
        |> List.map viewFormatDataToTableRow


view : Model -> Html Msg
view model =
    table [] <|
        [ viewTableHeader model ]
            ++ viewFormatDetailsAndButtons model
            ++ [ viewTableFooter model ]


updateModelQuantity : Model -> Format -> Quantity -> Model
updateModelQuantity model format delta =
    let
        newFormatData =
            Dict.get format model
                |> Maybe.map (\formatData -> { formatData | quantity = addNatural delta formatData.quantity })
                |> Maybe.withDefault { quantity = 0, capacity = 0, selected = False }
    in
        Dict.insert format newFormatData model


updateModelSelected : Model -> Format -> Selected -> Model
updateModelSelected model format checked =
    let
        newFormatData =
            Dict.get format model
                |> Maybe.map (\formatData -> { formatData | selected = checked })
                |> Maybe.withDefault { quantity = 0, capacity = 0, selected = False }
    in
        Dict.insert format newFormatData model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Add format num ->
                    updateModelQuantity model format num

                CheckboxToggled format checked ->
                    updateModelSelected model format checked
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
