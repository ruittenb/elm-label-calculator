module Main exposing (main)

import Html exposing (Html, table, tr, th, td, button, div, span, text, p)
import Html.Attributes exposing (class, colspan)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Tuple
import List


type alias Quantity =
    Int


type alias Capacity =
    Int


type alias Format =
    String


type alias FormatData =
    ( Quantity, Capacity )


type alias Model =
    Dict Format FormatData


type Msg
    = Add Format Quantity


model : Model
model =
    Dict.fromList
        [ ( "Avery", ( 0, 16 ) )
        , ( "OfficeDepot", ( 0, 24 ) )
        ]


init : ( Model, Cmd msg )
init =
    ( model, Cmd.none )


viewTableHeader : Model -> Html Msg
viewTableHeader model =
    tr []
        [ th [] [ text "labelsoort" ]
        , th [ colspan 3 ] [ text "aantal vel" ]
        , th [] [ text "labels per vel" ]
        , th [] [ text "totaal # labels" ]
        ]


viewTableFooter : Model -> Html Msg
viewTableFooter model =
    let
        quantity =
            Dict.toList model
                -- keep FormatData
                |> List.map Tuple.second
                -- keep Quantity
                |> List.map Tuple.first
                -- foldl : (a -> b -> b) -> b -> List a -> b
                |> List.foldl (+) 0

        labels =
            Dict.toList model
                -- keep FormatData
                |> List.map Tuple.second
                -- foldl : (a -> b -> b) -> b -> List a -> b
                |> List.foldl
                    (\( quantity, capacity ) accumulator -> accumulator + quantity * capacity)
                    0
    in
        tr [ class "footer" ]
            [ td [] []
            , td [] []
            , td [ class "number quantity" ] [ text <| toString quantity ]
            , td [] []
            , td [] []
            , td [ class "number" ] [ text <| toString labels ]
            ]


viewFormatDataToTableRow : ( Format, FormatData ) -> Html Msg
viewFormatDataToTableRow ( format, formatData ) =
    let
        ( quantity, capacity ) =
            formatData

        labels =
            quantity * capacity
    in
        tr []
            [ td [] [ text format ]
            , td []
                [ button [ onClick (Add format -10) ] [ text "-10" ]
                , button [ onClick (Add format -1) ] [ text "-1" ]
                ]
            , td [ class "number quantity" ] [ text <| toString quantity ]
            , td []
                [ button [ onClick (Add format 1) ] [ text "+1" ]
                , button [ onClick (Add format 10) ] [ text "+10" ]
                ]
            , td [ class "number" ] [ text <| toString capacity ]
            , td [ class "number" ] [ text <| toString labels ]
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


updateModel : Model -> Format -> Quantity -> Model
updateModel model format delta =
    let
        newFormatData =
            Dict.get format model
                |> Maybe.map (Tuple.mapFirst ((+) delta))
                |> Maybe.withDefault ( 0, 0 )
    in
        Dict.insert format newFormatData model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Add format num ->
                    updateModel model format num
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
