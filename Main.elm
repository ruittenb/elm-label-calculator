module Main exposing (main)

import Html exposing (Html, button, div, span, text)
import Dict exposing (Dict)
import List
import Tuple


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


viewFormatDetailsAndButtons : Model -> List (Html Msg)
viewFormatDetailsAndButtons model =
    -- TODO
    [ text "Lorem Ipsum" ]


view : Model -> Html Msg
view model =
    div [] (viewFormatDetailsAndButtons model)


updateModel : Model -> Format -> Quantity -> Model
updateModel model format delta =
    let
        newFormatData =
            Dict.get format model
                |> Maybe.map (\t -> Tuple.mapFirst ((+) delta) t)
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
