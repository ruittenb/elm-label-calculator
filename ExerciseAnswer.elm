module ExerciseAnswer exposing (..)

import Html exposing (Html, table, tr, td, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List


type alias Model =
    List Int


type Msg
    = IncrementAll


model : Model
model =
    [ 0, 1, 3 ]


limit : Int
limit =
    14


getTotal : Model -> Int
getTotal model =
    List.foldl (+) 0 model


viewTableCell : Model -> Int -> Html Msg
viewTableCell model num =
    let
        styleAttributes =
            if getTotal model > limit then
                [ ( "color", "red" ) ]
            else
                []
    in
        td [ style styleAttributes ] [ text <| toString num ]


view : Model -> Html Msg
view model =
    let
        viewTableCellModel =
            viewTableCell model
    in
        table []
            [ tr [] <|
                button [ onClick IncrementAll ] [ text "Increment All" ]
                    :: List.map viewTableCellModel model
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- there is only one msg
    ( List.map ((+) 1) model
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
