module Exercise exposing (..)

import Html exposing (Html, table, tr, td, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List


-- Exercise: This program contains a bug. The formatting (color: red) does not show. Why?


type alias Model =
    List Int


type Msg
    = IncrementAll


model : Model
model =
    [ 0, 2, 3 ]


limit : Int
limit =
    13


getTotal : Model -> Int
getTotal model =
    List.foldl (+) 0 model


viewTableCell : Int -> Html Msg
viewTableCell num =
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
    table []
        [ tr [] <|
            button [ onClick IncrementAll ] [ text "Increment All" ]
                :: List.map viewTableCell model
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
