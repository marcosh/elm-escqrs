module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)


type alias History =
    List Msg


applyEvents : History -> Int
applyEvents =
    applyEventsExcept 0


applyEventsExcept : Int -> History -> Int
applyEventsExcept n history =
    case history of
        head :: tail ->
            case head of
                Back ->
                    applyEventsExcept (n + 1) tail

                Increment ->
                    if (n > 0) then
                        applyEventsExcept (n - 1) tail
                    else
                        (applyEventsExcept 0 tail) + 1

                Decrement ->
                    if (n > 0) then
                        applyEventsExcept (n - 1) tail
                    else
                        (applyEventsExcept 0 tail) - 1

        [] ->
            0


view : History -> Html Msg
view history =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString (applyEvents history)) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Back ] [ text "Back" ]
        ]


type Msg
    = Increment
    | Decrement
    | Back


update : Msg -> History -> History
update msg history =
    msg :: history


main : Program Never
main =
    beginnerProgram
        { model = []
        , view = view
        , update = update
        }
