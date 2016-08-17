module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.App exposing (program)
import Html.Events exposing (onClick)
import List exposing (foldl)
import VirtualDom


--commands received from the user or the external world, the application
-- decides if and how to react to them


type Command
    = Increment
    | Decrement



-- facts that already happed, the application can not refuse to process them


type Event
    = Incremented Int
    | Decremented Int



-- actual state of the application


type alias State =
    Int


type alias Events =
    List Event



-- Cmd always goes with Event, because when a Cmd completes we receive the
-- notification of something that already happened


init : ( State, Cmd Event )
init =
    ( 0, Cmd.none )



-- the command handler receives a command and, using the history of the
-- application, decides which events needs to happen next. This is the point
-- where the application can decide not to process a command; in that case it
-- returns an empty list of Events


commandHandler : Command -> Events -> Events
commandHandler command events =
    case command of
        Increment ->
            [ Incremented 1 ]

        Decrement ->
            [ Decremented 1 ]



-- the event handler updates the state of the application according to the Event
-- that happened


eventHandler : Event -> State -> State
eventHandler event state =
    case event of
        Incremented amount ->
            state + amount

        Decremented amount ->
            state - amount



-- the process manager automates the execution of new Cmd's according to the
-- received Events


process : Events -> Maybe (Cmd Event)
process events =
    Nothing


view : State -> Html Command
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]



-- according to the state of the application, we decide how the external world
-- can interact with the application. Notice that Sub takes both Command's and
-- Event's as arguments, since from the external world we could receive both
-- notifications of thins which already happened or requests to perform commands


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none



-- FRAMEWORK
--
--
-- the model is actually composed by two parts: the write model, which is
-- just the list of the events happened to the application, and a read model,
-- which keeps track of the actual state of the application. Notice that the
-- read model can be reconstructed anytime from the write model just by applying
-- the events one by one


type alias Model =
    { write : Events
    , read : State
    }


type Msg
    = C Command
    | E Event


init' : ( Model, Cmd Msg )
init' =
    let
        ( initialState, command ) =
            init
    in
        ( { write = [], read = initialState }, Cmd.map E command )


view' : Model -> Html Msg
view' model =
    VirtualDom.map C (view model.read)


projection : Events -> State
projection events =
    foldl eventHandler (fst init) events


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( history, state, maybeCmd ) =
            progress msg model

        cmd =
            Maybe.withDefault Cmd.none (Maybe.map (Cmd.map E) maybeCmd)
    in
        ( { model | write = history, read = state }, cmd )


progress : Msg -> Model -> ( Events, State, Maybe (Cmd Event) )
progress msg model =
    case msg of
        C command ->
            let
                events =
                    commandHandler command model.write

                history =
                    List.append (events) model.write
            in
                ( history, projection history, process events )

        E event ->
            ( event :: model.write, eventHandler event model.read, process [ event ] )


subscriptions' : Model -> Sub Msg
subscriptions' model =
    subscriptions model.read


main : Program Never
main =
    program
        { init = init'
        , view = view'
        , update = update
        , subscriptions = subscriptions'
        }
