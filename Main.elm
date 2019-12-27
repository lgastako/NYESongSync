module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.General.HAlign as HAlign
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = const Sub.none
        , view = view
        }


type alias Model =
    { input : String
    , output : Result String ( Time, Time )
    }


type Msg
    = ChangeInput String


type alias Time =
    ( Int, Int, Int )


toSecs : Time -> Int
toSecs ( h, m, s ) =
    h * 60 * 60 + m * 60 + s


fromSecs : Int -> Time
fromSecs t =
    let
        h =
            t // 3600

        m =
            t % 3600 // 60

        s =
            t % 3600 % 60
    in
    ( h, m, s )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeInput s ->
            ( tryToUpdate { model | input = s }, Cmd.none )


tryToUpdate : Model -> Model
tryToUpdate model =
    case parseTime model.input of
        Ok dropTime ->
            let
                songSecs =
                    toSecs dropTime

                midnightSecs =
                    toSecs ( 24, 0, 0 )

                startSecs =
                    midnightSecs - songSecs

                startTime =
                    fromSecs startSecs
            in
            { model | output = Ok ( dropTime, startTime ) }

        Err e ->
            { model | output = Err e }


parseTime : String -> Result String Time
parseTime s =
    let
        handleMS ( ms, ss ) =
            case String.toInt ms of
                Err e ->
                    Err <| "Invalid minutes value: " ++ e

                Ok m ->
                    if String.length ss /= 2 then
                        Err "Seconds must be two digits."

                    else
                        case String.toInt ss of
                            Err e ->
                                Err <| "Invalid seconds value: " ++ e

                            Ok s ->
                                Ok ( 0, m, s )
    in
    case String.split ":" s of
        [ ms, ss ] ->
            handleMS ( ms, ss )

        [ hs, ms, ss ] ->
            case String.toInt hs of
                Err e ->
                    Err <| "Invalid hours value: " ++ e

                Ok h ->
                    if String.length ms /= 2 then
                        Err "Minutes must be two digits when hour is provided."

                    else
                        case handleMS ( ms, ss ) of
                            Err e ->
                                Err e

                            Ok ( _, m, s ) ->
                                Ok ( h, m, s )

        _ ->
            Err "Invalid time value, format should be hh:mm:ss or mm:ss."


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs10 ]
                [ h2 [ class "text-center" ] [ text "Enter time of bass drop" ] ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs10 ]
                [ p [ class "text-center" ]
                    [ input
                        [ onInput ChangeInput
                        , value model.input
                        ]
                        []
                    ]
                ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs10 ]
                [ h2 [ class "text-center" ] [ text "Time to start song" ] ]
            ]
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.xs10 ]
                [ div [ class "text-center" ]
                    [ case model.output of
                        Err msg ->
                            text msg

                        Ok ( _, startTime ) ->
                            let
                                altStartTime =
                                    fromSecs <|
                                        toSecs startTime
                                            - (12 * 60 * 60)
                            in
                            ul []
                                [ li [ style [ ( "list-style-type", " none" ) ] ]
                                    [ text <| renderTime startTime ]
                                , li [ style [ ( "list-style-type", " none" ) ] ]
                                    [ text <| "aka " ++ renderTime altStartTime ]
                                ]
                    ]
                ]
            ]
        ]


renderTime : Time -> String
renderTime ( h, m, s ) =
    let
        renderMS ( m, s ) =
            pad (toString m) ++ ":" ++ pad (toString s)
    in
    if h == 0 then
        renderMS ( m, s )

    else
        toString h ++ ":" ++ renderMS ( m, s )


pad : String -> String
pad s =
    if String.length s < 2 then
        "0" ++ s

    else
        s


init : ( Model, Cmd Msg )
init =
    ( Model "" (Err "No time entered.")
    , Cmd.none
    )


const : b -> a -> b
const x _ =
    x
