module Main exposing (..)

import Time exposing (Time)
import Task exposing (Task)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Html.App as HtmlApp
import BatteryStatus exposing (BatteryStatus)
import Svg exposing (Svg, svg, path, g, rect, polygon, defs, clipPath)
import Svg.Attributes as SvgAttr exposing (d, id, x, y, viewBox, width, height, fill, fillRule, points, transform)


performSafe : (a -> Msg) -> Task x a -> Cmd Msg
performSafe onSuccess task =
    Task.perform (\_ -> NoOp) onSuccess task


flatten : List ( a, Bool ) -> List a
flatten list =
    List.filterMap
        (\( val, pred ) ->
            if pred then
                Just val
            else
                Nothing
        )
        list


type alias Model =
    { batteryStatus : Maybe BatteryStatus }


type Msg
    = BatteryStatusChange BatteryStatus
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BatteryStatusChange batteryStatus ->
            { model | batteryStatus = Just batteryStatus } ! []

        NoOp ->
            model ! []


model : Model
model =
    Model Nothing


init : ( Model, Cmd Msg )
init =
    ( model
    , BatteryStatus.now |> performSafe BatteryStatusChange
    )


emptySpace : Bool -> Svg msg
emptySpace isCharging =
    g []
        <| flatten
            [ ( rect
                    [ fill "#FFFFFF"
                    , x "5"
                    , y "5"
                    , width "80"
                    , height "50"
                    ]
                    []
              , True
              )
            , ( polygon
                    [ fill "#000000"
                    , fillRule "evenodd"
                    , points "14.9850746 0 10.8208955 12.65625 20 12.65625 5.01492537 30 9.17910448 17.34375 0 17.34375"
                    , width "20"
                    , height "30"
                    , transform "translate(35, 15)"
                    ]
                    []
              , isCharging
              )
            ]


filledSpace : Bool -> Svg msg
filledSpace isCharging =
    g [ SvgAttr.clipPath "url(#chargeLevel)" ]
        <| flatten
            [ ( rect
                    [ fill "#000000"
                    , x "8"
                    , y "8"
                    , width "74"
                    , height "44"
                    ]
                    []
              , True
              )
            , ( polygon
                    [ fill "#FFFFFF"
                    , fillRule "evenodd"
                    , points "14.9850746 0 10.8208955 12.65625 20 12.65625 5.01492537 30 9.17910448 17.34375 0 17.34375"
                    , width "20"
                    , height "30"
                    , transform "translate(35, 15)"
                    ]
                    []
              , isCharging
              )
            ]


mask : Float -> Svg msg
mask percentage =
    clipPath [ id "chargeLevel" ]
        [ rect
            [ x "8"
            , y "8"
            , width <| toString <| (round (percentage * 74))
            , height "44"
            ]
            []
        ]


formatTime : Time -> String
formatTime time =
    let
        hours =
            time |> Time.inHours |> floor

        minutes =
            time - ((toFloat hours) * Time.hour) |> Time.inMinutes |> floor

        minutesString =
            if minutes < 10 then
                "0" ++ (toString minutes)
            else
                toString minutes
    in
        (toString hours) ++ ":" ++ minutesString


timingMessage : BatteryStatus -> Html Msg
timingMessage { isCharging, chargingTime, dischargingTime } =
    case ( isCharging, chargingTime, dischargingTime ) of
        ( True, Just time, _ ) ->
            text <| (formatTime time) ++ " Until Charged"

        ( False, _, Just time ) ->
            text <| (formatTime time) ++ " Remaining"

        ( _, _, _ ) ->
            text "Calculating battery life..."


view : Model -> Html Msg
view { batteryStatus } =
    if not BatteryStatus.isSupported then
        text "Can't read battery status :("
    else
        case batteryStatus of
            Nothing ->
                text ""

            Just status ->
                div
                    [ style
                        [ ( "display", "flex" )
                        , ( "justify-content", "center" )
                        , ( "align-items", "center" )
                        , ( "width", "100%" )
                        , ( "height", "100%" )
                        , ( "flex-direction", "column" )
                        ]
                    ]
                    [ svg
                        [ viewBox "0 0 100 60"
                        , width "200px"
                        , height "120px"
                        ]
                        [ defs [] [ mask status.level ]
                        , path
                            [ d "M97.5278396,15 L89.3095768,15 L89.3095768,2.34375 C89.3095768,1.0546875 88.3518931,0 87.1046771,0 L2.24944321,0 C1.00222717,0 0,1.0546875 0,2.34375 L0,57.65625 C0,58.9453125 1.00222717,60 2.24944321,60 L87.1269488,60 C88.3518931,60 89.3318486,58.9453125 89.3318486,57.65625 L89.3318486,45 L97.5501114,45 C98.7973274,45 99.7995546,43.9453125 99.7995546,42.65625 L99.7995546,17.34375 C99.7772829,16.0546875 98.7750557,15 97.5278396,15 L97.5278396,15 Z"
                            , fill "#000000"
                            ]
                            []
                        , emptySpace status.isCharging
                        , filledSpace status.isCharging
                        ]
                    , div
                        [ style
                            [ ( "font-family", "sans-serif" )
                            , ( "font-size", "2em" )
                            , ( "padding", "24px" )
                            ]
                        ]
                        [ timingMessage status ]
                    ]


main : Program Never
main =
    { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
        |> BatteryStatus.withChanges BatteryStatusChange
        |> HtmlApp.program
