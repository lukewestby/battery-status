effect module BatteryStatus
    where { subscription = MySub }
    exposing
        ( isSupported
        , Error(..)
        , isCharging
        , chargeLevel
        , chargingTime
        , dischargingTime
        , isChargingChanges
        , chargeLevelChanges
        , chargingTimeChanges
        , dischargingTimeChanges
        )

{-| This package helps you find out information about the user's battery life if
it is available through the browser. This will let you do things like control
the rate at which animations or network polling is performed to preserve the
user's battery life and account for reduced CPU performance at low power levels.

# Support
@docs isSupported

# Queries
@docs Error, isCharging, chargeLevel, chargingTime, dischargingTime

# Subscriptions
@docs isChargingChanges, chargeLevelChanges, chargingTimeChanges, dischargingTimeChanges
-}

import Dict exposing (Dict)
import Time exposing (Time)
import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder, Value, at, float, int, bool)
import Process
import Platform exposing (Router)
import Native.BatteryStatus


secondsToTime : Int -> Time
secondsToTime seconds =
    (toFloat seconds) * Time.second


infinity : Int
infinity =
    round (1 / 0)


{-| Whether the entire API is supported by the browser. When this value is False
all query tasks will fail with an Unsupported error and all subscriptions will
never fire.
-}
isSupported : Bool
isSupported =
    Native.BatteryStatus.isSupported


{-| Different errors that can happen when interacting with the Battery Status
API. Currently only Unsupported will occur
-}
type Error
    = Unsupported


{-| Find out if the user's device is charging
-}
isCharging : Task Error Bool
isCharging =
    Native.BatteryStatus.isCharging ()


{-| Find out the battery level of the user's device, from 0 to 1
-}
chargeLevel : Task Error Float
chargeLevel =
    Native.BatteryStatus.chargeLevel ()


{-| Find out how long until the user's battery is full charged. If the device is
not charging then the value will be Infinity.
-}
chargingTime : Task Error Time
chargingTime =
    Native.BatteryStatus.chargeLevel ()
        |> Task.map secondsToTime


{-| Find out how long until the user's battery dies. If the user's device is
charging then the value will be Infinity.
-}
dischargingTime : Task Error Time
dischargingTime =
    Native.BatteryStatus.dischargingTime ()
        |> Task.map secondsToTime


{-| Get notified when the user plugs in or unplugs their device.
-}
isChargingChanges : (Bool -> msg) -> Sub msg
isChargingChanges =
    Charging >> subscription


{-| Get notified when the user's operating system reports a change in power
level.
-}
chargeLevelChanges : (Float -> msg) -> Sub msg
chargeLevelChanges =
    ChargeLevel >> subscription


{-| Get notified when the user's battery gets closer to being fully charged.
-}
chargingTimeChanges : (Time -> msg) -> Sub msg
chargingTimeChanges =
    ChargingTime >> subscription


{-| Get notified when the user's battery gets closer to dying.
-}
dischargingTimeChanges : (Time -> msg) -> Sub msg
dischargingTimeChanges =
    DischargingTime >> subscription


type MySub msg
    = Charging (Bool -> msg)
    | ChargeLevel (Float -> msg)
    | ChargingTime (Time -> msg)
    | DischargingTime (Time -> msg)


type alias SelfMsg =
    { eventName : String
    , value : Value
    }


subMap : (a -> b) -> MySub a -> MySub b
subMap mapper sub =
    case sub of
        Charging tagger ->
            Charging (tagger >> mapper)

        ChargeLevel tagger ->
            ChargeLevel (tagger >> mapper)

        ChargingTime tagger ->
            ChargingTime (tagger >> mapper)

        DischargingTime tagger ->
            DischargingTime (tagger >> mapper)


type alias Watcher msg =
    { pid : Process.Id
    , handlers : List (Value -> msg)
    }


type alias State msg =
    Dict String (Watcher msg)


type alias HandlersDict msg =
    Dict String (List (Value -> msg))


handlerForSub : MySub msg -> (Value -> msg)
handlerForSub sub =
    case sub of
        Charging tagger ->
            Decode.decodeValue (at [ "target", "charging" ] bool)
                >> Result.withDefault False
                >> tagger

        ChargeLevel tagger ->
            Decode.decodeValue (at [ "target", "level" ] float)
                >> Result.withDefault 1
                >> tagger

        ChargingTime tagger ->
            Decode.decodeValue (at [ "target", "chargingTime" ] int)
                >> Result.withDefault infinity
                >> secondsToTime
                >> tagger

        DischargingTime tagger ->
            Decode.decodeValue (at [ "target", "dischargingTime" ] int)
                >> Result.withDefault infinity
                >> secondsToTime
                >> tagger


eventNameForSub : MySub msg -> String
eventNameForSub sub =
    case sub of
        Charging tagger ->
            "chargingchange"

        ChargeLevel tagger ->
            "levelchange"

        ChargingTime tagger ->
            "chargingtimechange"

        DischargingTime tagger ->
            "dischargingtimechange"


categorizeHelpHelp : (Value -> msg) -> Maybe (List (Value -> msg)) -> Maybe (List (Value -> msg))
categorizeHelpHelp handler maybeStuff =
    maybeStuff
        |> Maybe.withDefault []
        |> (::) handler
        |> Just


categorizeHelp : MySub msg -> HandlersDict msg -> HandlersDict msg
categorizeHelp sub handlers =
    handlers
        |> Dict.update (eventNameForSub sub) (categorizeHelpHelp (handlerForSub sub))


categorize : List (MySub msg) -> HandlersDict msg
categorize subs =
    List.foldl categorizeHelp Dict.empty subs


init : Task Never (State msg)
init =
    Task.succeed <| Dict.empty


onEffects : Router msg SelfMsg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router subs state =
    let
        categorized =
            categorize subs

        leftStep eventName { pid } nextStateTask =
            Process.kill pid `Task.andThen` \_ -> nextStateTask

        bothStep eventName { pid } handlers nextStateTask =
            nextStateTask
                |> Task.map (Dict.insert eventName (Watcher pid handlers))

        rightStep eventName handlers nextStateTask =
            nextStateTask
                `Task.andThen` \state ->
                                Process.spawn (Native.BatteryStatus.on eventName (SelfMsg eventName >> Platform.sendToSelf router))
                                    `Task.andThen` \pid ->
                                                    Task.succeed (Dict.insert eventName (Watcher pid handlers) state)
    in
        Task.succeed state


onSelfMsg : Router msg SelfMsg -> SelfMsg -> State msg -> Task Never (State msg)
onSelfMsg router { eventName, value } state =
    case Dict.get eventName state of
        Just { handlers } ->
            handlers
                |> List.map (\handler -> Platform.sendToApp router (handler value))
                |> Task.sequence
                |> Task.map (\_ -> state)

        Nothing ->
            Task.succeed state
