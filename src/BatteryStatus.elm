effect module BatteryStatus
    where { subscription = MySub }
    exposing
        ( isSupported
        , BatteryStatus
        , Error(..)
        , now
        , withChanges
        )

{-| This package helps you find out information about the user's battery life if
it is available through the browser. This will let you do things like control
the rate at which animations or network polling is performed to preserve the
user's battery life and account for reduced CPU performance at low power levels.

# Support
@docs isSupported

# Data Types
@docs BatteryStatus, Error

# Queries
@docs now

# Subscriptions
@docs withChanges
-}

import Time exposing (Time)
import Task exposing (Task)
import Process
import Platform exposing (Router)
import Native.BatteryStatus


{-| Whether the entire API is supported by the browser. When this value is False
all query tasks will fail with an Unsupported error and all subscriptions will
never fire.
-}
isSupported : Bool
isSupported =
    Native.BatteryStatus.isSupported


{-| Include battery status changes in your subscriptions if they are available
on the user's device.

    type Msg
        = BatteryStatusChange BatteryStatus.BatteryStatus


    main : Program Never
    main =
        { view = view, update = update, init = init, subscriptions = subscriptions }
            |> BatteryStatus.withChanges BatteryStatusChange
            |> Html.App.program
-}
withChanges :
    (BatteryStatus -> msg)
    -> { a | subscriptions : model -> Sub msg }
    -> { a | subscriptions : model -> Sub msg }
withChanges tagger program =
    if not isSupported then
        program
    else
        { program
            | subscriptions =
                (\model ->
                    Sub.batch
                        [ program.subscriptions model
                        , changes tagger
                        ]
                )
        }


{-| All available information about the battery in the device.
  * `isCharging` &mdash; whether the device is plugged in.
  * `level` &mdash; how much of a charge the battery has, from 0 to 1.
  * `chargingTime` &mdash; how long until the device is charged. If the device
    isn't charging or the time hasn't been calculated the value is Infinity.
  * `dischargingTime` &mdash; how long until the device's battery dies. If the
    device is plugged in or the time hasn't been calculated the value is
    Infinity.
-}
type alias BatteryStatus =
    { isCharging : Bool
    , level : Float
    , chargingTime : Maybe Time
    , dischargingTime : Maybe Time
    }


{-| Different errors that can happen when interacting with the Battery Status
API. Currently only Unsupported will occur.
-}
type Error
    = Unsupported


{-| Retrieve the current state of the device's battery. If the Battery Status
API isn't supported by the current device the task will fail with an
Unsupported error.
-}
now : Task Error BatteryStatus
now =
    Native.BatteryStatus.batteryStatus


{-| Subscribe to changes in the device's battery. If the Battery Status API
isn't supported, no notifications will be delivered.
-}
changes : (BatteryStatus -> msg) -> Sub msg
changes =
    Changes >> subscription



-- EFFECT MANAGER


type MySub msg
    = Changes (BatteryStatus -> msg)


unwrap : List (MySub msg) -> List (BatteryStatus -> msg)
unwrap list =
    List.map (\(Changes tagger) -> tagger) list


subMap : (a -> b) -> MySub a -> MySub b
subMap mapper (Changes tagger) =
    Changes (tagger >> mapper)


type alias State msg =
    { listeners : List (BatteryStatus -> msg)
    , processId : Maybe Process.Id
    }


init : Task Never (State msg)
init =
    Task.succeed <| State [] Nothing


onEffects : Router msg BatteryStatus -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router subs state =
    if isSupported then
        case ( subs, state.processId ) of
            ( [], Just pid ) ->
                Process.kill pid
                    |> Task.map (\_ -> State [] Nothing)

            ( _ :: _, Nothing ) ->
                Process.spawn (Native.BatteryStatus.onChange (Platform.sendToSelf router))
                    |> Task.map (Just >> State (unwrap subs))

            ( _, _ ) ->
                Task.succeed <| State (unwrap subs) state.processId
    else
        Task.succeed state


onSelfMsg : Router msg BatteryStatus -> BatteryStatus -> State msg -> Task Never (State msg)
onSelfMsg router batteryStatus state =
    state.listeners
        |> List.map (\listener -> Platform.sendToApp router (listener batteryStatus))
        |> Task.sequence
        |> Task.map (\_ -> state)
