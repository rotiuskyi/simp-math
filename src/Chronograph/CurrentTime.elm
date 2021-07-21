module Chronograph.CurrentTime exposing (..)

import Task
import Time


type alias CurrentTime =
    Time.Posix


init : Time.Posix
init =
    Time.millisToPosix 0


type CurrentTimeMsg
    = RequestTime
    | NewTime Time.Posix


update : CurrentTimeMsg -> CurrentTime -> ( CurrentTime, Cmd CurrentTimeMsg )
update msg currTime =
    case msg of
        RequestTime ->
            ( currTime, Task.perform NewTime Time.now )

        NewTime time ->
            ( time, Cmd.none )


every1sec : Sub CurrentTimeMsg
every1sec =
    Time.every 1000 NewTime
