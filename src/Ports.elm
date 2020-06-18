port module Ports exposing (dumpModel, saveSessionId)


port saveSessionId : Maybe String -> Cmd msg


port dumpModel : (() -> msg) -> Sub msg
