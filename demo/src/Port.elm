port module Port exposing (returnValue, sendWat)


port sendWat : String -> Cmd msg


port returnValue : (String -> msg) -> Sub msg
