port module UI.Ports exposing (copy, gotGoogleAccessToken, openWindow, persist, save, saveGoogleAccessTokenAndCloseWindow)

import Json.Decode exposing (Value)


port openWindow : String -> Cmd msg


port persist : Value -> Cmd msg


port copy : String -> Cmd msg


port save : String -> Cmd msg


port saveGoogleAccessTokenAndCloseWindow : String -> Cmd msg


port gotGoogleAccessToken : (String -> msg) -> Sub msg
