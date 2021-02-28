port module UI.Ports exposing (copyCanvas, gotGoogleAccessToken, openWindow, persist, saveCanvas, saveGoogleAccessToken, saveGoogleAccessTokenAndCloseWindow)

import Json.Decode exposing (Value)
import UI.Model exposing (CanvasId(..))


port openWindow : String -> Cmd msg


port persist : Value -> Cmd msg


copyCanvas : CanvasId -> Cmd msg
copyCanvas (CanvasId id) =
    copy id


port copy : String -> Cmd msg


saveCanvas : CanvasId -> Cmd msg
saveCanvas (CanvasId id) =
    save id


port save : String -> Cmd msg


port saveGoogleAccessToken : String -> Cmd msg


port saveGoogleAccessTokenAndCloseWindow : String -> Cmd msg


port gotGoogleAccessToken : (String -> msg) -> Sub msg
