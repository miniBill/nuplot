port module UI.Ports exposing (copyCanvas, exitFullscreenCanvas, fullscreenCanvas, gotGoogleAccessToken, isFullscreen, openWindow, persist, resetZoomCanvas, saveCanvas, saveGoogleAccessToken, saveGoogleAccessTokenAndCloseWindow)

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


fullscreenCanvas : CanvasId -> Cmd msg
fullscreenCanvas (CanvasId id) =
    fullscreen id


exitFullscreenCanvas : CanvasId -> Cmd msg
exitFullscreenCanvas (CanvasId id) =
    exitFullscreen id


resetZoomCanvas : CanvasId -> Cmd msg
resetZoomCanvas (CanvasId id) =
    resetZoom id


port fullscreen : String -> Cmd msg


port saveGoogleAccessToken : String -> Cmd msg


port saveGoogleAccessTokenAndCloseWindow : String -> Cmd msg


port gotGoogleAccessToken : (String -> msg) -> Sub msg


port exitFullscreen : String -> Cmd msg


port resetZoom : String -> Cmd msg


port isFullscreen : (Bool -> msg) -> Sub msg
