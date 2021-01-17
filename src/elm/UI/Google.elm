module UI.Google exposing (buildUrl, redirectUrl)

import Url.Builder as B


scope : String
scope =
    "https://www.googleapis.com/auth/drive.file"


buildUrl : String -> String
buildUrl root =
    B.crossOrigin "https://accounts.google.com"
        [ "o", "oauth2", "v2", "auth" ]
        [ B.string "scope" scope
        , B.string "response_type" "token"
        , B.string "client_id" clientId
        , B.string "redirect_uri" root
        ]


clientId : String
clientId =
    "696268500736-k646rm6l6qcucv2t1qufdg31ngsfgmhu.apps.googleusercontent.com"


redirectUrl : String
redirectUrl =
    "https://nuplot.netlify.app"
