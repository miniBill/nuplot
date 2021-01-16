module UI.L10N exposing (L10N, invariant, text, title)

import Element.WithContext as Element exposing (Element)
import Html.Attributes
import Model exposing (Language(..))


type alias L10N a =
    { en : a
    , it : a
    }


invariant : a -> { en : a, it : a }
invariant content =
    { en = content
    , it = content
    }


text : L10N String -> Element { a | language : Language } msg
text { en, it } =
    Element.with
        (\{ language } ->
            case language of
                En ->
                    en

                It ->
                    it
        )
        Element.text


title : L10N String -> Element.Attribute { a | language : Language } msg
title { en, it } =
    Element.withAttribute
        (\{ language } ->
            case language of
                En ->
                    en

                It ->
                    it
        )
        (Element.htmlAttribute << Html.Attributes.title)
