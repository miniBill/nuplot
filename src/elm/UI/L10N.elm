module UI.L10N exposing (L10N, invariant, text, title)

import Element.WithContext as Element exposing (Element)
import Html.Attributes
import Model exposing (Language(..))


type alias L10N a =
    { en : a
    , it : a
    }


invariant : a -> L10N a
invariant content =
    { en = content
    , it = content
    }


localize : L10N a -> { b | language : Language } -> a
localize { en, it } { language } =
    case language of
        En ->
            en

        It ->
            it


text : L10N String -> Element { a | language : Language } msg
text l10n =
    Element.with (localize l10n)
        Element.text


title : L10N String -> Element.Attribute { a | language : Language } msg
title l10n =
    Element.withAttribute (localize l10n)
        (Element.htmlAttribute << Html.Attributes.title)
