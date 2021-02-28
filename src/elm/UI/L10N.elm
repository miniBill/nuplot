module UI.L10N exposing (L10N, Language(..), invariant, map, text, title)

import Element.WithContext as Element exposing (Element)
import Html.Attributes


type Language
    = En
    | It


type alias L10N a =
    { en : a
    , it : a
    }


map : (a -> b) -> L10N a -> L10N b
map f { en, it } =
    { en = f en
    , it = f it
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
