module UI.L10N exposing (L10N, Language(..), concat, invariant, localize, map, sequence, text, title, traverse)

import Element.WithContext as Element exposing (Attribute, Element)
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


traverse : (a -> L10N b) -> List a -> L10N (List b)
traverse f =
    List.foldr
        (\e { en, it } ->
            let
                fe =
                    f e
            in
            { en = fe.en :: en
            , it = fe.it :: it
            }
        )
        { en = [], it = [] }


sequence : List (L10N a) -> L10N (List a)
sequence =
    traverse identity


concat : List (L10N String) -> L10N String
concat =
    sequence >> map String.concat


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


title : L10N String -> Attribute { a | language : Language } msg
title l10n =
    Element.withAttribute (localize l10n)
        (Element.htmlAttribute << Html.Attributes.title)
