module Xml exposing (..)

import Parser exposing (Parser,(|.),(|=),spaces,succeed,symbol,keyword, andThen)
import Dict exposing (Dict)
import Set exposing (Set)

type XmlTag
    = SubTags (Dict String (List XmlTag))
    | PresenceTag
    | XmlInt Int
    | XmlFloat Float
    | XmlString String

xmlFile : Parser XmlTag
xmlFile =
    succeed identity
        |. Parser.symbol "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        |. spaces
        |. possibleComments
        |. spaces
        |= (xmlTag |> Parser.map Tuple.second)
        |. spaces
        |. Parser.end

xmlTag : Parser (String,XmlTag)
xmlTag =
    succeed (Debug.log "xmlTag")
        |. symbol "<"
        |= Parser.oneOf 
            [ succeed (\s -> ("comment",XmlString s))
                |. symbol "!--"
                |= Parser.getChompedString 
                    (Parser.chompUntil "-->")
                |. symbol "-->"
            , (succeed identity
                |= Parser.getChompedString (Parser.chompWhile Char.isAlpha)
                |. spaces
                |. discardAttributeList)
                |> andThen 
                    (\s -> 
                        Parser.oneOf
                            [ succeed (s,PresenceTag)
                                |. symbol "/>"
                            , succeed (s,PresenceTag)
                                |. symbol ">"
                                |. spaces
                                |. symbol ("</"++s++">")
                            , succeed (\n -> (s, n))
                                |. symbol ">"
                                |= keyNumber 
                                    { int = Just XmlInt
                                    , hex = Nothing
                                    , octal = Nothing
                                    , binary = Nothing
                                    , float = Just XmlFloat
                                    }
                                    (Set.fromList ['<'])
                                |. symbol ("</"++s++">")
                            , succeed (\str -> (s, XmlString str))
                                |. symbol ">"
                                |= Parser.variable
                                    { start = Char.isAlphaNum
                                    , inner = \c -> c /= '<'
                                    , reserved = Set.empty
                                    }
                                |. symbol ("</"++s++">")
                            , succeed (\x -> (s, x))
                                |. symbol ">"
                                |. spaces
                                |= xmlTagContents
                                |. symbol ("</"++s++">")
                            ]
                            |. spaces
                    )
            ]

xmlTagContents : Parser XmlTag
xmlTagContents =
    Parser.loop Dict.empty
        (\dictState ->
            Parser.oneOf 
                [ succeed
                    (\(tag,xmlDat) -> Parser.Loop (insertDictListAdd tag xmlDat dictState))
                    |= xmlTag
                , Parser.commit (Parser.Done (SubTags dictState))
                ]
                |. spaces
        )

insertDictListAdd : comparable -> a -> Dict comparable (List a) -> Dict comparable (List a)
insertDictListAdd target newItem =
    Dict.update target
        (\m ->
            case m of
                Just l ->
                    Just (newItem::l)
                Nothing ->
                    Just (List.singleton newItem)
        )

type alias KeyNumberData a =
    { int : Maybe (Int -> a)
    , hex : Maybe (Int -> a)
    , octal : Maybe (Int -> a)
    , binary : Maybe (Int -> a)
    , float : Maybe (Float -> a)
    }

keyNumber : KeyNumberData a -> Set Char -> Parser a
keyNumber dat allowedChars =
    Parser.oneOf
        [ Parser.backtrackable
            ( Parser.number dat
                |. Parser.variable
                    { start = \c -> not (Set.member c allowedChars)
                    , inner = always False
                    , reserved = Set.empty
                    }
                |. Parser.problem "ExpectingKeynumber"
            )
        , Parser.number dat
        ]

possibleComments : Parser ()
possibleComments =
    Parser.loop () 
        (always 
            ( Parser.oneOf 
                [ succeed (Parser.Loop ())
                    |. Parser.multiComment "<!--" "-->" Parser.NotNestable
                    |. symbol "-->"
                , succeed (Parser.Done ())
                ]
                |. spaces
            )
        )

discardAttributeList : Parser ()
discardAttributeList = 
    Parser.loop () 
        (always
            ( Parser.oneOf
                [ succeed (Parser.Loop ())
                    |. Parser.variable
                        { start = Char.isAlpha
                        , inner = (\c -> Char.isAlphaNum c || c == ':')
                        , reserved = Set.empty
                        }
                    |. symbol "="
                    |. Parser.multiComment "\"" "\"" Parser.NotNestable
                    |. symbol "\""
                , succeed (Parser.Done ())
                ]
                |. spaces
            )
        )