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
    succeed identity
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
        (\d ->
            Parser.oneOf 
                [ succeed (\x -> Parser.Done d)
                    |= Parser.number
                        { int = Just XmlInt
                        , hex = Nothing
                        , octal = Nothing
                        , binary = Nothing
                        , float = Just XmlFloat
                        }
                , succeed 
                    (\(s,x) -> 
                        Parser.Loop (d |> insertDictListAdd s x)
                    )
                    |= xmlTag
                , succeed (Parser.Done (SubTags d))
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