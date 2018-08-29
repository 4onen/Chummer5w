module Xml exposing (..)

import Parser exposing (Parser,(|.),(|=),spaces,succeed,symbol,keyword, andThen)
import Dict exposing (Dict)
import Set exposing (Set)

type XmlTag
    = Xmls (Dict String (List XmlTag))
    | XmlInt Int
    | XmlFloat Float
    | XmlString String
    | XmlTag

xmlFile : Parser XmlTag
xmlFile =
    succeed identity
        |. Parser.symbol "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        |. spaces
        |. Parser.oneOf 
            [ xmlComment
            , succeed ()
            ]
        |. spaces
        |= (xmlTag |> Parser.map Tuple.second)
        |. spaces
        |. Parser.end

xmlTag : Parser (String,XmlTag)
xmlTag =
    Parser.oneOf
        [ xmlInt
        , xmlFloat
        ,   ( succeed identity
                |. symbol "<"
                |= Parser.getChompedString (Parser.chompIf Char.isAlpha)
                |. Parser.chompUntil ">"
                |. symbol ">"
                |. spaces
            ) |> andThen 
            (\s -> 
                (xmlTagContent s) 
                    |> Parser.map 
                        (\l -> 
                            l
                                |> List.foldl
                                    (\(s1,x1)->
                                        Dict.update s1
                                            (\m ->
                                                case m of
                                                    Just x2 ->
                                                        Just (x1::x2)
                                                    Nothing ->
                                                        Just [x1]
                                            )
                                    ) Dict.empty
                                |> (\d -> (s,Xmls d))
                        )
            )
        , xmlString
        , succeed (\s -> (s,XmlTag))
            |. symbol "<"
            |= Parser.getChompedString (Parser.chompIf Char.isAlpha)
            |. symbol " />"
        ]
        |. spaces

xmlTagContent : String -> Parser (List (String, XmlTag))
xmlTagContent s =
    Parser.loop [] 
        (\l ->
            Parser.oneOf
                [ succeed (Parser.Done l)
                    |. symbol ("</"++s++">")
                , succeed (\x -> Parser.Loop (x::l))
                    |= xmlTag
                ]
                |. spaces
        )

xmlInt : Parser (String, XmlTag)
xmlInt =
    ( succeed identity 
        |. symbol "<"
        |= Parser.getChompedString (Parser.chompIf Char.isAlpha)
        |. symbol ">"
    ) |> andThen
    (\s -> 
        Parser.map (\i -> (s,XmlInt i))
            (Parser.int 
                |. symbol ("</"++s++">")
            )
    )

xmlFloat : Parser (String, XmlTag)
xmlFloat =
    ( succeed identity 
        |. symbol "<"
        |= Parser.getChompedString (Parser.chompIf Char.isAlpha)
        |. symbol ">"
    ) |> andThen
    (\s -> 
        Parser.map (\i -> (s,XmlFloat i))
            (Parser.float 
                |. symbol ("</"++s++">")
            )
    )

xmlString : Parser (String, XmlTag)
xmlString =
    ( succeed identity 
        |. symbol "<"
        |= Parser.getChompedString (Parser.chompIf Char.isAlpha)
        |. symbol ">"
    ) |> andThen
    (\s ->
        Parser.map (\str -> (s, XmlString str))
            (Parser.getChompedString (Parser.chompUntil ("</"++s++">"))
                |. symbol ("</"++s++">")
            )
    )

xmlComment : Parser ()
xmlComment =
    Parser.multiComment "<!--" "-->" Parser.NotNestable
        |. Parser.symbol "-->"