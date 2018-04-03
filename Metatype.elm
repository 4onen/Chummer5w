module Metatype exposing (..)

import Attributes exposing (AttributeObject)

type Metatype 
    = Dwarf (Maybe DwarfMetavariant)
    | Elf (Maybe ElfMetavariant)
    | Human (Maybe HumanMetavariant)
    | Ork (Maybe OrkMetavariant)
    | Troll (Maybe TrollMetavariant)
    | Metasapient Never

type DwarfMetavariant
    = Gnomes
    | Harumen
    | Koborokuru
    | Menehune
    | Querx

type ElfMetavariant
    = Dalakiton
    | Dryad
    | NightOne
    | Wakyambi
    | XapiriThepe

type HumanMetavariant 
    = Nartaki

type OrkMetavariant
    = Hobgoblins
    | Ogres
    | Oni
    | Satyr

type TrollMetavariant
    = Cyclops
    | Fomori
    | Giants
    | Minotaurs



getStats : Metatype -> (AttributeObject, AttributeObject)
getStats metatype =
    case metatype of
        Human Nothing ->
            ( AttributeObject 1 1 1 1 1 1 1 1 2 1 1
            , AttributeObject 6 6 6 6 6 6 6 6 7 6 6
            )
        Human (Just Nartaki) ->
            ( AttributeObject 1 1 1 1 1 1 1 1 1 1 1
            , AttributeObject 6 6 6 6 6 6 6 6 6 6 6
            )
        _ ->
            getStats (Human Nothing)