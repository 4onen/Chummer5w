module Qualities exposing (..)

import Parser exposing (Parser,(|.),(|=),spaces,succeed)

import Utilities
import Xml.Decode.Advanced as Xml

type alias Quality =
    { id : String
    , name : String
    , karmaCost : Int
    , category : Category
    , implemented : Bool
    , charGenOnly : Bool
    , mutant : Bool
    , includeInLimit : String --What is this???
    , hide : Bool --Seriously, what?
    , doublecareer : Maybe Bool --I don't even...
    , limitTimesTaken : Int
    , firstlevelbonus : String
    , requirements : String
    , bonuses : List QualityBonus
    , forbiddenCoQualities : List String
    , source : String
    , sourcePage : Int
    , nameOnPage : Maybe String
    }

type Category
    = Positive
    | Negative

type QualityBonus
    = Ambidextrous
    | SelectSkill Int
    | NativeLanguageLimitModifier Int
    | Notoriety Int
    | SpecificSkill String Int
    | SelectText
    | SelectAttributes String
    | ConditionMonitor String
    | LimitModifier Limit Int
    | SpecificAttribute String Int
    | SpellResistanceModifier Int
    | SelectMentorSpirit
    | Memory Int
    | PhysicalCMRecovery Int
    | StunCMRecovery Int
    | PathogenResist ContaminantRoute Int
    | ToxinResist ContaminantRoute Int
    | DamageResistance Int
    | MartialArt String
    | EnableAttribute String
    | EnableTab String
    | UnlockSkills String
    | AddGear String
    | SkillCategory String Int
    | LifestyleCost Int
    | BlockSkillGroupDefaulting
    | DisableSpecializationEffects
    | CyberwareTotalESSMultiplier Int
    | DisableBioware
    | SkillCategoryKarmaCostMultiplier String Int
    | SkillCategoryPointCostMultiplier String Int
    | SkillGroupCategoryDisable String
    | StreetCredMultiplier Int
    | SkillCategorySpecializationKarmaCostMultiplier String Int
    | SkillGroupCategoryKarmaCostMultiplier String Int
    | FocusBindingKarmaCost String

type Limit
    = Mental
    | Physical
    | Social

type ContaminantRoute 
    = Contact
    | Ingestion
    | Inhalation
    | Injection

fileParser : Parser (List String)
fileParser =
    Parser.map (Debug.toString >> List.singleton)
        <| Xml.xmlFile
{--    succeed List.reverse
        |. initParser
        |= Parser.loop [] qualitiesListParser--}

qualitiesListParser : List String -> Parser (Parser.Step (List String) (List String))
qualitiesListParser l =
    Parser.oneOf
        [ succeed (\q -> Parser.Loop (q::l))
            |= parseBetween 
                ( Parser.keyword "<quality>")
                ( Parser.keyword "</quality>")
                ( qualityParser )
            |. spaces
        , succeed (Parser.Loop l)
            |. Parser.lineComment "<!--" --"-->" Parser.NotNestable
            |. spaces
        , succeed (Parser.Done l)
            |. Parser.symbol "</qualities>"
            |. spaces
        ]

qualityParser : Parser String
qualityParser = 
    Parser.map Debug.toString
    <| succeed Quality
        |= stringBetween
            "<id>"
            "</id>"
        |. spaces
        |= stringBetween "<name>" "</name>"
        |. spaces
        |= intBetween "<karma>" "</karma>"
        |. spaces
        |= categoryParser
        |. spaces
        |= Parser.oneOf
            [ succeed False
                |. Parser.symbol "<implemented>False</implemented>"
            , succeed False
            ]
        |= Parser.oneOf
            [ succeed True
                |. Parser.symbol "<chargenonly />"
            , succeed False
            ]
        |. spaces
        |= Parser.oneOf
            [ succeed True
                |. Parser.symbol "<mutant>True</mutant>"
            , succeed False
            ]
        |. spaces
        |= Parser.oneOf
            [ stringBetween "<includeinlimit>" "</includeinlimit>" 
            , succeed ""
            ]
        |. spaces
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.symbol "<hide />"
            , succeed False
            ]
        |. spaces
        |= Parser.oneOf
            [ Parser.succeed (Just False)
                |. Parser.symbol "<doublecareer>False</doublecareer>"
            , succeed Nothing
            ] 
        |. spaces
        |. Parser.oneOf 
            [ succeed ()
                |. Parser.symbol "<contributetolimit>False</contributetolimit>"
            , succeed ()
            ]
        |. spaces
        |. Parser.oneOf 
            [ succeed ()
                |. Parser.symbol "<onlyprioritygiven />"
            , succeed ()
            ]
        |. spaces
        |= Parser.oneOf 
            [ succeed Utilities.maxInt
                |. Parser.symbol "<limit>False</limit>"
            , intBetween "<limit>" "</limit>"
            , succeed 1
            ]
        |. spaces
        |= Parser.oneOf
            [ stringBetweenTags "firstlevelbonus"
            , succeed ""
            ]
        |. spaces
        |= Parser.oneOf
            [ stringBetween "<required>" "</required>"
            , Parser.succeed ""
            ]
        |. spaces
        |= Parser.oneOf
            [ succeed []
                |. Parser.symbol "<bonus />"
            , succeed identity
                |. Parser.symbol "<bonus>"
                |. spaces
                |= Parser.loop [] bonusListParser
            , succeed []
            ]
        |. spaces
        |. Parser.oneOf --Bad bad hack!!!
            [ stringBetween "<required>" "</required>"
            , Parser.succeed ""
            ]
        |. spaces
        |= Parser.oneOf [Parser.map (List.singleton) (stringBetween "<forbidden>" "</forbidden>"), succeed []]
        |. spaces
        |. Parser.oneOf --Bad bad hack!!!
            [ stringBetween "<required>" "</required>"
            , Parser.succeed ""
            ]
        |. spaces
        |= stringBetween "<source>" "</source>"
        |. spaces
        |= intBetween "<page>" "</page>"
        |. spaces
        |= Parser.oneOf
            [ Parser.map Just (stringBetweenTags "nameonpage")
            , succeed Nothing]
        |. spaces

bonusListParser : List QualityBonus -> Parser (Parser.Step (List QualityBonus) (List QualityBonus))
bonusListParser l = 
    Parser.oneOf 
        [ simpleBonus Ambidextrous "ambidextrous" l
        , succeed (\i -> Parser.Loop (SelectSkill i :: l))
            |= parseBetween
                (Parser.symbol "<selectskill>")
                (Parser.symbol "</selectskill>")
                (intBetween "<max>" "</max>"
                    |. spaces
                )
        , simpleIntBonus NativeLanguageLimitModifier "nativelanguagelimit" l
        , simpleIntBonus Notoriety "notoriety" l
        , succeed (\(s,i) -> Parser.Loop (SpecificSkill s i :: l))
            |= parseBetween
                ( Parser.symbol "<specificskill>")
                ( Parser.symbol "</specificskill>")
                ( succeed (\s i -> (s,i))
                    |= stringBetween "<name>" "</name>"
                    |. spaces
                    |= intBetween "<bonus>" "</bonus>"
                    |. spaces
                )
        , simpleBonus SelectText "selecttext" l
        , todoStringBonus SelectAttributes "selectattributes" l
        , todoStringBonus ConditionMonitor "conditionmonitor" l
        , simpleIntBonus (LimitModifier Mental) "mentallimit" l
        , simpleIntBonus (LimitModifier Physical) "physicallimit" l
        , simpleIntBonus (LimitModifier Social) "sociallimit" l
        , succeed (\(s,i) -> Parser.Loop (SpecificAttribute s i :: l))
            |= parseBetween
                ( Parser.symbol "<specificattribute>")
                ( Parser.symbol "</specificattribute>")
                ( succeed (\s i -> (s,i))
                    |= stringBetweenTags "name"
                    |. spaces
                    |= intBetween "<max>" "</max>"
                    |. spaces
                )
        , simpleIntBonus SpellResistanceModifier "spellresistance" l
        , simpleBonus SelectMentorSpirit "selectmentorspirit" l
        , simpleIntBonus Memory "memory" l
        , simpleIntBonus PhysicalCMRecovery "physicalcmrecovery" l
        , simpleIntBonus StunCMRecovery "stuncmrecovery" l
        , simpleIntBonus (PathogenResist Contact) "pathogencontactresist" l
        , simpleIntBonus (PathogenResist Ingestion) "pathogeningestionresist" l
        , simpleIntBonus (PathogenResist Inhalation) "pathogeninhalationresist" l
        , simpleIntBonus (PathogenResist Injection) "pathogeninjectionresist" l
        , simpleIntBonus (ToxinResist Contact) "toxincontactresist" l
        , simpleIntBonus (ToxinResist Ingestion) "toxiningestionresist" l
        , simpleIntBonus (ToxinResist Inhalation) "toxininhalationresist" l
        , simpleIntBonus (ToxinResist Injection) "toxininjectionresist" l
        , simpleIntBonus DamageResistance "damageresistance" l
        , todoStringBonus MartialArt "martialart" l
        , todoStringBonus EnableAttribute "enableattribute" l
        , todoStringBonus EnableTab "enabletab" l
        , todoStringBonus UnlockSkills "unlockskills" l
        , todoStringBonus AddGear "addgear" l
        , succeed (\(s,i) -> Parser.Loop (SkillCategory s i :: l))
            |= parseBetween
                ( Parser.symbol "<skillcategory>")
                ( Parser.symbol "</skillcategory>")
                ( succeed (\s i -> (s,i))
                    |= stringBetweenTags "name"
                    |. spaces
                    |= intBetween "<bonus>" "</bonus>"
                    |. spaces
                )
        , simpleIntBonus LifestyleCost "lifestylecost" l
        , simpleBonus BlockSkillGroupDefaulting "blockskillgroupdefaulting" l
        , simpleBonus DisableSpecializationEffects "disablespecializationeffects" l
        , succeed (Parser.Loop (l))
            |. stringBetween "<selectskill" "</selectskill>"
        , simpleIntBonus CyberwareTotalESSMultiplier "cyberwaretotalessmultiplier" l
        , simpleBonus DisableBioware "disablebioware" l
        , succeed (\(s,i) -> Parser.Loop (SkillCategoryKarmaCostMultiplier s i :: l))
            |= parseBetween
                ( Parser.symbol "<skillcategorykarmacostmultiplier>")
                ( Parser.symbol "</skillcategorykarmacostmultiplier>")
                ( succeed (\s i -> (s,i))
                    |= stringBetweenTags "name"
                    |. spaces
                    |= intBetween "<val>" "</val>"
                    |. spaces
                )
        , todoStringBonus SkillGroupCategoryDisable "skillgroupcategorydisable" l
        , succeed (\(s,i) -> Parser.Loop (SkillCategoryPointCostMultiplier s i :: l))
            |= parseBetween
                ( Parser.symbol "<skillcategorypointcostmultiplier>")
                ( Parser.symbol "</skillcategorypointcostmultiplier>")
                ( succeed (\s i -> (s,i))
                    |= stringBetweenTags "name"
                    |. spaces
                    |= intBetween "<val>" "</val>"
                    |. spaces
                )
        , simpleIntBonus StreetCredMultiplier "streetcredmultiplier" l
        , succeed (\(s,i) -> Parser.Loop (SkillCategorySpecializationKarmaCostMultiplier s i :: l))
            |= parseBetween
                ( Parser.symbol "<skillcategoryspecializationkarmacostmultiplier>")
                ( Parser.symbol "</skillcategoryspecializationkarmacostmultiplier>")
                ( succeed (\s i -> (s,i))
                    |= stringBetweenTags "name"
                    |. spaces
                    |= intBetween "<val>" "</val>"
                    |. spaces
                )
        , succeed (\(s,i) -> Parser.Loop (SkillGroupCategoryKarmaCostMultiplier s i :: l))
            |= parseBetween
                ( Parser.symbol "<skillgroupcategorykarmacostmultiplier>")
                ( Parser.symbol "</skillgroupcategorykarmacostmultiplier>")
                ( succeed (\s i -> (s,i))
                    |= stringBetweenTags "name"
                    |. spaces
                    |= intBetween "<val>" "</val>"
                    |. spaces
                )
        , todoStringBonus FocusBindingKarmaCost "focusbindingkarmacost" l
        , succeed (Parser.Done l)
            |. Parser.symbol "</bonus>"
        ]
        |. spaces

simpleBonus : QualityBonus -> String -> List QualityBonus -> Parser (Parser.Step (List QualityBonus) (List QualityBonus))
simpleBonus b s l =
    succeed (Parser.Loop (b :: l))
        |. Parser.symbol ("<"++s++" />")

simpleIntBonus : (Int -> QualityBonus) -> String -> List QualityBonus -> Parser (Parser.Step (List QualityBonus) (List QualityBonus))
simpleIntBonus b s l =
    succeed (\i -> Parser.Loop (b i :: l))
        |= intBetween ("<"++s++">") ("</"++s++">")

todoStringBonus : (String -> QualityBonus) -> String -> List QualityBonus -> Parser (Parser.Step (List QualityBonus) (List QualityBonus))
todoStringBonus b s l =
    succeed (\str -> Parser.Loop (b str :: l))
        |= stringBetweenTags s


categoryParser : Parser Category
categoryParser =
    Parser.oneOf
        [ succeed Positive
            |. Parser.symbol "<category>Positive</category>"
        , succeed Negative
            |. Parser.symbol "<category>Negative</category>"
        ]
    {--(stringBetween "<category>" "</category>") 
        |> Parser.andThen
        (\c ->
            case c of
                "Positive" -> succeed Positive
                "Negative" -> succeed Negative
                notCategory -> Parser.problem <| "Expecting quality category, found \""++notCategory++"\"."
        )--}



initParser : Parser ()
initParser =
    succeed ()
        |. Parser.chompUntil "<version>"
        |. parseBetween 
            (Parser.symbol "<version>")
            (Parser.symbol "</version>")
            ( succeed identity
                |. Parser.int
                |. spaces
            )
        |. spaces
        |. Parser.chompUntil "<categories>"
        |. parseBetween 
            ( Parser.symbol "<categories>")
            ( Parser.symbol "</categories>")
            ( succeed identity
                |. categoryParser
                |. spaces
                |. categoryParser
                |. spaces
            )
        |. spaces
        |. Parser.symbol "<qualities>"
        |. spaces

parseBetween : Parser open -> Parser close -> Parser mid -> Parser mid
parseBetween open close mid =
    succeed identity
        |. open 
        |. spaces
        |= mid
        |. close

stringBetween : String -> String -> Parser String
stringBetween open close =
    succeed identity
        |. Parser.symbol open
        |. spaces
        |= Parser.getChompedString (Parser.chompUntil close)
        |. Parser.symbol close

stringBetweenTags : String -> Parser String
stringBetweenTags s =
    stringBetween ("<"++s++">") ("</"++s++">")

intBetween : String -> String -> Parser Int
intBetween open close =
    succeed identity
        |. Parser.symbol open
        |. spaces
        |= Parser.oneOf 
            [ Parser.int
            , succeed negate
                |. Parser.symbol "-"
                |= Parser.int
            ]
        |. spaces
        |. Parser.symbol close