module Magicality exposing (..)

type Magicality
    = Magician
    | AspectedMagician
    | Adept
    | MysticAdept
    | Technomancer

getBaseMagic : Magicality -> Int -> Int
getBaseMagic mag i =
    case (i,mag) of
        (4,_) -> 0
        
        (3,Magician) -> 0
        (3,MysticAdept) -> 0
        (3,Technomancer) -> 0
        (3,Adept) -> 2
        (3,AspectedMagician) -> 2
        
        (2,Magician) -> 3
        (2,MysticAdept) -> 4
        (2,Technomancer) -> 0
        (2,Adept) -> 4
        (2,AspectedMagician) -> 3
        
        (1,Magician) -> 4
        (1,MysticAdept) -> 4
        (1,Technomancer) -> 0
        (1,Adept) -> 6
        (1,AspectedMagician) -> 5
        
        (0,Magician) -> 6
        (0,MysticAdept) -> 6
        (0,Technomancer) -> 0
        (0,Adept) -> -1
        (0,AspectedMagician) -> -1
        
        _ -> 84

getBaseResonance : Magicality -> Int -> Int
getBaseResonance mag i =
    case (i,mag) of
        (4,_) -> 0

        (3,_) -> 0

        (2,Technomancer) -> 3
        (2,_) -> 0

        (1,Technomancer) -> 4
        (1,_) -> 0

        (0,Technomancer) -> 6
        (0,_) -> 0
        
        _ -> 84

viewMagicRating : Magicality -> Int -> String
viewMagicRating mag i =
    case (i,mag) of
        (4,_) -> 
            "Mundane -- 0 Magic, 0 Resonance"
        
        (3,Magician) ->
            "Magician -- too weak, 0 Magic"
        (3,MysticAdept) ->
            "Mystic Adept -- too weak, 0 Magic"
        (3,Technomancer) ->
            "Technomancer -- too weak, 0 Resonance"
        (3,Adept) ->
            "Adept -- 2 Magic"
        (3,AspectedMagician) ->
            "Aspected Magician -- 2 Magic"
        
        (2,Magician) ->
            "Magician -- 3 Magic"
        (2,MysticAdept) ->
            "Magician -- 3 Magic"
        (2,Technomancer) ->
            "Technomancer -- 3 Resonance"
        (2,Adept) ->
            "Adept -- 4 Magic"
        (2,AspectedMagician) ->
            "Aspected Magician -- 3 Magic"
        
        (1,Magician) ->
            "Magician -- 4 Magic"
        (1,MysticAdept) ->
            "Magician -- 4 Magic"
        (1,Technomancer) ->
            "Technomancer -- 4 Resonance"
        (1,Adept) ->
            "Adept -- 6 Magic"
        (1,AspectedMagician) ->
            "Aspected Magician -- 5 Magic"
        
        (0,Magician) ->
            "Magician -- 6 Magic"
        (0,MysticAdept) ->
            "Magician -- 6 Magic"
        (0,Technomancer) ->
            "Technomancer -- 6 Resonance"
        (0,Adept) ->
            "Adept -- Too Strong!!!"
        (0,AspectedMagician) ->
            "Aspected Magician -- Too Strong!!!"
        
        _ ->
            "Yeah, uh, I'm not sure how you got this selection. GJ?"

viewMagicClass : Magicality -> Int -> String
viewMagicClass mag i =
    case (i,mag) of
        (4,_) -> 
            "Mundane -- 0 Magic, 0 Resonance"
        
        (3,Magician) ->
            "Magician -- Too weak, 0 Magic"
        (3,MysticAdept) ->
            "Mystic Adept -- too weak, 0 Magic"
        (3,Technomancer) ->
            "Technomancer -- too weak, 0 Resonance"
        (3,Adept) ->
            "Adept -- 2 Magic"
        (3,AspectedMagician) ->
            "Aspected Magician -- 2 Magic"
        
        (2,Magician) ->
            "Magician -- 3 Magic, 5 spells"
        (2,MysticAdept) ->
            "Magician -- 3 Magic, 5 spells"
        (2,Technomancer) ->
            "Technomancer -- 3 Resonance, 1 Complex Form"
        (2,Adept) ->
            "Adept -- 4 Magic, one Rading 2 Active skill"
        (2,AspectedMagician) ->
            "Aspected Magician -- 3 Magic, one Rating 2 Magical skill group"
        
        (1,Magician) ->
            "Magician -- 4 Magic, two Rating 4 Magical skills, 7 spells"
        (1,MysticAdept) ->
            "Magician -- 4 Magic, two Rating 4 Magical skills, 7 spells"
        (1,Technomancer) ->
            "Technomancer -- 4 Resonance, two Rating 4 Resonance skills, 2 Complex Form"
        (1,Adept) ->
            "Adept -- 6 Magic, one Rading 4 Active skill"
        (1,AspectedMagician) ->
            "Aspected Magician -- 5 Magic, one Rating 4 Magical skill group"
        
        (0,Magician) ->
            "Magician -- 6 Magic, two Rating 5 Magical skills, 10 spells"
        (0,MysticAdept) ->
            "Magician -- 6 Magic, two Rating 5 Magical skills, 10 spells"
        (0,Technomancer) ->
            "Technomancer -- 6 Resonance, two Rating 5 Resonance skills, 5 Complex Form"
        (0,Adept) ->
            "Adept -- Too Strong!!!"
        (0,AspectedMagician) ->
            "Aspected Magician -- Too Strong!!!"
        
        _ ->
            "Yeah, uh, I'm not sure how you got this selection. GJ?"