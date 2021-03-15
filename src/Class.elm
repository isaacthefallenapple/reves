module Class exposing (..)

import Ability exposing (Ability)
import Boon exposing (Boon(..))
import Boon.Domain exposing (Domain(..))
import Boon.Resistance exposing (Resistance(..))
import Boon.Skill exposing (Skill(..))


type alias Class =
    { name : String
    , coreAbilities : List Ability
    , boons : List Boon
    }


classes : List Class
classes =
    [ doc
    , cloak
    , awoken
    ]


doc : Class
doc =
    { name = "Doc"
    , boons =
        [ GainResistance Resolve 2
        , GainResistance Reputation 2
        , GainRefresh [ "Help those who cannot help themselves." ]
        , GainSkills [ Patch, Compel ]
        , GainDomains [ Science, LowSociety ]
        , GainEquipment [ "The common red medic outfit bearing the official white medic seal of the Hegemony", "A less than ideally stocked medkit", "EITHER Scalpel (D3, Concealable) OR Stun gun (D6, One-shot, Stunning)" ]
        ]
    , coreAbilities =
        [ { name = "Medical Attention"
          , flavor = Just "You take some time to check on your allies and stitch them up if need be."
          , boons = []
          , text = "Once per session, help your allies heal, physically and mentally. All allies present may restore 3 stress from Body or Resolve."
          }
        , { name = "Bedside Manner"
          , flavor = Just "People are grateful for your help."
          , boons = []
          , text = "When you heal someone, gain a bond with them until the end of the next day."
          }
        ]
    }


cloak : Class
cloak =
    { name = "Cloak"
    , boons =
        [ GainResistance Shadow 3
        , GainResistance Body 1
        , GainRefresh [ "Show someone they should not have underestimated you" ]
        , GainSkills [ Skulk, Scrap ]
        , GainDomains [ Criminal, HighSociety ]
        , GainEquipment [ "Light body armour (Armour 2)", "EITHER Climbing gear and ropes", "Shoddy rifle (D8, Ranged, Reload, Unreliable) and Knife (D3, Concealable) OR Sword (D6) and Meteor Hammer (D3, Ranged, Stunning)" ]
        ]
    , coreAbilities =
        [ { name = "Surprise Infiltration"
          , flavor = Just "Nothing can keep you out."
          , boons = []
          , text = "Once per session, insert yourself into a situation where you are not currently present, so long as there's some conceivable way you could get in there."
          }
        , { name = "Tactician"
          , flavor = Just "You are a trained infiltrator, and others would do well to heed your words."
          , boons = []
          , text = "When you enter a dangerous situation, you can name up to three features or opportunities that your allies can take advantage of. The first time you or an ally uses an opportunity, they roll with mastery (for example: cover with a good view of the battlefield, an exit, a badly-guarded door, a stack of barrels, etc.)."
          }
        ]
    }


awoken : Class
awoken =
    { name = "Awoken"
    , boons =
        [ GainResistance Resolve 2
        , GainResistance Shadow 1
        , GainResistance Body 1
        , GainRefresh [ "Take something back from those who would oppress you." ]
        , GainEquipment [ "A small Ur-artefact whose powers have not revealed themselves to you", "EITHER A knife (D3, Concealable) OR A staff (D3, Conduit)" ]
        , GainSkills [ Compel, Resist ]
        , GainDomains [ Science, Weirdness ]
        ]
    , coreAbilities =
        [ { name = "Heart's Desire"
          , flavor = Just "You know what people want."
          , boons = []
          , text = "Once per situation, pick an NPC that you can observe for a while. The GM will tell you what they want most of all right now."
          }
        , { name = "Moonlight"
          , flavor = Just "The Way's light shines forth from you."
          , boons = []
          , text = "Your rune glows as brightly as the full moon, casting a calm light into the darkness that cannot be extinguished unless you decide to snuff it, or you fall unconscious."
          }
        ]
    }
