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
        , GainEquipment [ "The common red medic outfit bearing the official white medic seal of the Hegemony.", "A less than ideally stocked medkit." ]
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
        , GainRefresh [ "" ]
        , GainSkills [ Skulk, Scrap ]
        , GainDomains [ Criminal, HighSociety ]
        , GainEquipment [ "Light body armour (Armour 2)", "Climbing gear and ropes" ]
        ]
    , coreAbilities =
        [ { name = "Surprise Infiltration"
          , flavor = Just "Nothing can keep you out."
          , boons = []
          , text = "Once per session, insert yourself into a situation where you are not currently present, so long as there’s some conceivable way you could get in there."
          }
        , { name = "Tactician"
          , flavor = Just "You are a trained infiltrator, and others would do well to heed your words."
          , boons = []
          , text = "When you enter a dangerous situation, you can name up to three features or opportunities that your allies can take advantage of. The first time you or an ally uses an opportunity, they roll with mastery (for example: cover with a good view of the battlefield, an exit, a badly-guarded door, a stack of barrels, etc.)."
          }
        , { name = "Just a Scratch"
          , flavor = Just "You word your diagnosis as kindly as possible."
          , boons = [ GainSkills [ Deceive ] ]
          , text = "When you or an ally suffer Body fallout, you may roll Deceive+Science. On a success, the effects of the fallout can be ignored until the end of the situation."
          }
        ]
    }
