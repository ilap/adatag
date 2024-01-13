{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | This module contains some belper functions for code testing (not unittests)
module IntegriTree.Utils where

import IntegriTree.IntegriTree
import IntegriTree.Val
import PlutusTx.Prelude hiding (toList)
import Prelude qualified as Haskell

-- Function to update a node in the tree and append a new node
update :: BuiltinByteString -> IntegriTree -> Maybe IntegriTree
update _ LabeledEmpty = Nothing
update _ node@(LabeledNode _ left right)
  | hasRight node = update "a" right
  | hasLeft node = update "a" left
  | otherwise = Nothing

hasLeft :: IntegriTree -> Bool
hasLeft LabeledEmpty = False
hasLeft (LabeledNode _ l _) = case l of
  LabeledNode {} -> True
  LabeledEmpty -> False

hasRight :: IntegriTree -> Bool
hasRight LabeledEmpty = False
hasRight (LabeledNode _ _ r) = case r of
  LabeledNode {} -> True
  LabeledEmpty -> False

---------------------------------------------------------------------------------------------------
------------------------------- HELPER FUNCTIONS FOR TESTIMG --------------------------------------
-- ghci> import IntegriTree.Tree
-- ghci> import Data.Maybe (fromMaybe)
-- ghci> :set -XOverloadedStrings
-- ghci> proof $ fromMaybe LabeledEmpty $ findNodeByVal sampleTree (IntegriTree.Tree.Val "g" "h")
-- c39e8c972fa8
-- ghci> import IntegriTree.Tree
-- ghci> :set -XOverloadedStrings
-- ghci> let m = mkProof "z" sampleTree
-- ghci> import Data.Maybe (fromJust)
-- ghci> let TreeProof ( HashNode v l r, p) = fromJust mp
-- ghci> ghci> combineThreeHashes (hash (a v <> b v)) l r
-- 2fe3a179f298d5769faa557c29ee6f7e0d48d98b7e6ea8b92d3d1a4141db9860
-- Tracing the code
-- rh = Debug.Trace.trace ("Hash" ++ Haskell.show (hash "")) $ hash ""
sampleVals :: [Val]
sampleVals =
  [ Val 1 "`" "c"
  , Val 2 "z" "{"
  , Val 3 "f" "g"
  , Val 4 "j" "l"
  , Val 5 "d" "e"
  , Val 6 "y" "z"
  , Val 7 "g" "h"
  , Val 8 "l" "o"
  , Val 9 "u" "ul"
  , Val 10 "s" "u"
  , Val 11 "x" "y"
  , Val 12 "e" "f"
  , Val 13 "o" "s"
  , Val 14 "i" "j"
  , Val 15 "v" "x"
  , Val 16 "c" "d"
  , Val 17 "h" "i"
  , Val 18 "ul" "v" -- change 9 to Val 9 "u" "ul"
  ]

sampleVals2 :: [Val]
-- sampleVals2 = [Val "`" "adam_1969", Val "susan012" "sylvan_37", Val "mike-a210" "mystic_33", Val "adam_1969" "alex_omega", Val "angel012" "aqua_69", Val "pedre_33" "phoenix_12", Val "luna_23" "luna_55", Val "alex_omega" "angel012", Val "bella_8" "casc-ada", Val "harmony-42" "harmony-88", Val "david_123" "dreamer_22", Val "violet-22" "whispering_wolf", Val "james5" "jason_13", Val "whispering_wolf" "whispers_12", Val "emma_11" "enchanted_66", Val "enchanted_66" "enigma_12", Val "johnny_9" "journey_23", Val "mystic_33" "mystic_8", Val "phoenix_77" "quasar_33", Val "sapphire_8" "seraph_22", Val "stardust_23" "susan012", Val "olivia_99" "pedre_33", Val "journeyer_44" "legend_22", Val "chris_7" "david_123", Val "tranquility_12" "tranquility_14", Val "melody_56" "meridian_12", Val "celestial_81" "chris_7", Val "infinity_3" "james5", Val "nebula_17" "nebula_88", Val "legend_22" "lullaby_22", Val "obsidian_10" "oliver_6", Val "aurora_92" "bella_8", Val "grace_14" "harmony-14", Val "cascade-19" "cascade-27", Val "michael_8" "mike-a210", Val "zen_8" "zenith_11", Val "nova_45" "obsidian_10", Val "luminary_6" "luminary_67", Val "harmony-14" "harmony-42", Val "nebula_88" "nebulous_61", Val "seraph_27" "seraph_33", Val "aqua_69" "astral001", Val "ethereal_11" "ethereal_22", Val "cascade-27" "cascade_12", Val "oliver_6" "olivia_99", Val "whispers_22" "whispers_36", Val "tempest_44" "tempest_67", Val "trinity_8" "violet-22", Val "jason_13" "johnny_9", Val "astral_77" "astral_9", Val "illusion_9" "infinity_19", Val "solstice_21" "stardust_23", Val "sylvan_37" "tempest_16", Val "eternal_4" "ethereal_11", Val "journey_23" "journeyer_44", Val "serenade_15" "serendipity_7", Val "enigma_18" "enigma_37", Val "dreamweaver_25" "dreamweaver_36", Val "drizzle_7" "drizzle_91", Val "aurora_45" "aurora_6", Val "harmony-88" "illusion_76", Val "solitude_14" "solstice_18", Val "infinity_19" "infinity_3", Val "zenith_99" "zephyr_12", Val "mariner_23" "melody_56", Val "nocturne_42" "nova_45", Val "ethereal_8" "etherme", Val "phoenix_55" "phoenix_76", Val "mystique_12_1" "mystique_12_2", Val "luminous_32" "luna_14", Val "cascade_77" "cascade_8", Val "zephyr_4" "zephyr_55", Val "whispers_66" "whispers_67", Val "ember_18" "emma_11", Val "seraphim_9" "serenade_15", Val "lunar_37" "lunar_61", Val "meridian_12" "michael_8", Val "nebulous_61" "nocturnal1985", Val "celestial_14" "celestial_19", Val "quasar_33" "quasar_36", Val "radiant_76" "radiant_77", Val "drifter_8" "drizzle_7", Val "solace_22" "solace_92", Val "echoes_43" "echoes_8", Val "tranquility_67" "tranquility_9", Val "siren_12" "siren_77", Val "astral_9" "aurora-02", Val "zenith_11" "zenith_77", Val "dreamer_88" "dreamer_99", Val "serendipity_7" "serenity_14", Val "cascade_55" "cascade_77", Val "luminosity_14" "luminous_32", Val "mystic_8" "mystique_12_1", Val "drizzle_91" "echo-es01", Val "zenith_77" "zenith_99", Val "lullaby_5" "lullaby_7", Val "ethereal_22" "ethereal_33", Val "whispers_61" "whispers_66", Val "enigma_37" "enigma_67", Val "lunar_13" "lunar_37", Val "tempest_16" "tempest_44", Val "phoenix_29" "phoenix_55", Val "radiant_44" "radiant_76", Val "serenity_23" "serenity_33", Val "solstice_18" "solstice_21", Val "illusion_76" "illusion_77", Val "celestial_19" "celestial_67", Val "cascade_9" "celestial_12", Val "tranquility_36" "tranquility_67", Val "zephyr_88" "{", Val "whispers_12" "whispers_22", Val "seraph_57" "seraphim_9", Val "luminary_33" "luminary_6", Val "dreamweaver_21" "dreamweaver_25", Val "aurora_6" "aurora_92", Val "echoes_8" "ember_18", Val "ethereal_47" "ethereal_8", Val "solace_92" "solitude_14", Val "astral_17" "astral_77", Val "quasar_55" "quasar_67", Val "radiant_33" "radiant_44", Val "luna_14" "luna_23", Val "nocturnal_61" "nocturne_42", Val "mystique_12_2" "mystique_12_3", Val "siren_77" "siri001", Val "cascade_12" "cascade_36", Val "serenity_44" "shoelace004", Val "tempest_8" "tempike", Val "dreamer_99" "dreamweaver_21", Val "lullaby_22" "lullaby_5", Val "ethereal_37" "ethereal_47", Val "whispers_67" "whispers_8", Val "enigma_12" "enigma_18", Val "lunar_61" "lunar_9", Val "phoenix_76" "phoenix_77", Val "rada-007" "rada007", Val "illusion_77" "illusion_8", Val "celestial_67" "celestial_81", Val "cascade_36" "cascade_55", Val "tranquility_9" "trinity_8", Val "zephyr_12" "zephyr_4", Val "whispers_88" "zen_8", Val "seraph_33" "seraph_57", Val "luminary_14" "luminary_33", Val "dreamweaver_36" "dreamweaver_67", Val "aurora-02" "aurora002", Val "echo-es01" "echoes01", Val "ethereal_33" "ethereal_37", Val "shoelace1" "siren_12", Val "astral001" "astral1", Val "quasar_36" "quasar_55", Val "radiant_88" "sapphire_8", Val "luna_55" "luna_67", Val "nocturnal_12" "nocturnal_61", Val "mystique_12_3" "mystique_12_4", Val "siri001-02" "solace_22", Val "cascade_8" "cascade_9", Val "serenity_33" "serenity_44", Val "tempike" "tranquility_12", Val "dreamer_22" "dreamer_88", Val "lullaby_7" "luminary_14", Val "etherme" "etherme2", Val "whispers_36" "whispers_61", Val "enigma_67" "eternal_4", Val "lunar_9" "mariner_23", Val "phoenix_12" "phoenix_29", Val "radiant_77" "radiant_88", Val "illusion_8" "illusion_9", Val "celestial_12" "celestial_14", Val "casc-ada" "cascada9", Val "tranquility_14" "tranquility_36", Val "zephyr_55" "zephyr_88", Val "whispers_8" "whispers_88", Val "seraph_22" "seraph_27", Val "luminary_67" "luminosity_14", Val "dreamweaver_67" "drifter_8", Val "aurora002" "aurora_45", Val "echoes01" "echoes_43", Val "etherme2" "grace_14", Val "shoelace004" "shoelace1", Val "astral1" "astral_17", Val "quasar_67" "rada-007", Val "rada007" "radiant_33", Val "luna_67" "lunar_13", Val "nocturnal1985" "nocturnal_12", Val "mystique_12_4" "nebula_17", Val "siri001" "siri001-02", Val "cascada9" "cascade-19", Val "serenity_14" "serenity_23", Val "tempest_67" "tempest_8"]
sampleVals2 = [Val 1 "`" "adam_1969", Val 2 "susan012" "sylvan_37", Val 3 "mike-a210" "mystic_33", Val 4 "adam_1969" "alex_omega", Val 5 "angel012" "aqua_69", Val 6 "pedre_33" "phoenix_12", Val 7 "luna_23" "luna_55", Val 8 "alex_omega" "angel012", Val 9 "bella_8" "casc-ada", Val 10 "harmony-42" "harmony-88", Val 11 "david_123" "dreamer_22", Val 12 "violet-22" "whispering_wolf", Val 13 "james5" "jason_13", Val 14 "whispering_wolf" "whispers_12", Val 15 "emma_11" "enchanted_66", Val 16 "enchanted_66" "enigma_12", Val 17 "johnny_9" "journey_23", Val 18 "mystic_33" "mystic_8", Val 19 "phoenix_77" "quasar_33", Val 20 "sapphire_8" "seraph_22", Val 21 "stardust_23" "susan012", Val 22 "olivia_99" "pedre_33", Val 23 "journeyer_44" "legend_22", Val 24 "chris_7" "david_123", Val 25 "tranquility_12" "tranquility_14", Val 26 "melody_56" "meridian_12", Val 27 "celestial_81" "chris_7", Val 28 "infinity_3" "james5", Val 29 "nebula_17" "nebula_88", Val 30 "legend_22" "lullaby_22", Val 31 "obsidian_10" "oliver_6", Val 32 "aurora_92" "bella_8", Val 33 "grace_14" "harmony-14", Val 34 "cascade-19" "cascade-27", Val 35 "michael_8" "mike-a210", Val 36 "zen_8" "zenith_11", Val 37 "nova_45" "obsidian_10", Val 38 "luminary_6" "luminary_67", Val 39 "harmony-14" "harmony-42", Val 40 "nebula_88" "nebulous_61", Val 41 "seraph_27" "seraph_33", Val 42 "aqua_69" "astral001", Val 43 "ethereal_11" "ethereal_22", Val 44 "cascade-27" "cascade_12", Val 45 "oliver_6" "olivia_99", Val 46 "whispers_22" "whispers_36", Val 47 "tempest_44" "tempest_67", Val 48 "trinity_8" "violet-22", Val 49 "jason_13" "johnny_9", Val 50 "astral_77" "astral_9", Val 51 "illusion_9" "infinity_19", Val 52 "solstice_21" "stardust_23", Val 53 "sylvan_37" "tempest_16", Val 54 "eternal_4" "ethereal_11", Val 55 "journey_23" "journeyer_44", Val 56 "serenade_15" "serendipity_7", Val 57 "enigma_18" "enigma_37", Val 58 "dreamweaver_25" "dreamweaver_36", Val 59 "drizzle_7" "drizzle_91", Val 60 "aurora_45" "aurora_6", Val 61 "harmony-88" "illusion_76", Val 62 "solitude_14" "solstice_18", Val 63 "infinity_19" "infinity_3", Val 64 "zenith_99" "zephyr_12", Val 65 "mariner_23" "melody_56", Val 66 "nocturne_42" "nova_45", Val 67 "ethereal_8" "etherme", Val 68 "phoenix_55" "phoenix_76", Val 69 "mystique_12_1" "mystique_12_2", Val 70 "luminous_32" "luna_14", Val 71 "cascade_77" "cascade_8", Val 72 "zephyr_4" "zephyr_55", Val 73 "whispers_66" "whispers_67", Val 74 "ember_18" "emma_11", Val 75 "seraphim_9" "serenade_15", Val 76 "lunar_37" "lunar_61", Val 77 "meridian_12" "michael_8", Val 78 "nebulous_61" "nocturnal1985", Val 79 "celestial_14" "celestial_19", Val 80 "quasar_33" "quasar_36", Val 81 "radiant_76" "radiant_77", Val 82 "drifter_8" "drizzle_7", Val 83 "solace_22" "solace_92", Val 84 "echoes_43" "echoes_8", Val 85 "tranquility_67" "tranquility_9", Val 86 "siren_12" "siren_77", Val 87 "astral_9" "aurora-02", Val 88 "zenith_11" "zenith_77", Val 89 "dreamer_88" "dreamer_99", Val 90 "serendipity_7" "serenity_14", Val 91 "cascade_55" "cascade_77", Val 92 "luminosity_14" "luminous_32", Val 93 "mystic_8" "mystique_12_1", Val 94 "drizzle_91" "echo-es01", Val 95 "zenith_77" "zenith_99", Val 96 "lullaby_5" "lullaby_7", Val 97 "ethereal_22" "ethereal_33", Val 98 "whispers_61" "whispers_66", Val 99 "enigma_37" "enigma_67", Val 100 "lunar_13" "lunar_37", Val 101 "tempest_16" "tempest_44", Val 102 "phoenix_29" "phoenix_55", Val 103 "radiant_44" "radiant_76", Val 104 "serenity_23" "serenity_33", Val 105 "solstice_18" "solstice_21", Val 106 "illusion_76" "illusion_77", Val 107 "celestial_19" "celestial_67", Val 108 "cascade_9" "celestial_12", Val 109 "tranquility_36" "tranquility_67", Val 110 "zephyr_88" "{", Val 111 "whispers_12" "whispers_22", Val 112 "seraph_57" "seraphim_9", Val 113 "luminary_33" "luminary_6", Val 114 "dreamweaver_21" "dreamweaver_25", Val 115 "aurora_6" "aurora_92", Val 116 "echoes_8" "ember_18", Val 117 "ethereal_47" "ethereal_8", Val 118 "solace_92" "solitude_14", Val 119 "astral_17" "astral_77", Val 120 "quasar_55" "quasar_67", Val 121 "radiant_33" "radiant_44", Val 122 "luna_14" "luna_23", Val 123 "nocturnal_61" "nocturne_42", Val 124 "mystique_12_2" "mystique_12_3", Val 125 "siren_77" "siri001", Val 126 "cascade_12" "cascade_36", Val 127 "serenity_44" "shoelace004", Val 128 "tempest_8" "tempike", Val 129 "dreamer_99" "dreamweaver_21", Val 130 "lullaby_22" "lullaby_5", Val 131 "ethereal_37" "ethereal_47", Val 132 "whispers_67" "whispers_8", Val 133 "enigma_12" "enigma_18", Val 134 "lunar_61" "lunar_9", Val 135 "phoenix_76" "phoenix_77", Val 136 "rada-007" "rada007", Val 137 "illusion_77" "illusion_8", Val 138 "celestial_67" "celestial_81", Val 139 "cascade_36" "cascade_55", Val 140 "tranquility_9" "trinity_8", Val 141 "zephyr_12" "zephyr_4", Val 142 "whispers_88" "zen_8", Val 143 "seraph_33" "seraph_57", Val 144 "luminary_14" "luminary_33", Val 145 "dreamweaver_36" "dreamweaver_67", Val 146 "aurora-02" "aurora002", Val 147 "echo-es01" "echoes01", Val 148 "ethereal_33" "ethereal_37", Val 149 "shoelace1" "siren_12", Val 150 "astral001" "astral1", Val 151 "quasar_36" "quasar_55", Val 152 "radiant_88" "sapphire_8", Val 153 "luna_55" "luna_67", Val 154 "nocturnal_12" "nocturnal_61", Val 155 "mystique_12_3" "mystique_12_4", Val 156 "siri001-02" "solace_22", Val 157 "cascade_8" "cascade_9", Val 158 "serenity_33" "serenity_44", Val 159 "tempike" "tranquility_12", Val 160 "dreamer_22" "dreamer_88", Val 161 "lullaby_7" "luminary_14", Val 162 "etherme" "etherme2", Val 163 "whispers_36" "whispers_61", Val 164 "enigma_67" "eternal_4", Val 165 "lunar_9" "mariner_23", Val 166 "phoenix_12" "phoenix_29", Val 167 "radiant_77" "radiant_88", Val 168 "illusion_8" "illusion_9", Val 169 "celestial_12" "celestial_14", Val 170 "casc-ada" "cascada9", Val 171 "tranquility_14" "tranquility_36", Val 172 "zephyr_55" "zephyr_88", Val 173 "whispers_8" "whispers_88", Val 174 "seraph_22" "seraph_27", Val 175 "luminary_67" "luminosity_14", Val 176 "dreamweaver_67" "drifter_8", Val 177 "aurora002" "aurora_45", Val 178 "echoes01" "echoes_43", Val 179 "etherme2" "grace_14", Val 180 "shoelace004" "shoelace1", Val 181 "astral1" "astral_17", Val 182 "quasar_67" "rada-007", Val 183 "rada007" "radiant_33", Val 184 "luna_67" "lunar_13", Val 185 "nocturnal1985" "nocturnal_12", Val 186 "mystique_12_4" "nebula_17", Val 187 "siri001" "siri001-02", Val 188 "cascada9" "cascade-19", Val 189 "serenity_14" "serenity_23", Val 190 "tempest_67" "tempest_8"]

-- Create a sample tree from the list of Vals
sampleTree :: IntegriTree
sampleTree = fromList sampleVals

log2 :: Integer -> Integer
log2 x = if odd x then 0 else (Haskell.floor . Haskell.logBase (2 :: Haskell.Double) . Haskell.fromIntegral) x

printTree :: IntegriTree -> Haskell.IO ()
printTree LabeledEmpty = Haskell.putStr ""
printTree n =
  go n 1 "" level -- npr
  where
    s = size n
    level = (Haskell.floor . Haskell.sqrt . Haskell.fromIntegral) s
    ppr = "+"
    npr = "-"

    go :: IntegriTree -> Integer -> Haskell.String -> Integer -> Haskell.IO ()
    go LabeledEmpty _ _ _ = Haskell.putStr ""
    go node@(LabeledNode val left right) xi p l = do
      let pf = if even xi && xi /= 0 then npr else ppr

      Haskell.putStrLn
        $ p
        ++ pf
        ++ " "
        ++ Haskell.show xi -- printf "%*d" (l * 2 + 1) i
        ++ " ("
        ++ Haskell.show va
        ++ " "
        ++ Haskell.show vb
        ++ ", Valhash: "
        ++ Haskell.show vh
        ++ ", proof: "
        ++ Haskell.show pn
        ++ ", pL: "
        ++ Haskell.show pl
        ++ ", pR: "
        ++ Haskell.show pr
      go left (2 * xi) (p ++ ppr) (l - 1)
      go right (2 * xi + 1) (p ++ ppr) (l - 1)
      where
        (Val _ va vb) = val
        vh = hashVal val
        pn = rootHash node
        pl = rootHash left
        pr = rootHash right
