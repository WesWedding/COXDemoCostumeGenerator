module COXCostume  
(  Costume(..),  
   CostumePart(..),
   outputCostume,
   Color,
   Scale,
   ScaleTriple
)  where

type Scale = Float
type ScaleTriple = (Scale, Scale, Scale)
type ColorVal = Integer
type Color = (ColorVal, ColorVal, ColorVal)

newline = ['\n']
emptyStr = "\"\""


-- Show our types cleaned up a bit; without the parens
showColor :: Color -> String
showColor (a,b,c) = (show a) ++", " ++ (show b) ++", " ++ (show c)

showScales :: ScaleTriple -> String
showScales (a,b,c) = (show a) ++", " ++ (show b) ++", " ++ (show c)

-- Sometimes an empty string needs to be more than just an empty string...
showStr :: String -> String
showStr "" = "\"\""
showStr s = s

data CostumePart = CostumePart { partName :: String
                               , geometry :: String
                               , texture1 :: String
                               , texture2 :: String
                               , displayName :: String
                               , regionName :: String
                               , bodySetName :: String
                               , color1 :: Color
                               , color2 :: Color
                               , color3 :: Color
                               , color4 :: Color
                               } deriving Show

data Costume = Costume { costumeName :: String
		       , scale :: Scale
                       , boneScale :: Scale
                       , chestScale :: Scale
                       , waistScale :: Scale
                       , headScales :: ScaleTriple
                       , browScales :: ScaleTriple
                       , cheekScales :: ScaleTriple
                       , chinScales :: ScaleTriple
                       , craniumScales :: ScaleTriple
                       , jawScales :: ScaleTriple
                       , noseScales :: ScaleTriple
                       , skinColor :: Color
                       , numParts  :: Integer
                       , bodyType :: Integer
                       , costumeParts :: [CostumePart]
                       } deriving Show

outputCostume :: Costume -> String
outputCostume c = outputHeader
                      ++ showCostume c
                      ++ outputFooter

showCostume :: Costume -> String
showCostume c = "Scale " ++ show (scale c) ++ newline
                ++ "BoneScale " ++ show (boneScale c) ++ newline
                ++ "ChestScale " ++ show (chestScale c) ++ newline
                ++ "WaitScale " ++ show (waistScale c) ++ newline
                ++ "HeadScales " ++ showScales (headScales c) ++ newline
                ++ "BrowScales " ++ showScales (browScales c) ++ newline
                ++ "CheekScales " ++ showScales (cheekScales c) ++ newline
                ++ "ChinScales " ++ showScales (chinScales c) ++ newline
                ++ "CraniumScales " ++ showScales (craniumScales c) ++ newline
                ++ "JawScales " ++ showScales (jawScales c) ++ newline
                ++ "NoseScales " ++ showScales (noseScales c) ++ newline
                ++ "SkinColor " ++ showColor (skinColor c) ++ newline
                ++ "NumParts " ++ show (numParts c) ++ newline
                ++ "BodyType " ++ show (bodyType c) ++ newline
                ++ (foldr (++) "" . map showCostumePart) (costumeParts c)

showCostumeParts :: [CostumePart] -> String
showCostumeParts p = undefined

showCostumePart :: CostumePart -> String
showCostumePart p = "CostumePart \"\"" ++ newline
                    ++ "{" ++ newline
                    ++ "\tGeometry " ++ (geometry p) ++ newline
                    ++ "\tTexture1 " ++ (texture1 p) ++ newline
                    ++ "\tTexture2 " ++ (texture2 p) ++ newline
                    ++ "\tDisplayName " ++ showStr (displayName p) ++ newline
                    ++ "\tRegionName " ++ showStr (regionName p) ++ newline
                    ++ "\tBodySetName " ++ (bodySetName p) ++ newline
                    ++ "\tColor1  " ++ showColor (color1 p) ++ newline
                    ++ "\tColor2  " ++ showColor (color2 p) ++ newline
                    ++ "\tColor3  " ++ showColor (color3 p) ++ newline
                    ++ "\tColor4  " ++ showColor (color4 p) ++ newline
                    ++ "}" ++ newline ++ newline ++ newline

outputHeader = ['{', '\n']
outputFooter = ['\n','}']


blankCostumePart = CostumePart "" "Tight" "leather_03" "Camo" "P887196332" "Lower Body" "Tight" (2,  0,  153) (255,  253,  0) (0,  0,  0) (0,  0,  0)

blankCostumeList = [blankCostumePart | y <- [1..28]]

testCostume1 = Costume "TestCostume1" (-3.249) 0.86 0.52 0.37 (0,  0,  0) (0,  0,  0) (0,  0,  0) (0,  0,  0) (0,  0,  0) (0,  0,  0) (0,  0,  0) (212,  137,  114) 28 4 blankCostumeList

testDemoLinesJustCostume = ["0   3203 NEW \"Wes The Mess\"",
	"0   3203 COSTUME 4 7289d4 -3.249228 0.860000 0.000000 0.000000 0.520000 0.370000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000",
	"0   3203 PARTSNAME Tight leather_03 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Tight skin_tank_01a skin_Tank_01b 00990002 00cc0100",
	"0   3203 PARTSNAME V_HUGE_HEAD.GEO/GEO_Head_V_Asym_Standard !v_face_skin_head_10 Small_Mask_1 0000fdff 00000000",
	"0   3203 PARTSNAME Small skin_Small_01a skin_Small_01b 00990002 00cc0100",
	"0   3203 PARTSNAME Smooth combat_01 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Style_01 Style_01a Style_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Glasses_05 Glasses_01a Glasses_01b 00000000 00000000",
	"0   3203 PARTSNAME Tight base W 0000fdff 00009998",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Beard_Goatee_01 Beard_01a Beard_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000"]


testInputLines1 = ["0   3202 MOV READY 0",
	"0   3202 HP 10710.00",
	"0   3202 HPMAX 10710.00",
	"0   3203 Player",
	"0   3203 NEW \"Wes The Mess\"",
	"0   3203 COSTUME 4 7289d4 -3.249228 0.860000 0.000000 0.000000 0.520000 0.370000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000",
	"0   3203 PARTSNAME Tight leather_03 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Tight skin_tank_01a skin_Tank_01b 00990002 00cc0100",
	"0   3203 PARTSNAME V_HUGE_HEAD.GEO/GEO_Head_V_Asym_Standard !v_face_skin_head_10 Small_Mask_1 0000fdff 00000000",
	"0   3203 PARTSNAME Small skin_Small_01a skin_Small_01b 00990002 00cc0100",
	"0   3203 PARTSNAME Smooth combat_01 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Style_01 Style_01a Style_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Glasses_05 Glasses_01a Glasses_01b 00000000 00000000",
	"0   3203 PARTSNAME Tight base W 0000fdff 00009998",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Beard_Goatee_01 Beard_01a Beard_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000"]

testInputLines2 = ["0   3202 MOV READY 0",
	"0   3202 HP 10710.00",
	"0   3202 HPMAX 10710.00",
	"0   3203 Player",
	"0   3203 NEW \"Wes The Mess\"",
	"0   3203 COSTUME 4 7289d4 -3.249228 0.860000 0.000000 0.000000 0.520000 0.370000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000",
	"0   3203 PARTSNAME Tight leather_03 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Tight skin_tank_01a skin_Tank_01b 00990002 00cc0100",
	"0   3203 PARTSNAME V_HUGE_HEAD.GEO/GEO_Head_V_Asym_Standard !v_face_skin_head_10 Small_Mask_1 0000fdff 00000000",
	"0   3203 PARTSNAME Small skin_Small_01a skin_Small_01b 00990002 00cc0100",
	"0   3203 PARTSNAME Smooth combat_01 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Style_01 Style_01a Style_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Glasses_05 Glasses_01a Glasses_01b 00000000 00000000",
	"0   3203 PARTSNAME Tight base W 0000fdff 00009998",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Beard_Goatee_01 Beard_01a Beard_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3202 HP 10710.00",
	"0   3202 HPMAX 10710.00",
	"0   3203 Player",
	"0   3203 NEW \"Wes The Mess\"",
	"0   3203 COSTUME 4 7289d4 -3.249228 0.860000 0.000000 0.000000 0.520000 0.370000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000",
	"0   3203 PARTSNAME Tight leather_03 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Tight skin_tank_01a skin_Tank_01b 00990002 00cc0100",
	"0   3203 PARTSNAME V_HUGE_HEAD.GEO/GEO_Head_V_Asym_Standard !v_face_skin_head_10 Small_Mask_1 0000fdff 00000000",
	"0   3203 PARTSNAME Small skin_Small_01a skin_Small_01b 00990002 00cc0100",
	"0   3203 PARTSNAME Smooth combat_01 Camo 00990002 0000fdff",
	"0   3203 PARTSNAME Style_01 Style_01a Style_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Glasses_05 Glasses_01a Glasses_01b 00000000 00000000",
	"0   3203 PARTSNAME Tight base W 0000fdff 00009998",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME Beard_Goatee_01 Beard_01a Beard_01b 00ffac00 00990002",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000",
	"0   3203 PARTSNAME none none none 00000000 00000000"]