import COXCostume
import Numeric

{- 

TODO: 
	*  Parse the "NEW" line better; strip out quotes, handle multiple words
	*  Fix the save function so it maps correctly to the costume list instead of the placeholder behavior
	*  Get the RegionName value set during demo parsing (use the update function?)
	*  If strings for a costume part are blank, we need to add an empty string to the output: \"\"
	*  Possibly missing 2 parts at the bottom of the file?  Seems like extraneous empty parts
-}



-- Let's save all of these Costumes into their own separate files, woo hoo!
saveCostumes :: FilePath -> IO [Costume] -> IO ()
saveCostumes fp cs = do l <- cs
                        let t = map outputCostume l
                        let c = (l !! 3)
                        writeFile (fp ++ costumeName c ++".costume") (outputCostume c) -- Temporary, need to fix this so it works over the whole list

-- Let's get all of the costumes in a demo file!  Hooray!
parseDemoFile :: FilePath -> IO [Costume]
parseDemoFile p = do s <- readFile p
                     let g = (map parseCostumeLines . getCostumeChunks . lines) s
                     return g

-- Let's get a list of costume chunks!
-- Given 1000s of lines, return a list of each costume's strings [[CostumeStrings], [CostumeStrings]...]
getCostumeChunks :: [String] -> [[String]]
getCostumeChunks []     = []
getCostumeChunks (x:y:xs) = case isCostume y of  -- NEW lines followed by COSTUME lines means we've got a costume!
                            True -> [take 28 (x:y:xs)] ++ getCostumeChunks (drop 27 (y:xs)) -- The costume data is 28 lines in the demo file
                            False -> getCostumeChunks (y:xs)
	where isCostume b = isCostumeLine (words b)
getCostumeChunks _      = []

-- Given a series of lines, check that the second one is, indeed, the start of information we need
-- Parse the costume lines that follow
getCostume :: [String] -> Maybe Costume
getCostume (x:y:ys) = if isCostumeLine (words y)
			then Just (parseCostumeLines (y:ys))
			else Nothing


-- All of the lines for a costume from a cohdemo file should be here.
-- Starting with the "NEW" line and "COSTUME" lines
-- The PARTSNAME lines will be handed off to another function 
parseCostumeLines :: [String] -> Costume
parseCostumeLines (n:c:parts) = Costume { costumeName = ((!!3) . words) n,
                                          scale = read (costTokens !! 5) :: Float,
                                          boneScale = read (costTokens !! 6) :: Float,
                                          chestScale = read (costTokens !! 7) :: Float,
                                          waistScale = read (costTokens !! 8) :: Float,
                                          headScales = (read (costTokens !! 9):: Float, read (costTokens !! 10):: Float, read (costTokens !! 11):: Float),
                                          browScales = (read (costTokens !! 9):: Float, read (costTokens !! 10):: Float, read (costTokens !! 11):: Float),
                                          cheekScales = (read (costTokens !! 9):: Float, read (costTokens !! 10):: Float, read (costTokens !! 11):: Float),
                                          chinScales = (read (costTokens !! 9):: Float, read (costTokens !! 10):: Float, read (costTokens !! 11):: Float),
                                          craniumScales = (read (costTokens !! 9):: Float, read (costTokens !! 10):: Float, read (costTokens !! 11):: Float),
                                          jawScales = (read (costTokens !! 9):: Float, read (costTokens !! 10):: Float, read (costTokens !! 11):: Float),
                                          noseScales = (read (costTokens !! 9):: Float, read (costTokens !! 10):: Float, read (costTokens !! 11):: Float),
                                          skinColor = hexToColor (costTokens !! 4),
                                          numParts = 28,
                                          bodyType = read (costTokens !! 3) :: Integer,
                                          costumeParts = map parseCostumePartsLine parts  
                                        }
	where costTokens = words c

parseCostumePartsLine :: String -> CostumePart
parseCostumePartsLine xs = CostumePart { partName = "",
                                         geometry = partsTokens !! 3,
                                         texture1 = partsTokens !! 4,
                                         texture2 = partsTokens !! 5,
                                         displayName = "P2039232", -- Can be any string, doesn't break things if it's wrong
                                         regionName = "Head",
                                         bodySetName = "Standard",
                                         color1 = hexToColor (drop 2 (partsTokens !! 6)), -- We don't need the first two digits
                                         color2 = hexToColor (drop 2 (partsTokens !! 7)), -- First two digits aren't needed her, either
                                         color3 = (0,0,0),  -- Appears unused by current implementations 
                                         color4 = (0,0,0)  -- Appears unused by current implementations 
                                        }
	where partsTokens = words xs
parseCostumePartsLine _             = undefined

isCostumeLine :: [String] -> Bool
isCostumeLine (_:_:"COSTUME":xs) = True
isCostumeLine _ = False


-- This function expects a single line, broken into separate words
isPlayerLine :: [String] -> Bool
isPlayerLine [_, _, "Player"] = True
isPlayerLine w                = False

-- In the demo, the skinColor attribute is represented in hex, but in the .cohcostume format,
-- is is represented as a series of 3 numbers.
-- Example:  7289d4 is three hex values:  72(114) 89(137) d4(212) , and they're in reverse order compared to what we want
hexToColor :: String -> Color
hexToColor (a:b:c:d:e:f:_) = ((fst . head . readHex) (e:[f]) , (fst . head . readHex) (c:[d]), (fst . head . readHex) (a:[b]))
hexToColor _               = (0,0,0)