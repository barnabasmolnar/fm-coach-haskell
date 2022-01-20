module Formula where

import           Parser.ParserUtils             ( AttributeValuePair
                                                , Coach
                                                  ( Coach
                                                  , attacking
                                                  , defending
                                                  , determination
                                                  , discipline
                                                  , distribution
                                                  , fitness
                                                  , handling
                                                  , mental
                                                  , motivating
                                                  , shotStopping
                                                  , tactical
                                                  , technical
                                                  )
                                                )

type Formula = Coach -> Int

data Rating = Rating
  { category    :: String
  , weightedSum :: Int
  , stars       :: Float
  }
  deriving Show

exampleCoach :: Coach
exampleCoach = Coach 15 18 14 16 12 18 20 11 14 2 3 2

cityCoach :: Coach
cityCoach = Coach 20 20 20 20 20 20 20 20 20 20 20 20

manUtdCoach :: Coach
manUtdCoach = Coach 1 1 1 1 1 1 1 1 1 1 1 1

baseFormula :: [(Formula, Int)] -> Formula
baseFormula pairs coach =
  let ddm      = [(determination, 2), (discipline, 2), (motivating, 2)]
      allPairs = ddm ++ pairs
  in  foldl (\acc (fn, weight) -> acc + fn coach * weight) 0 allPairs

fitnessFormula :: Formula
fitnessFormula = baseFormula [(fitness, 9)]

defendingTacticalFormula :: Formula
defendingTacticalFormula = baseFormula [(defending, 6), (tactical, 3)]

defendingTechnicalFormula :: Formula
defendingTechnicalFormula = baseFormula [(defending, 6), (technical, 3)]

attackingTacticalFormula :: Formula
attackingTacticalFormula = baseFormula [(attacking, 6), (tactical, 3)]

attackingTechnicalFormula :: Formula
attackingTechnicalFormula = baseFormula [(attacking, 6), (technical, 3)]

possessionTacticalFormula :: Formula
possessionTacticalFormula = baseFormula [(tactical, 6), (mental, 3)]

possessionTechnicalFormula :: Formula
possessionTechnicalFormula = baseFormula [(technical, 6), (mental, 3)]

gkShotStoppingFormula :: Formula
gkShotStoppingFormula = baseFormula [(shotStopping, 9)]

gkHandlingFormula :: Formula
gkHandlingFormula = baseFormula [(handling, 6), (distribution, 3)]

myClamp :: Ord a => (a, a) -> a -> a
myClamp (lower, upper) = max lower . min upper

starRating :: Int -> Float
starRating x = myClamp (0.5, 5) $ fromIntegral (x `div` 30 + 1) / 2

coachRatings :: Coach -> [Rating]
coachRatings coach = map
  calculateRating
  [ ("Defending Tactical"  , defendingTacticalFormula)
  , ("Defending Technical" , defendingTechnicalFormula)
  , ("Attacking Tactical"  , attackingTacticalFormula)
  , ("Attacking Technical" , attackingTechnicalFormula)
  , ("Possession Tactical" , possessionTacticalFormula)
  , ("Possession Technical", possessionTechnicalFormula)
  , ("Fitness"             , fitnessFormula)
  , ("GK Shot Stopping"    , gkShotStoppingFormula)
  , ("GK Handling"         , gkHandlingFormula)
  ]
 where
  calculateRating (label, formula) =
    let weightedSum = formula coach
    in  Rating label weightedSum (starRating weightedSum)
