-- ASSIGNMENT 5 

-- Help-code from https://github.com/dicander/help_files_mastery_test 
-- Define the Vec3 type
data Vec3 = Vec3 Double Double Double
  deriving (Show, Eq)

-- Implement vector operations using an instance of Num
instance Num Vec3 where
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2) -- Not standard vector multiplication
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n = Vec3 (fromInteger n) (fromInteger n) (fromInteger n)

-- PART I
-- Calculating the reflection of incoming ray using formula: r = d − 2(d · n)n
-- Using helper functions that calculate dot product and multiplication with scalar
reflect :: Vec3 -> Vec3 -> Vec3
reflect incoming normal =
    incoming - scalarMul (2*dotProduct incoming normal) normal 

-- Helper function that calculates the dot product of two Vec3
-- Takes two Vec3 as parameters and uses pattern matching to calculate
dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 a b c) (Vec3 d e f) = a*d + b*e + c*f

-- Helper function that calculates multiplication of scalar and Vec3
-- Takes scalar and a Vec3 as parameters and uses pattern matching
scalarMul :: Double -> Vec3 -> Vec3
scalarMul scalar (Vec3 a b c) = Vec3 (a*scalar) (b*scalar) (c*scalar)

-- PART II
-- Removing rays with negative z-value. Uses pattern matching. 
-- Takes a list of Vec3 as parameter and returns the list with negative z filtered
-- Uses lambda-function with a Vec3, it finds z with pattern match and applies the z≥0 rule on ALL elements in rays list
removeNegativeZ :: [Vec3] -> [Vec3]
removeNegativeZ rays = filter (\(Vec3 _ _ z) -> z >= 0) rays 

-- PART III
-- Takes a normal vector and a list of incoming rays (Vec3),
-- reflects each ray over the normal, and then removes any rays with a negative z-component.
-- It returns a new list of reflected rays with non-negative z-components.
-- Uses dot-notation to first apply map (reflect normal) on incomingRays and then apply removeNegativeZ
reflectAndRemoveNegativeZ :: Vec3 -> [Vec3] -> [Vec3]
reflectAndRemoveNegativeZ normal incomingRays = (removeNegativeZ . map (reflect normal)) incomingRays

main :: IO()
main = do
    -- let result1 = reflect (Vec3 0.5 (-0.5) 0.0) (Vec3 0 1 0)
    -- print result1

    -- let result2 = reflect (Vec3 0.2 (-0.7) 0.0) (Vec3 0 1 0)
    -- print result2

    -- let result3 = reflect (Vec3 1.3 (-1.7) 1.3) (Vec3 1 1 1)
    -- print result3

    -- let result4 = removeNegativeZ [Vec3 0.2 0.3 (-0.2), Vec3 1 2 3, Vec3 (-0.2) 0.3 0.4]
    -- print result4

    -- let result5 = removeNegativeZ [Vec3 0.2 0.3 0.3, Vec3 1 2 (-4),Vec3 (-0.2) 0.3 0.4]
    -- print result5

    -- let result6 = reflectAndRemoveNegativeZ (Vec3 1 1 1) [(Vec3 0.2 0.3 (-0.2)), (Vec3 1 2 3), (Vec3 (-0.2) 0.3 0.4)]
    -- print result6

    -- let result7 = reflectAndRemoveNegativeZ (Vec3 0 1 0) [(Vec3 0.2 0.3 (-0.2)), (Vec3 1 2 3), (Vec3 (-0.2) 0.3 0.4)]
    -- print result7

    let result8 = reflectAndRemoveNegativeZ (Vec3 1 0 1) [(Vec3 0.2 0.3 1), (Vec3 1 2 3), (Vec3 (-0.2) 0.3 (-0.4))]
    print result8