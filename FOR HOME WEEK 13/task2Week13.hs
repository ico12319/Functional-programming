import Data.List
main :: IO ()
main = do
  print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
  print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
  print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False




consecutiveLates :: StudentRecord -> Int
consecutiveLates record = maximum $ map length $ filter (\x -> head x == Late) $  group record


canPass :: Criterion -> (StudentRecord -> Bool)
canPass (maxMisses, maxLates) = (\record -> length (filter (==Absent) record) <= maxMisses && consecutiveLates record <= maxLates)




type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present 
 deriving (Show, Eq)
type StudentRecord = [Attendance]

cP = canPass (1, 2)
