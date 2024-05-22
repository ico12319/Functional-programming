main :: IO()
main = do
    print $ cP [Present, Late, Present, Absent, Present, Present, Present,Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent,Late, Present] == True
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present,Absent, Present] == False


cP = canPass (1,2)
type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present deriving (Eq)
type StudentRecord = [Attendance]



canPass :: Criterion -> (StudentRecord -> Bool)
canPass (maxMisses, maxLates) record = helper maxMisses maxLates 0 0 record
 where
    helper :: Misses -> Lates -> Misses -> Lates -> StudentRecord -> Bool
    helper _ _ _ _ [] = True
    helper maxMisses maxLates currentMisses currentLates (x:xs)
     | x == Absent && currentMisses < maxMisses = helper maxMisses maxLates (currentMisses + 1) 0 xs
     | x == Late && currentLates < maxLates = helper maxMisses maxLates currentMisses (currentLates + 1) xs
     | x == Present = helper maxMisses maxLates currentMisses 0 xs
     | otherwise = False

