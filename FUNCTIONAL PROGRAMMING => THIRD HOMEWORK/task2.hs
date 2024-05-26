getParentSize :: FileSystem -> Name -> Size
getParentSize fs filename
  | null sizes = -1
  | otherwise = minimum sizes
    where 
     sizes = findFile fs filename []


findFile :: FileSystem -> Name -> [Size] -> [Size]
findFile (File name _) _ acc = acc
findFile (Directory dirName children) filename acc = results
  where 
  results = concatMap (\child -> findFileHelper filename child (sumDirectory (Directory dirName children) : acc)) children
  findFileHelper filename (File name size) acc
    | name == filename = acc
    | otherwise = []
  findFileHelper filename dir@(Directory _ children) acc =
    findFile dir filename acc

sumDirectory :: FileSystem -> Size
sumDirectory (File _ size) = size
sumDirectory (Directory _ children) = sum (map sumDirectory children)

type Command = String
type Size = Int
type Name = String

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]

fileSystem :: FileSystem
fileSystem = Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]

data FileSystem = Directory Name [FileSystem] | File Name Size
 deriving (Eq, Show)