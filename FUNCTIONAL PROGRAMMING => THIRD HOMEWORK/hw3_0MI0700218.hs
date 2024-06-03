import Data.List
import Data.Char 

main :: IO ()
main = do
  print $ generateFileSystem commands == Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]
  print $ getParentSize (generateFileSystem commands) "i" == 584
  print $ getParentSize (generateFileSystem commands) "g" == 94853
  print $ getParentSize (generateFileSystem commands) "b.txt" == 48381165
  print $ getParentSize (generateFileSystem commands) "abc" == -1
 

type Command = String
type Size = Int
type Name = String

data FileSystem = Directory Name [FileSystem] | File Name Size 
  deriving (Eq, Show)

generateFileSystem :: [Command] -> FileSystem
generateFileSystem (command:commands) = fst $ helper (Directory "/" []) ["/"] commands
  where
    helper :: FileSystem -> [String] -> [Command] -> (FileSystem, [Command])
    helper fileSystem currentPath [] = (fileSystem, [])
    helper fileSystem currentPath (command:commands)
     | isPrefixOf "$ cd " command = helper fileSystem (updatePath currentPath (drop 5 command)) commands
     | isPrefixOf "$ ls" command = helper fileSystem currentPath commands
     | isPrefixOf "dir " command = helper (addDirectory fileSystem currentPath directoryName) currentPath commands
     | otherwise = helper (addFile fileSystem currentPath fileName fileSize) currentPath commands
      where
        directoryName = drop 4 command
        (fileSize, fileName) = let (sizeStr:nameParts) = words command in (read sizeStr, unwords nameParts)
    
    addDirectory :: FileSystem -> [String] -> Name -> FileSystem
    addDirectory (Directory dirName contents) ["/"] newDir = Directory dirName (sortFileSystem (Directory newDir [] : contents))
    addDirectory (Directory dirName contents) (p:ps) newDir
     | dirName == p = Directory dirName (sortFileSystem (map (\c -> addDirectory c ps newDir) contents ++ if null ps then [Directory newDir []] else []))
     | otherwise = Directory dirName contents
    addDirectory fs _ _ = fs
    
    addFile :: FileSystem -> [String] -> Name -> Size -> FileSystem
    addFile (Directory dirName contents) ["/"] newFile size = Directory dirName (sortFileSystem (File newFile size : contents))
    addFile (Directory dirName contents) (p:ps) newFile size
     | dirName == p = Directory dirName (sortFileSystem (map (\c -> addFile c ps newFile size) contents ++ if null ps then [File newFile size] else []))
     | otherwise = Directory dirName contents
    addFile fs _ _ _ = fs


sortFileSystem :: [FileSystem] -> [FileSystem]
sortFileSystem = sortOn (\fs -> (isFile fs, map toLower (getName fs)))
  where
    isFile (File _ _) = True
    isFile _ = False
    getName (Directory name _) = name
    getName (File name _) = name

updatePath :: [String] -> String -> [String]
updatePath _ "/" = ["/"]
updatePath currentPath ".." = if length currentPath > 1 then init currentPath else currentPath
updatePath currentPath dir = currentPath ++ [dir]

-- Task 2

directorySize :: FileSystem -> Size
directorySize (File _ size) = size
directorySize (Directory _ contents) = sum (map directorySize contents)

getParentSize :: FileSystem -> Name -> Size
getParentSize fileSystem name = getSize fileSystem
 where
    getSize (File _ _) = -1
    getSize dir@(Directory _ contents) =
      let sizes = map getSize contents
          fileInDir = any (hasFile name) contents
      in if fileInDir
         then minimum (directorySize dir : filter (/= -1) sizes)
         else -1

    hasFile name (File fname _) = name == fname
    hasFile name (Directory _ contents) = any (hasFile name) contents

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]
