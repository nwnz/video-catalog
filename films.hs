import System.Cmd
import System.Environment
import System.Directory
import System.FilePath.Posix

import Control.Monad

import Data.List
import Data.Char
import qualified Codec.Binary.UTF8.String as UTF8

-- myFilmDir ::FilePath
-- myFilmDir = UTF8.encodeString $ "/media/doc/Videos/"

--- . and ..
isWrongDir filename = filename /= "." && filename /= ".."

recursiveTraversalDir :: FilePath -> IO [FilePath]
recursiveTraversalDir dir = do filesRaw <- getDirectoryContents dir
                               let files = map (\x -> dir ++ "/" ++ x)
                                           $ filter (isWrongDir) filesRaw
                               dirs <- filterM (doesDirectoryExist) files
                               newDirectoryContent <- mapM (recursiveTraversalDir) dirs
                               return (dirs ++ (concat newDirectoryContent))

--   files <-getContentDir
--   directories = filter (isDirectory) files
--   newDirectoriesContent = map (recursiveTraversalDir) directories
--   return $ concat newDirectoriesContent

decode = UTF8.decodeString

toLowStr = map toLower

videoFormats = ["avi", "mkv", "mp4"]

isVideoFormat fileName = or $ map (\ext -> ext `isInfixOf` name) videoFormats
  where name = fileName

filterAviMkv :: String -> [FilePath] -> [FilePath]
filterAviMkv searchString list =
  filter (\x -> toLowStr searchString `isInfixOf` toLowStr x)
  $ filter (\x -> isVideoFormat x) list

showWithNumbers list = concatMap (\(x,y) -> show x ++ ") " ++ y ++ "\n")
            $ zip [1..numFiles] list
  where numFiles = length list

getPlayerProg = "vlc"

getChosenNumber "" max = Nothing

getChosenNumber str max | all isDigit str
                          && num < max
                          && num >= 0 = Just num
  where num = (read str ::Int) - 1

getChosenNumber str max = Nothing

getFiles fileDir = do filesRaw <- getDirectoryContents fileDir
                      let files = map (\x -> fileDir ++ "/" ++ x) $ filter (isWrongDir) filesRaw
                      return files

quoteFileName "" = ""

quoteFileName (x:xs) = case x == '\'' of
  True -> "'\\''" ++ quoteFileName xs
  False ->  x : quoteFileName xs

main = do args <- getArgs
          let argDirs = args
          putStrLn "Поиск файлов. Подождите..."
          dirs <- mapM (recursiveTraversalDir) argDirs
          filesM <- mapM getFiles (argDirs ++ concat dirs)
          case filesM of
            [] -> fail "use: films dir1 dir2 ... dir_n"
            _  -> putStr ""

          let files = concat filesM
          putStrLn ("Найдено файлов: " ++ show (length files))
          putStrLn "Строка поиска(просто Enter для вывода всех файлов): "
          searchString <- getLine

          let filteredFiles = filterAviMkv searchString files
          let numFiles = length filteredFiles
          if numFiles /= 0
            then do putStrLn $ showWithNumbers filteredFiles
                    putStrLn "Введите номер запускаемого файла: "
                    line <- getLine
                    case getChosenNumber line numFiles of
                      Nothing -> putStrLn "Номер файла неверный"
                      Just num -> do let chosenFile = filteredFiles !! num
                                     let fileName = getPlayerProg ++ " "
                                                    ++ "'"
                                                    ++ quoteFileName chosenFile
                                                    ++ "'"
                                     putStrLn $ fileName
                                     errorNum <- system fileName
                                     print "ok"
            else putStrLn "Файлов с таким набором символов не найдено "