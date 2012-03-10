import System.Cmd
import System.Environment
import System.Directory
import System.FilePath.Posix

import Data.List
import Data.Char
import qualified Codec.Binary.UTF8.String as UTF8

-- myFilmDir ::FilePath
-- myFilmDir = UTF8.encodeString $ "/media/doc/Videos/"

-- recursiveTraversalDir dir = 
--   files <-getContentDir
--   directories = filter (isDirectory) files
--   newDirectoriesContent = map (recursiveTraversalDir) directories
--   return $ concat newDirectoriesContent

decode = UTF8.decodeString

toLowStr = map toLower

filterAviMkv searchString list = filter (\x -> toLowStr searchString `isInfixOf` toLowStr(decode x))
               $ filter (\x ->  
                          "mkv" `isInfixOf`  decode x
                          || "avi" `isInfixOf`  decode x) list

showWithNumbers list = concatMap (\(x,y) -> show x ++ ") " ++ decode y ++ "\n") 
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
                      let files = map (fileDir ++) filesRaw
                      return files

main = do args <- getArgs
          filesM <- mapM getFiles args
          case filesM of
            [] -> fail "use: films dir1 dir2 ... dir_n"
            _  -> putStr "" 
              
          let files = concat filesM
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
                                                    ++ chosenFile
                                                    ++ "'"
                                     errorNum <- system fileName
                                     print "ok"
            else print "Результаты"