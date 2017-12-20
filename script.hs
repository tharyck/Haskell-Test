import System.Process
import System.Directory
import System.IO as T

main = do
  alunos <- getDirectoryContents "alunos"
  generateResult "matricula.txt" alunos

generateResult :: String -> [FilePath] -> IO ()
generateResult fileName alunos = do
  let readingFiles alunos = if (length alunos) >= 1
                                then do
                                  runningScript (last alunos)
                                  readingFiles (init alunos :: [FilePath])
                                else
                                  return()
  readingFiles alunos
  putStrLn "Worked Final"

runningScript :: FilePath -> IO ()
runningScript aluno = do
  if (aluno /= ".." && aluno /= ".")
    then do
      let fileInfo = "./alunos/" ++ aluno
      dataInfo <- T.readFile fileInfo
      writeToFile dataInfo
      let cmd = "ghci GenerateFile.hs -e \"main \\\"" ++ aluno ++ "\\\"\""
      callCommand cmd
      putStrLn "Script Worked"
  else
    putStrLn "Worked2"

writeToFile :: String -> IO()
writeToFile str = do
  T.writeFile "MultisetMap.hs" str
  putStrLn "Write to File Worked"
  -- callCommand "ghci GenerateFile.hs -e \"main \\\"323\\\"\""
  -- putStrLn "Worked2"
