{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

import Test.HUnit
import Data.List.Split
import GHC.Generics
import System.IO as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)
import tests

data Output = Output { matricula :: [Char], falhas :: Int,
                    passaram :: Int, totalTestes :: Int, excecoes :: Int} deriving (Show, Generic, ToJSON)

main = do
  s <- T.readFile "matricula.txt"
  generateResult "matricula.txt" s

generateResult :: String -> String -> IO ()
generateResult fileName str = do
  let replace xs initChar replChar = concat [if x == initChar then replChar else [x] | x <- xs]
  let formattingString = replace str ',' ""
  let formattingString1 = replace formattingString '}' ""
  let formattedString = splitOn " " formattingString1
  let formattedMatricula = splitOn "." fileName

  let matricula = formattedMatricula !! 0
  let totalTestes = read (formattedString !! 3) :: Int
  let passaram = read (formattedString !! 6) :: Int
  let excecoes = read (formattedString !! 9) :: Int
  let falhas = read (formattedString !! 12) :: Int
  let output = Output { matricula = matricula, falhas = falhas, passaram = passaram,
                          totalTestes = totalTestes,  excecoes = excecoes}
  I.writeFile "result.json" (encodeToLazyText output)
  T.putStrLn "Worked"
