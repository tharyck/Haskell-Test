{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module File (lerArquivo) where

import Testes as Teste
import Test.HUnit
import Data.List.Split
import GHC.Generics
import System.IO as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

data Output = Output { matricula :: String, falhas :: Int,
                    passaram :: Int, totalTestes :: Int, excecoes :: Int} deriving (Show, Generic, ToJSON)


lerArquivo :: String -> IO()
lerArquivo matricula = do
  --s <- runTestTT $ test $ mconcat [Testes.tests ]
  s <- runTestTT Teste.tests
  generateResult matricula s

generateResult :: String -> Counts -> IO ()
generateResult fileName str = do
  let formattedMatricula = splitOn "." fileName
  let matricula = formattedMatricula !! 0
  let totalTestes = cases str
  let passaram = (tried str) - errors str - failures str
  let excecoes = errors str
  let falhas = failures str
  let output = Output { matricula = matricula, falhas = falhas, passaram = passaram,
                          totalTestes = totalTestes,  excecoes = excecoes}
  I.writeFile "result.json" (encodeToLazyText output)
  T.putStrLn "Worked"