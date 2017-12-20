{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import File as FileReader

main :: IO ()
main = do
    csvData <- BL.readFile "matricula.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (matricula, pasta) ->
            FileReader.lerArquivo $ (pasta ++ matricula)