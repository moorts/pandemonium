{-# LANGUAGE DeriveGeneric #-}

module Lib (
    pandemize,
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics

data Config = Config
    { sep :: String
    , stringKeys :: [String]
    , floatKeys :: [String]
    , intKeys :: [String]
    }
    deriving (Show, Generic)

allKeys :: Config -> [String]
allKeys c = stringKeys c ++ floatKeys c ++ intKeys c

instance FromJSON Config

data Data = Data
    { stringData :: M.Map String [String]
    , floatData :: M.Map String [Float]
    , intData :: M.Map String [Int]
    }
    deriving (Show, Generic)

instance ToJSON Data

skipValue :: String -> Bool
skipValue = any (not . (`elem` skipValues))
  where
    skipValues = [' ', ',']

parseKey :: String -> String -> Maybe (String, [String])
parseKey string key
    | parts == [] = Nothing
    | prefix == key = Just $ (key, values)
    | otherwise = Nothing
  where
    parts = splitOn ":" string
    prefix = head parts
    tempValues = concat (tail parts)
    values = filter skipValue (splitOn " " tempValues)

parseLine :: [String] -> String -> Maybe (String, [String])
parseLine cKeys string = listToMaybe $ mapMaybe (parseKey string) cKeys

parseBlock :: Config -> [String] -> M.Map String [String]
parseBlock config
    | otherwise = M.fromList . mapMaybe (parseLine cKeys)
  where
    cKeys = allKeys config

parse :: Config -> String -> [M.Map String [String]]
parse config = map ((parseBlock config) . lines) . filter (not . null) . splitOn (sep config)

toData :: Config -> M.Map String [String] -> Data
toData config block = Data (strings block) (floats block) (ints block)
  where
    strings = M.filterWithKey (\k _ -> k `elem` stringKeys config)
    floats = M.map (map read) . M.filterWithKey (\k _ -> k `elem` floatKeys config)
    ints = M.map (map read) . M.filterWithKey (\k _ -> k `elem` intKeys config)

toDataList :: Config -> [M.Map String [String]] -> [Data]
toDataList config blocks = map (toData config) blocks

parseFile :: Config -> FilePath -> IO ([M.Map String [String]])
parseFile config path = do
    s <- readFile path
    return $ parse config s

pandemize :: FilePath -> FilePath -> IO ()
pandemize configPath dataPath = do
    Just config <- decodeFileStrict configPath
    rawData <- parseFile config dataPath
    B.writeFile "./out.json" $ encodePretty (toDataList config rawData)

someFunc :: IO ()
someFunc = do
    pandemize "./data/example_config.json" "./data/example.txt"
