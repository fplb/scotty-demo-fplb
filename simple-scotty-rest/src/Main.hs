{-# LANGUAGE OverloadedStrings #-}
-- https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON , ToJSON)
import GHC.Generics
import Web.Scotty


data Voyager = Voyager {voyagerId :: Int , voyagerName :: String } deriving (Show, Generic)
instance ToJSON Voyager
instance FromJSON Voyager


scot :: Voyager
scot =  Voyager { voyagerId = 1, voyagerName = "scotty" }

numberOne :: Voyager
numberOne = Voyager { voyagerId = 2, voyagerName = "Number One" }

allVoyagers :: [Voyager]
allVoyagers = [scot, numberOne]

matchesId :: Int -> Voyager -> Bool
matchesId id voyager = voyagerId voyager == id


main = do
  putStrLn "Starting server"
  scotty 5000 $ do
    get "/beam/:name" $ do
      name <- param "name"
      text ("beam" <> name <> "!")

    get "/voyagers" $ do
      json allVoyagers

    get "/voyagers/:id" $ do
      id <- param "id"
      json (filter (matchesId id) allVoyagers)

    post "/voyagers" $ do
      voyager <- jsonData :: ActionM Voyager
      json voyager


