{-# LANGUAGE NoImplicitPrelude #-}

module Env where

import qualified Configuration.Dotenv as Dotenv
import Control.Monad.Trans.Except
import qualified Data.Text as Text
import qualified Database.MongoDB as MongoDB
import qualified Database.Redis as Redis
import Relude
import qualified System.Envy as Envy

data Env = Env
    { port :: Int
    , mongoPipe :: MongoDB.Pipe
    , redisConn :: Redis.Connection
    }

getEnv :: ExceptT String IO Env
getEnv = do
    void $ Dotenv.onMissingFile (Dotenv.loadFile Dotenv.defaultConfig) (pure [])

    redisPort <-
        Envy.runParser (Envy.env "REDIS_PORT")
            >>= (except . bimap Text.unpack Redis.PortNumber . readEither)

    mongoPort <-
        Envy.runParser (Envy.env "MONGO_PORT")
            >>= (except . bimap Text.unpack MongoDB.PortNumber . readEither)

    Env
        <$> Envy.runParser (Envy.env "PORT")
        <*> liftIO (MongoDB.connect (MongoDB.Host "localhost" mongoPort))
        <*> liftIO (Redis.connect Redis.defaultConnectInfo{Redis.connectPort = redisPort})
