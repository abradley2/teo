module Env (getEnv, Env (..)) where

import Configuration.Dotenv qualified as Dotenv
import Control.Monad.Trans.Except
import Data.Text qualified as Text
import Database.MongoDB qualified as MongoDB
import Database.Redis qualified as Redis
import Relude
import System.Envy qualified as Envy

data Env = Env
    { port :: Int
    , mongoPipe :: MongoDB.Pipe
    , redisConn :: Redis.Connection
    , requestId :: Maybe Text
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
        <*> pure Nothing
