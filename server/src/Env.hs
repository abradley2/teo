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

    mongoAtlasCluster <- Envy.runParser (Envy.env "MONGO_ATLAS_CLUSTER")

    mongoAtlasUser <- Envy.runParser (Envy.env "MONGO_ATLAS_USER")

    mongoAtlasPassword <- Envy.runParser (Envy.env "MONGO_ATLAS_PASSWORD")

    replicaSet <-
        liftIO $
            MongoDB.openReplicaSetSRV' mongoAtlasCluster

    mongoPipe <- liftIO $ MongoDB.primary replicaSet

    connectedResult <-
        liftIO $
            MongoDB.access mongoPipe MongoDB.master "admin" $
                MongoDB.auth
                    mongoAtlasUser
                    mongoAtlasPassword

    _ <-
        if connectedResult
            then pure ()
            else ExceptT . pure $ Left "Failed to authenticate with mongo: invalid credentials"

    Env
        <$> Envy.runParser (Envy.env "PORT")
        <*> pure mongoPipe
        <*> liftIO (Redis.connect Redis.defaultConnectInfo{Redis.connectPort = redisPort})
        <*> pure Nothing
