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

    -- replicaSet <-
    --     liftIO $
    --         MongoDB.openReplicaSetTLS
    --             ( Text.pack "atlas-1vsvgu-shard-0"
    --             ,
    --                 [ MongoDB.host "ac-tky6m5c-shard-00-00.ndsopa6.mongodb.net"
    --                 , MongoDB.host "ac-tky6m5c-shard-00-01.ndsopa6.mongodb.net"
    --                 , MongoDB.host "ac-tky6m5c-shard-00-02.ndsopa6.mongodb.net"
    --                 ]
    --             )

    mongoPipe <- liftIO $ MongoDB.connect (MongoDB.host "127.0.0.1")

    -- replicaSet <-
    --     liftIO $
    --         MongoDB.openReplicaSetSRV' "cluster0.ndsopa6.mongodb.net"

    -- mongoPipe <- liftIO $ MongoDB.primary replicaSet

    -- mongoUser <- Envy.runParser (Envy.env "MONGO_USER")

    -- mongoPassword <- Envy.runParser (Envy.env "MONGO_PASSWORD")

    -- _ <-
    --     if connectedResult
    --         then pure ()
    --         else ExceptT . pure $ Left "Failed to authenticate with mongo: invalid credentials"

    Env
        <$> Envy.runParser (Envy.env "PORT")
        <*> pure mongoPipe
        <*> liftIO (Redis.connect Redis.defaultConnectInfo{Redis.connectPort = redisPort})
        <*> pure Nothing
