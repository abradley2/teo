module Action.Event (listEvents) where

import Action (Action, AppError (..), LoggingContext)
import Action qualified
import Action.Auth.Documents.User (User (..))
import Action.Event.Documents.Event (Event (..))
import Action.Event.Documents.Event qualified as Documents.Event
import Data.Text.Lazy qualified as LazyText
import Database.MongoDB (Cursor)
import Database.MongoDB.Query qualified as MQuery
import Network.HTTP.Types (status500)
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

listEvents :: User -> ActionT LazyText.Text Action ()
listEvents user =
    do
        ctx <- Action.getLoggingContext
        let logger = Action.createLogger' "listEvents" ctx

        Action.logDebug logger $ "fetching events for " <> show user.userId

        cursor <-
            Action.withMongoAction
                ( MQuery.find
                    ( MQuery.select
                        [ Documents.Event.userIdField user.userId
                        ]
                        Documents.Event.collection
                    )
                )
                >>= Action.withError
                    logger
                    ( \err ->
                        AppError
                            { log = Just $ "Error fetching events: " <> show err
                            , response = "Error fetching events"
                            , status = status500
                            }
                    )

        Action.logDebug logger "Retrieved events from mongo, converting to JSON"

        resJson <- parseEvents ctx cursor

        Action.logDebug logger "Sending events to client"

        ScottyT.json resJson

parseEvents :: LoggingContext -> Cursor -> ActionT LazyText.Text Action [Event]
parseEvents ctx cursor = do
    let logger = Action.createLogger' "parseEvents" ctx
    eventDocResult <- Action.withMongoAction $ MQuery.next cursor
    case eventDocResult of
        Left err ->
            Action.throwError logger $
                AppError
                    { log = Just $ "Error calling next cursor " <> show err
                    , response = "Internal server error"
                    , status = status500
                    }
        Right Nothing -> do
            void . Action.withMongoAction $ MQuery.closeCursor cursor
            pure []
        Right (Just eventDoc) ->
            case Documents.Event.decodeEvent eventDoc of
                Left err -> do
                    Action.logWarn logger $ "Error parsing event document " <> show err
                    parseEvents ctx cursor
                Right event -> (event :) <$> parseEvents ctx cursor
