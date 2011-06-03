{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Snap.Logging.MongoDB 
  (logAccess',
  logAccess)
where

import Snap.Extension (SnapExtend)
import Snap.Extension.DB.MongoDB (HasMongoDBState, MonadMongoDB, select, insert, repsert, MongoDBState(..), getMongoDBState, access, WriteMode (Unsafe), MasterOrSlaveOk (Master), use, Database, Action, Failure)
import Data.Bson ((=:), val)
import Snap.Types (MonadSnap, rqURI, rqMethod, getRequest)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO, lift, MonadIO)
import Control.Monad.Reader (MonadReader, asks, ReaderT)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (fromMaybe)
import Snap.Extension.Session.CookieSession (getFromSession, MonadSession)

withDBUnsafe :: (HasMongoDBState s, MonadIO (SnapExtend s), MonadReader s (SnapExtend s)) => 
  ReaderT Database (Action IO) a -> SnapExtend s (Either Failure a)
withDBUnsafe run = do
    (MongoDBState pool db) <- asks getMongoDBState
    liftIO . access Unsafe Master pool $ use db run
    
{-|
 The logAccess function provides a default session variable to record the unique identifier for the user, 'accountName'
-}
logAccess action = logAccess' (getFromSession "accountName") action

{-|
 The logAccess' function takes an action that specifies how to get a unique identifier for the user and 
 the handler to run and it logs the page generation time and access count to the tables "time" and "access" respectively
 -}
logAccess' userId action = do
  pageName  <- liftM rqURI getRequest
  method    <- liftM (pack . show . rqMethod) getRequest
  start     <- liftIO $ getCurrentTime
  result    <- action
  u         <- userId
  let user = fromMaybe "Anonymous" u
  end       <- liftIO $ getCurrentTime
  let diff = fromRational $ toRational $ diffUTCTime end start
  
  withDBUnsafe $ insert "time"   ["page" =: pageName, "method" =: method, "user" =: user, "time" =: (diff :: Double), "date" =: start]
  withDBUnsafe $ repsert (select ["page" =: pageName, "method" =: method, "user" =: user] "access") ["$inc" =: ["hits" =: (1 :: Int)]]
  return result