{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when, forM_, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Data.Text (Text, dropWhile, pack, unpack, stripPrefix)

import Discord
import Discord.Types
    ( Event(MessageCreate),
      GatewaySendable(UpdateStatus),
      UpdateStatusOpts(UpdateStatusOpts, updateStatusOptsSince,
                       updateStatusOptsGame, updateStatusOptsNewStatus,
                       updateStatusOptsAFK),
      Channel(ChannelText),
      Message(messageId, messageChannelId, messageAuthor,
              messageContent),
      Activity(Activity, activityName, activityType, activityUrl),
      ActivityType(ActivityTypeGame),
      UpdateStatusType(UpdateStatusOnline),
      Snowflake,
      User(userIsBot) )
import qualified Discord.Requests as R

import Lambdabot.Main
import Lambdabot.Plugin.Haskell

import System.IO.Silently (capture)

modulesInfo :: Modules
modulesInfo = $(modules $  corePlugins ++ ["type", "hoogle"])

online :: String -> IO String
online strs = do
    let request = void $ lambdabotMain modulesInfo [onStartupCmds ==> [strs]]
    (response, _)  <- capture request
    return response

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = if serverId == -1
       then TIO.putStrLn "ERROR: modify the source and set testserverid to your serverid"
       else pingPong


-- check the url in a discord server
--                                <server id>           <channel id>
-- https://discord.com/channels/2385235298674262408/4286572469284672046
serverId :: Snowflake
serverId = 939755692787638276

-- | Replies "pong" to every message that starts with "ping"
pingPong :: IO ()
pingPong = do
  tok <- TIO.readFile "./token"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = liftIO $ putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          }

  -- only reached on an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd
  TIO.putStrLn err

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  let activity = Activity { activityName = "ping-pong"
                          , activityType = ActivityTypeGame
                          , activityUrl = Nothing
                          }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isCommand m) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        -- A very simple message.
        response <- liftIO $ handleCommand m
        void $ restCall (R.CreateMessage (messageChannelId m) (T.pack response))

      _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCommand :: Message -> Bool
isCommand = ("!" `T.isPrefixOf`) . T.toLower . messageContent

handleCommand :: Message -> IO String
handleCommand = online . (\s -> '@' : tail s) . T.unpack  . messageContent