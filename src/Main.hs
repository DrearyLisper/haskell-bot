{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

import Control.Monad (when, forM_, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

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

  Right chans <- restCall $ R.GetGuildChannels testserverid
  forM_ (take 1 (filter (\c -> channelName c == "bots") (filter isTextChannel chans)))
        (\channel -> restCall $ R.CreateMessage (channelId channel)
                        "Hello! I will reply to pings with pongs")


-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10 ^ (6 :: Int))

        -- A very simple message.
        void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")

      _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent