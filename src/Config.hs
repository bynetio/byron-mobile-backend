{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Config where

import           ClassyPrelude
import           Data.Aeson
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Handler.Warp (HostPreference)
import qualified Conferer
import           Conferer.FromConfig.Warp ()
import qualified Conferer.Source.Dhall    as Dhall
import qualified Conferer.Source.Env      as Env
import CommandLine
import qualified Data.Aeson.Encode.Pretty as AP


newtype SocketConfig = SocketConfig {
    socketConfigPath :: FilePath
} deriving (Generic, Show)

instance ToJSON SocketConfig

data NodeConfig = NodeConfig {
    nodeConfigMagic   :: Maybe String,
    nodeConfigNetwork :: String,
    nodeConfigSocket  :: SocketConfig
} deriving (Generic, Show)

instance ToJSON NodeConfig

data AppConfig = AppConfig {
    appConfigServer :: Warp.Settings,
    appConfigNode   :: NodeConfig
} deriving (Generic)

instance ToJSON AppConfig where
    toJSON cfg = object
      [
        "server" .= object
          [
            "server" .= object
              [
                  "host" .= host cfg
                , "port" .= port cfg
              ]
          ],
        "node" .= toJSON (appConfigNode cfg)
      ]
      where
          port = Warp.getPort . appConfigServer
          host :: AppConfig -> String
          host = parseHost . Warp.getHost . appConfigServer
            where
              parseHost :: HostPreference -> String
              parseHost p = case (words . show) p of
                _ : h : _ -> fromMaybe (show p) . readMay $ h
                _         -> show p

instance Conferer.FromConfig NodeConfig
instance Conferer.FromConfig AppConfig
instance Conferer.FromConfig SocketConfig

instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig {
      appConfigServer = Warp.setHost "0.0.0.0" $ Warp.setPort 8088 Conferer.configDef,
      appConfigNode = NodeConfig (Just "1097911063") "Testnet" (SocketConfig "/var/node-ipc/socket")
    }

loadAppConfig :: CommandLineConfig -> IO AppConfig
loadAppConfig cliCfg = do
    cfg <- mkConfig
    Conferer.fetch cfg
  where
    mkConfig :: IO Conferer.Config
    mkConfig = Conferer.mkConfig' []
     [   Env.fromConfig "app"
       , Dhall.fromFilePath (fromMaybe "app.config" $ clcConfigPath cliCfg)
     ]

encodePretty :: AppConfig -> Text
encodePretty = decodeUtf8 . toStrict . AP.encodePretty     