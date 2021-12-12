{-# OPTIONS_GHC -fno-warn-orphans #-}

module App where

import qualified Adapter.HTTP.Main           as HTTP
import           ClassyPrelude
import           CommandLine
import           Config
import qualified Domain.Wallet               as D
import           Logger
import           Network.Wai.Handler.Warp    (getHost, getPort, runSettings)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Options.Applicative         (execParser)
import qualified Adapter.CardanoNode.Wallet as CN

startApp :: AppConfig -> IO ()
startApp cfg = do
    logger I $ pack ("Starting server on " <> hostPort <> " ...")
    runSettings (appConfigServer cfg) $
      simpleCors $ HTTP.app HTTP.WalletService {
         HTTP.getUtxos = nt . CN.getUtxos
       , HTTP.submitTx = nt . CN.submitTx
       , HTTP.getBalance = nt . D.getBalance
      }
  where
    port = getPort . appConfigServer
    host  = getHost . appConfigServer
    hostPort = foldr (<>) "" [show $ host cfg, ":", show $ port cfg]
    state :: CN.State CN.AppWallet
    state = CN.State {
          CN.nodeConfig = appConfigNode cfg
        , CN.logAction = richMessageAction
    }
    nt :: CN.AppWallet a -> IO a
    nt = flip CN.runApp state

instance D.WalletService CN.AppWallet where
  getUtxos = CN.getUtxos
  submitTx = CN.submitTx

instance D.AssetMetadataService CN.AppWallet where
  getMetadata _ _ = return Nothing

main :: IO ()
main = do
    cmdConfig <- execParser cmdParser
    appConfig <- loadAppConfig cmdConfig
    case clcCommand cmdConfig of
      StartApp   -> startApp appConfig
      ShowConfig -> logger I $ encodePretty appConfig

