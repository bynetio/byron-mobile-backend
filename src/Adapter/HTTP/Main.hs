{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}

module Adapter.HTTP.Main where

import           Servant hiding (Handler)
import qualified          Servant as S
import           ClassyPrelude
import qualified Domain.Wallet as D
import           Data.Aeson           (ToJSON (toJSON), encode)
import           Control.Monad.Except (ExceptT(..))


type API = "wallet" :> Capture "address" D.WalletAddress :> "utxos" :> Get '[JSON] [D.Utxo]
       :<|> "wallet" :> Capture "address" D.WalletAddress :> "balance" :> Get '[JSON] D.WalletBalance
       :<|> "transaction" :> "submit" :> ReqBody '[JSON] D.TxPayload :> Post '[JSON] D.TxId

proxyAPI :: Proxy API
proxyAPI = Proxy

server :: WalletService -> Server API
server service = handleGetUtxos :<|> handleGetBalance :<|> handleSubmitTx
  where
    handleGetUtxos :: D.WalletAddress -> S.Handler [D.Utxo]
    handleGetUtxos address = liftHandler $ getUtxos service address

    handleGetBalance :: D.WalletAddress -> S.Handler D.WalletBalance
    handleGetBalance address = liftHandler $ getBalance service address

    handleSubmitTx :: D.TxPayload -> S.Handler D.TxId
    handleSubmitTx payload = liftHandler $ submitTx service payload

    adaptErrors ::  Either D.WalletServiceError a -> Either ServerError a
    adaptErrors (Left _) = Left $ internalServerError "Internal Server Error"
    adaptErrors (Right value) = Right value

    internalServerError = httpError err500

    httpError :: ServerError -> Text -> ServerError
    httpError err msg =
        err
          { errBody = encode $ toJSON $ ErrorMessage msg
          , errHeaders = errorHeaders
          }
        where
          errorHeaders = [("Content-Type", "application/json")]

    liftHandler :: IO a -> S.Handler a
    liftHandler = S.Handler . ExceptT . fmap adaptErrors . (logError <=< try)
        where
          logError :: Exception e =>  Either e a -> IO (Either e a)
          logError err@(Left e) = do
            return err
          logError a            = return a


newtype ErrorMessage = ErrorMessage { 
  errorMessage :: Text
} deriving (Generic, ToJSON)


data WalletService = WalletService {
  getUtxos :: D.WalletAddress -> IO [D.Utxo],
  submitTx :: D.TxPayload -> IO D.TxId,
  getBalance :: D.WalletAddress -> IO D.WalletBalance
}

app :: WalletService -> Application
app = serve proxyAPI . server