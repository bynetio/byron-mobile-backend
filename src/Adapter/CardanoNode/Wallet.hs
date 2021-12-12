{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Adapter.CardanoNode.Wallet (submitTx, runApp, getUtxos, State(..), AppWallet) where
import qualified Cardano.Api                                       as CA
import           ClassyPrelude
import           Colog
import           Config
import           Control.Exception                                 (throw)
import qualified Domain.Wallet                                     as D
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import qualified Prelude                                           as P
import           Data.Proxy                                        (Proxy (..))
import qualified Data.Text.Conversions as TC

data State m = State {
  nodeConfig :: NodeConfig,
  logAction  :: LogAction m Message
}

instance HasLog (State m) Message m where
    getLogAction :: State m -> LogAction m Message
    getLogAction = logAction

    setLogAction :: LogAction m Message -> State m -> State m
    setLogAction newLogAction env = env { logAction = newLogAction }

newtype AppWallet a = AppWallet {
  unApp :: ReaderT (State AppWallet) IO a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader (State AppWallet))

runApp :: AppWallet a -> State AppWallet -> IO a
runApp app = runReaderT (unApp app)

getUtxos :: D.WalletAddress -> AppWallet [D.Utxo]
getUtxos address = do
  address' <- liftIO $ parseAddress $ D.unWalletAddress address
  utxos <- queryUtxo address'
  return $ toDomainUtxo utxos

toNodeTx :: D.TxPayload -> Either D.WalletServiceError (CA.Tx CA.AlonzoEra)
toNodeTx (D.TxPayload _ _ payload) = txBody >>= deserialiseTx
  where
    unHexBs :: ByteString -> Maybe ByteString
    unHexBs v =  TC.decodeText (TC.UTF8 v) >>= TC.convertText <&> TC.unBase16

    txBody = maybe (Left $ D.DeserialiseFromCBORError $ "Payload: " <> payload) Right (unHexBs $ encodeUtf8 payload)

    deserialiseTx :: ByteString -> Either D.WalletServiceError (CA.Tx CA.AlonzoEra)
    deserialiseTx bs =
      case CA.deserialiseFromCBOR (CA.proxyToAsType (Proxy :: Proxy (CA.Tx CA.AlonzoEra))) bs of
        Left err -> Left $ D.DeserialiseFromCBORError $ (pack . P.show) err
        Right tx -> Right tx


submitTx :: D.TxPayload -> AppWallet D.TxId
submitTx payload = do
  tx <- liftIO $ liftEitherIO id (toNodeTx payload)
  let txId = (CA.getTxId . CA.getTxBody) tx
  submitNodeTx tx
  return $ D.TxId (showR txId)

nodeConn :: AppWallet (CA.LocalNodeConnectInfo CA.CardanoMode)
nodeConn = do
    cfg <- asks nodeConfig
    return $ connInfo (net cfg) (socketConfigPath $ nodeConfigSocket cfg)
  where
    net :: NodeConfig -> CA.NetworkId
    net (NodeConfig (Just netMagic) "Testnet"  _) = CA.Testnet $ magicFrom netMagic
    net cfg = throw $ D.UknownNetworkIdError $ (pack . show) cfg
    magicFrom :: String -> CA.NetworkMagic
    magicFrom magic = CA.NetworkMagic $ fromMaybe (throw $ D.NetworkMagicMismatchError (pack magic)) $ readMay magic
    connInfo :: CA.NetworkId -> FilePath -> CA.LocalNodeConnectInfo CA.CardanoMode
    connInfo = CA.LocalNodeConnectInfo (CA.CardanoModeParams (CA.EpochSlots 21600))

submitNodeTx :: CA.Tx CA.AlonzoEra -> AppWallet ()
submitNodeTx tx = do
  conn <- nodeConn
  res <- liftIO $ CA.submitTxToNodeLocal conn $ CA.TxInMode tx CA.AlonzoEraInCardanoMode
  case res of
    SubmitSuccess     -> return ()
    SubmitFail reason -> throw $ D.SubmitTxError $ (pack . show) reason

queryUtxo :: CA.AddressAny -> AppWallet (CA.UTxO CA.AlonzoEra)
queryUtxo addr = do
    conn <- nodeConn
    tip <- liftIO $ CA.getLocalChainTip conn
    utxosOrErr <- liftIO $ CA.queryNodeLocalState conn (Just $ CA.chainTipToChainPoint tip) query
    case utxosOrErr of
        Right (Right utxos) -> return utxos
        err                 -> throw $ D.QueryUtxoError $ (pack . show) err
  where
    query = CA.QueryInEra CA.AlonzoEraInCardanoMode $ CA.QueryInShelleyBasedEra CA.ShelleyBasedEraAlonzo (CA.QueryUTxO queryByAddress)
    queryByAddress :: CA.QueryUTxOFilter
    queryByAddress = CA.QueryUTxOByAddress $ singleton addr

parseAddress :: Text -> IO CA.AddressAny
parseAddress addr = liftMaybeIO maybeAddress (D.AddressDecodingError addr)
  where
    maybeAddress :: Maybe CA.AddressAny
    maybeAddress = CA.toAddressAny <$> CA.deserialiseAddress CA.AsShelleyAddress addr

liftMaybeIO :: Exception e =>  Maybe a -> e -> IO a
liftMaybeIO (Just a) _ = return a
liftMaybeIO _ e        = throw e

liftEitherIO :: (Exception e, Show err) => (err -> e) -> Either err b -> IO b
liftEitherIO f (Left err)    = throw $ f err
liftEitherIO _ (Right value) = return value

showR :: Show a => a -> Text
showR = pack . P.read . show

toDomainUtxo :: CA.UTxO CA.AlonzoEra -> [D.Utxo]
toDomainUtxo (CA.UTxO m) =
  let utxos = mapToList m
  in flip map utxos $ \(CA.TxIn txId txIx, txOut) -> D.Utxo (toTxId txId) (toTxIx txIx) (toAdaValue txOut) (toAssets txOut)
  where

    toTxId :: CA.TxId -> D.TxId
    toTxId txId = D.TxId $ showR txId

    toTxIx :: CA.TxIx -> Word
    toTxIx (CA.TxIx ix) = ix

    txOutValue :: CA.TxOut CA.CtxUTxO CA.AlonzoEra -> CA.Value
    txOutValue (CA.TxOut _ value _) = CA.txOutValueToValue value

    isAda :: CA.AssetId -> Bool
    isAda CA.AdaAssetId = True
    isAda _             = False

    quantityToInt (CA.Quantity num) = num

    txOutToAssets = CA.valueToList . txOutValue

    toAdaValue :: CA.TxOut CA.CtxUTxO CA.AlonzoEra -> D.AdaValue
    toAdaValue txOut = let adaAssets = snd <$> filter (isAda . fst) (txOutToAssets txOut)
                           adaQuantity = foldMap id adaAssets
                       in D.AdaValue $ quantityToInt adaQuantity

    toAssets :: CA.TxOut CA.CtxUTxO CA.AlonzoEra -> [D.Asset]
    toAssets txOut = let assets = txOutToAssets txOut
                     in catMaybes $ f <$> assets
      where
        f :: (CA.AssetId, CA.Quantity) -> Maybe D.Asset
        f (CA.AssetId pn (CA.AssetName an), CA.Quantity q) = Just $ D.Asset (D.PolicyId $ showR pn) (D.AssetName $ showR an) q Nothing
        f (CA.AdaAssetId, _) = Nothing
