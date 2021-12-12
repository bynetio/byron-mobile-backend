{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}

module Domain.Wallet where

import           ClassyPrelude
import qualified Servant as S
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))

newtype PolicyId = PolicyId { unPolicyId :: Text } deriving (Generic, Show, Eq, Ord)

newtype AssetName = AssetName { unAssetName :: Text } deriving (Generic, Show, Eq, Ord)

newtype TxId = TxId { unTxId :: Text } deriving (Generic, Show)

newtype AdaValue = AdaValue { unAdaValue :: Integer } deriving (Generic, Show, Eq)

newtype WalletAddress = WalletAddress { 
  unWalletAddress :: Text 
} deriving (Generic, Show, Eq, Ord)
  deriving newtype (S.FromHttpApiData)

instance A.ToJSON PolicyId where
  toJSON p = A.toJSON (unPolicyId p)

instance A.ToJSON AssetName where
  toJSON an = A.toJSON (unAssetName an)

instance A.ToJSON TxId where
  toJSON txId = A.toJSON (unTxId txId)

instance A.ToJSON AdaValue where
  toJSON a = A.toJSON (unAdaValue a)

instance Semigroup AdaValue where
  AdaValue a <> AdaValue b = AdaValue $ a + b

instance Monoid AdaValue where
  mempty = AdaValue 0

data AssetMetadata = AssetMetadata {
      assetMetadataName:: Text
    , assetMetadataDescription :: Text
    , assetMetadataTicker :: Text
    , assetMetadataDecimal :: Int
    , assetMetadataUrl :: Text
    , assetMetadataLogo :: Text
} deriving (Generic, Show)

instance A.ToJSON AssetMetadata where
  toJSON meta = A.object 
    [
        "name" .= assetMetadataName meta
      , "description" .= assetMetadataDescription meta
      , "ticker" .= assetMetadataTicker meta
      , "decimal" .= assetMetadataDecimal meta
      , "url" .= assetMetadataUrl meta
      , "logo" .= assetMetadataLogo meta
    ]

data Asset = Asset {
      assetPolicyId :: PolicyId
    , assetName     :: AssetName
    , assetQuantity :: Integer
    , assetMetadata :: Maybe AssetMetadata
} deriving (Generic, Show)

instance A.ToJSON Asset where
  toJSON asset = A.object 
    [
          "policy_id" .= assetPolicyId asset
        , "asset_name" .= assetName asset
        , "quantity" .= assetQuantity asset
        , "metadata" .= A.toJSON (assetMetadata asset)
    ]

data Utxo = Utxo {
      utxoId :: TxId
    , utxoIndex  :: Word
    , utxoAmount :: AdaValue
    , utxoAssets :: [Asset]
} deriving (Generic, Show)

instance A.ToJSON Utxo where
  toJSON utxo = A.object
    [
        "id" .= utxoId utxo
      , "index" .= utxoIndex utxo
      , "amount" .= A.object
         [
             "quantity" .= utxoAmount utxo
           , "unit" .= ("lovelace" :: Text)
         ]
      , "assets" .= utxoAssets utxo
    ]

data WalletBalance = WalletBalance {
      walletBalanceAmount :: AdaValue
    , walletBalanceAssets :: [Asset]  
} deriving (Generic, Show)

instance A.ToJSON WalletBalance where
  toJSON wb = A.object
    [
        "assets" .= A.toJSON (walletBalanceAssets wb)
      , "amount" .= A.object
         [
             "quantity" .= walletBalanceAmount wb
           , "unit" .= ("lovelace" :: Text)
         ]
    ]

data TxPayload = TxPayload {
    txPayloadType :: Text,
    txPayloadDescription :: Text,
    txPayloadCborHex :: Text
} deriving (Generic, Show)

instance A.FromJSON TxPayload where
  parseJSON = A.withObject "TxPayload" $ \payload -> TxPayload
    <$> payload .: "type"
    <*> payload .: "description"
    <*> payload .: "cborHex"

data WalletServiceError 
  = InvalidWalletAddress Text
  | UknownNetworkIdError Text
  | NetworkMagicMismatchError Text
  | SubmitTxError Text
  | QueryUtxoError Text
  | AddressDecodingError Text
  | DeserialiseFromCBORError Text
  | SomeError Text deriving (Show, Typeable)

instance Exception WalletServiceError

class (Monad m) => WalletService m where
  getUtxos :: WalletAddress -> m [Utxo]
  submitTx :: TxPayload -> m TxId

class (Monad m) => AssetMetadataService m where
  getMetadata :: PolicyId -> AssetName -> m (Maybe AssetMetadata)

getBalance :: (WalletService m, AssetMetadataService m) => WalletAddress -> m WalletBalance
getBalance address = do
  utxos <- getUtxos address
  let ada = foldMap utxoAmount utxos
  let assets = (mapToList . toAssetMap) utxos
  assetsWithMeta <- forM assets $ \((policyId, name), q) -> do
    meta <- getMetadata policyId name
    return $ Asset policyId name q meta
  return $ WalletBalance ada assetsWithMeta
  where
    insertAsset :: Asset -> Map (PolicyId, AssetName) Integer -> Map (PolicyId, AssetName) Integer
    insertAsset a = insertWith (+) (assetPolicyId a, assetName a) (assetQuantity a)
    toAssetMap :: [Utxo] -> Map (PolicyId, AssetName) Integer
    toAssetMap xs = let assets = join $ map utxoAssets xs
                    in  foldr insertAsset mempty assets
                    