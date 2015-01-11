{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module DataTypes (Shoe,
                  ShoeRaw,
                  migrateAll,
                  shoeFromRawData,
                  saveImageData,
                  photoR,
                  shoePhotoPath,
                  shoeColor,
                  shoeSize,
                  shoeDescription,
                  queryShoe) where

import           Control.Monad.IO.Class   (MonadIO)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (encodeUtf8)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Base64   (decode)
import           Data.Conduit             (($$), Source, yield)
import           Data.ByteString.Base16   (encode)
import           Database.Persist.Sql     (SqlPersistT)
import           Data.Int                 (Int64)
import           Control.Monad.Trans.Resource
import           GHC.Generics
import           Database.Persist
import           Database.Persist.TH
import qualified Data.Conduit.Binary as CB
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as B

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Shoe
    description Text
    color       Text
    size        Int
    photoPath   Text
    deriving Show
|]

data ShoeRaw =
  ShoeRaw { descriptionR :: !Text
          , colorR       :: !Text
          , sizeR        ::  Int
          , photoR       :: !Text
  } deriving (Show, Generic)

shoeFromRawData :: ShoeRaw -> FilePath -> Shoe
shoeFromRawData rawShoeData shoeImageFile =
    let (ShoeRaw description color size _) = rawShoeData in
    (Shoe description color size (Data.Text.pack shoeImageFile))

instance FromJSON ShoeRaw
instance ToJSON ShoeRaw

sourceByteString :: MonadIO m => ByteString -> Source m ByteString
sourceByteString byteString = do
    yield byteString

fileNameFromBytes :: ByteString -> FilePath
fileNameFromBytes bytes =
    let hashString = B.unpack $ Data.ByteString.Base16.encode $ SHA256.hash bytes
    in "./images/" ++ hashString ++ ".jpg"

saveImageData :: Text -> IO String
saveImageData imageData = do
    let byteData = Data.ByteString.Base64.decode $ encodeUtf8 imageData
    case byteData of
        Right bytes -> do
            let filePath = fileNameFromBytes bytes
            runResourceT $ (sourceByteString bytes) $$ CB.sinkFile filePath
            return filePath
        _ -> do
            print "Invalid data"
            return ""

queryShoe :: MonadIO m => Int64 -> SqlPersistT m (Maybe Shoe)
queryShoe shoeId = do
    let Right key = keyFromValues [PersistInt64 shoeId]
    get key

