{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson               (Value, encode, decodeStrict, object, (.=) )
import           Data.Aeson.Parser        (json)
import           Data.ByteString          (ByteString, concat)
import           Data.ByteString.Char8    (pack)
import           Data.Conduit             (($$), Sink)
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400, status404)
import           Network.Wai              (Application, Request, Response, responseLBS, pathInfo, responseBuilder, requestMethod, responseFile)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)
import           Blaze.ByteString.Builder (copyByteString)
import           Data.Text                (Text, unpack, pack, append)
import           Data.Serialize           (encode)
import           Data.Monoid              (mconcat)
import           Database.Persist.Class   (keyToValues)
import           Data.Int                 (Int64)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Text.Hamlet
import           Database.Persist.Sqlite
import           GHC.Generics
import qualified Data.Conduit.List as CL

-- custom modules
import           DataTypes                (Shoe, ShoeRaw, shoeFromRawData, saveImageData, photo, shoeDescription, shoeSize, shoeColor, shoePhotoPath, queryShoe)


main :: IO ()
main = run 3000 app

getDefault :: Network.Wai.Request -> IO Response
getDefault req = do
    return $ responseLBS
        status404
        [("Content-Type", "application/json")]
        $ "Not Found"

data Route = GetShoes | GetShoe Int64

renderUrl :: Route -> [(Text, Text)] -> Text
renderUrl GetShoes _ = "/shoes"
renderUrl (GetShoe shoeId) _ = Data.Text.append "/shoes/" (Data.Text.pack (show shoeId))

shoesTemplate :: HtmlUrl Route
shoesTemplate = [hamlet|
<html>
    <head>
        shoes
    <body>
        shoes
|]

shoeTemplate :: Shoe -> HtmlUrl Route
shoeTemplate shoe = [hamlet|
<html>
    <head>
        shoe
    <body>
        <p>Description:#{shoeDescription shoe}
        <p>Size:#{shoeSize shoe}
        <p>Color:#{shoeColor shoe}
        <p>
            <img src="/shoe_images/#{shoePhotoPath shoe}">
|]

getShoes :: Network.Wai.Request -> IO Response
getShoes req = do
    let html = shoesTemplate renderUrl
    return $ responseBuilder status200 [ ("Content-Type", "text/plain") ] $ renderHtmlBuilder html

getShoe :: Network.Wai.Request -> Int64 -> IO Response
getShoe req shoeId = do
    maybeShoe <- runSqlite "shoes.db" $ queryShoe shoeId
    case maybeShoe of
        Nothing -> return $ responseBuilder status404 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString [ "Not Found" ]
        Just shoe -> do
            let html = shoeTemplate shoe renderUrl
            return $ responseBuilder status200 [ ("Content-Type", "text/html") ] $ renderHtmlBuilder html

getShoeImage :: Network.Wai.Request -> String -> IO Response
getShoeImage req imageFileName =
    return $ responseFile status200 [ ("Content-Type", "image/jpeg") ] ("./images/" ++ imageFileName) Nothing

aggregateSink :: MonadIO m => Sink ByteString m ByteString
aggregateSink = CL.fold (\s1 s2 -> Data.ByteString.concat [s1,s2]) ""

postShoes :: Network.Wai.Request -> IO Response
postShoes req = do
    shoe :: Maybe ShoeRaw <- (sourceRequestBody req $$ aggregateSink) >>= (return . decodeStrict)
    case shoe of
      Just rawData  -> do
        imageFilePath <- saveImageData $ photo rawData
        shoeId <- runSqlite "shoes.db" $ do
            shoeId <- insert $ shoeFromRawData rawData imageFilePath
            return shoeId
        let Right (intValue :: Int64) = (fromPersistValue.head.keyToValues) shoeId
        return $ responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString [ "post shoes ", Data.ByteString.Char8.pack (show intValue), "\n" ]

      Nothing -> return $ responseBuilder status400 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString [ "Bad request" ]

app :: Application
app req sendResponse = handle (sendResponse . invalidJson) $ do
    response <- case (requestMethod req, pathInfo req) of
      ("GET",  ["shoes", shoeId]) -> getShoe req  (read ( Data.Text.unpack shoeId ))
      ("GET",  ["shoe_images", imageName]) -> getShoeImage req (Data.Text.unpack imageName)
      ("GET",  ["shoes"]) -> getShoes req
      ("POST", ["shoes"]) -> postShoes req
      _                   -> getDefault  req

    sendResponse response

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
    status400
    [("Content-Type", "application/json")]
    $ Data.Aeson.encode $ object
        [ ("message" .= show ex)
        ]

--{"description": "SADIE Faux Suede Heels with Bow", "color": "red", "size": 34, "photo": "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsKCwsNDhIQDQ4RDgsLEBYQERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wAARCAA2ACUDASIAAhEBAxEB/8QAGwABAQACAwEAAAAAAAAAAAAAAAkGBwEFCAr/xAAxEAABAwMDAgUCBAcAAAAAAAABAgMEBQYRAAchCBIJEyIxUTJhF0GCkSMzQlJxgaH/xAAaAQABBQEAAAAAAAAAAAAAAAAAAgMEBQcG/8QAKxEAAQMDAgMHBQAAAAAAAAAAAQACAwQFERIhIjFxBhMyQVFhgaHB0eHx/9oADAMBAAIRAxEAPwCqClJQMqISPknXPGpgeKRvTNuC7U7a0+oPRaVTGG35jbKyA9KWAtPdj3CElGPglWs18N/ruO4UWHtTuJPIuyKnyaRVJCuai2kfyXFH3eSBwT9YGD6h6mhICcLpKqxVFLRw1bjnvBqx6D+YPQ+xVCuPjTj40xpjTq5tONNMaaEKSviRbJ1qxt3pd5+WqTb1xL8xmTyfKfCfWyo/keO5Pyk8fSceHno1RolYj1ykyHYEyM+mTGlMEpU08ghQUlQ9iDj/AJq3XiFW8mv9LtwKUgKXCmQZCCRkpJkttEj9LqtS3bsuVZN6XFt7c7PlS4z6h5CvoWsDlSPstHaoEe4xqvk4HEBbXaqxl5tbYp/HHt8AAA/UZ/aq10T9TUfqe2Zh1qQW2rpppEGuREcdr4HDoH5IcHqH5A9w/p1v/wDfUH+nPqCqnRrvy7Ukoen27KIi1enoIzJjE5S4gHjzEZ7h+pOQFHVtLD3YtHcyzIV123X4VSoMsJDctDwSErUQA2sHBQvJAKFYIJxjUuN+oLL7pQOo5yGjhPL8LLP300wTpp1Ui6K+bHo249qVG27hhmfR56AiRHDq2ioBQUMLQQpJCkgggg8anB4ou1kml3dQL8hxVR1ONCDKlMjAUpHLKyf7u3KT9ko1T3H21je4W3dB3StSbblyQG6jSpiO1xpfuPggjkEHkEcjSHsDhhXNpuLrZVsn5tGxHqDz+x6gL5/Kg63e1WpzNecDDaXAl2ayj+IGyecge/8Aof4B9tbAvna/8MaYmo27PcqNjTXEOImJX6mncHsRISDjux3dqx6VDPbg9yR7wvjwmLSqSlu2zdtWpKlZwxMSiQ2PsOEq/cnWvZvhLXfPhsQHNzWDT2HFOtsrgLUkKPurt80DP31BMMgIwdlpMfaW2RytqYTpd5gt8uozg/K9PeH1uBeO4uxDU+7JaaoyxKMal1IvIcekR0oTkOkKKu9CypHrCVYSD6shRa7/AKP+lxXSxZVWoi7nkXM7UpglrUqMIzTJCAnCG+9fJxyrPOE8DHLU9uQBlZhc5Yp6yWWHGlxyMDA39lvzA0wNNNKVYnGuONNNCEOmmmhC/9k="}
