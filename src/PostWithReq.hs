{-# LANGUAGE OverloadedStrings #-}

module PostWithReq where

import Control.Monad
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text (unpack)
import Network.HTTP.Req


getExample :: MonadHttp m => m BsResponse
getExample = req GET url NoReqBody bsResponse mempty
    where
    url = http "example.com"
runGetExample :: IO ()
runGetExample = do
    resByteString <- runReq defaultHttpConfig getExample
    -- レスポンスはByteStringで返ってくる。ページのエンコーディングを予め調べておいてデコードする
    -- サーバー側のエンコーディングは変わる可能性があるので、常に耐性を持たせる必要がある
    putStrLn $ toString $ responseBody resByteString
    where
        toString :: ByteString -> String
        toString = unpack . decodeUtf8
        -- pureなコードでは下を使う
        toString' :: ByteString -> Either UnicodeException String
        toString' b = unpack <$> decodeUtf8' b

postTaisya :: MonadHttp m => m BsResponse
postTaisya = req POST url (ReqBodyUrlEnc params) bsResponse mempty
    where
    url = https "works.hue.worksap.com" /: "self-workflow" /: "cws" /: "srwtimerec"
    params :: FormUrlEncodedParam
    params = "dakoku" =: ("taisya" :: String)
        <> "timezone" =: (540 :: Int)
runPostTaisya :: IO ()
runPostTaisya = do
    resByteString <- runReq defaultHttpConfig postTaisya
    print $ responseBody resByteString


someFunc :: IO ()
someFunc = runGetExample
