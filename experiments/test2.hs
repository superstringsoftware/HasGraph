{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import qualified Text.JSON as J
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as T
-- import MediaWiki.API


stupidRequest :: String -> Int -> IO String
stupidRequest s n = simpleHTTP (getRequest s) >>= fmap (take n) . getResponseBody

testWiki = "https://en.wikipedia.org/w/api.php?action=query&titles=Main%20Page&prop=revisions&rvprop=content&format=json"

rw s = simpleHttp s

simpleDecode :: T.ByteString -> Maybe A.Object
simpleDecode s = A.decode s

ex s = do
    st <- rw s
    let ct = T.unpack st
    print ct


-- do smth like
-- stupidRequest "http://meteorology.io" 500 >>= return . parseTags
