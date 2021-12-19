import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "-XHaskell2010", "Data/ByteString/Builder/HTTP/Chunked.hs"]


