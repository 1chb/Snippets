{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Vector qualified as V
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdin)

formatJson :: Int -> Int -> Value -> BL.ByteString
formatJson maxLineLength indentLevel value =
  let encoded = encode value
      split = BL.length encoded > fromIntegral (maxLineLength - indentLevel)
   in case value of
        Object obj | split -> formatObject maxLineLength (indentLevel + 2) obj
        Array arr | split -> formatArray maxLineLength (indentLevel + 2) arr
        _ -> encoded

formatObject :: Int -> Int -> KM.KeyMap Value -> BL.ByteString
formatObject maxLineLength indentLevel obj =
  let indent = BL.replicate (fromIntegral indentLevel) ' '
      formattedContents :: [BL.ByteString] =
        KM.foldrWithKey (\k v acc -> (indent <> BL.pack (show k) <> ": " <> formatJson maxLineLength indentLevel v) : acc) [] obj
   in "{\n" <> BL.intercalate ",\n" formattedContents <> "\n" <> BL.drop 2 indent <> "}"

formatArray :: Int -> Int -> V.Vector Value -> BL.ByteString
formatArray maxLineLength indentLevel arr =
  let indent = BL.replicate (fromIntegral indentLevel) ' '
      formattedContents :: [BL.ByteString] =
        V.foldr (\v acc -> (indent <> formatJson maxLineLength indentLevel v) : acc) [] arr
   in "[\n" <> BL.intercalate ",\n" formattedContents <> "\n" <> BL.drop 2 indent <> "]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [maxLineLengthStr] ->
      perform maxLineLengthStr (B.hGetContents stdin) BL.putStr
    maxLineLengthStr : filePaths ->
      forM_ filePaths $ \filePath ->
        perform maxLineLengthStr (B.readFile filePath) (B.writeFile filePath)
    _ -> hPutStrLn stderr "Usage: PrettyJSON <max-line-length> [<file-path>...]"
  where
    perform maxLineLengthStr getInput putOutput = do
      let maxLineLength = read maxLineLengthStr :: Int
      content <- getInput
      case decode content :: Maybe Value of
        Just jsonValue -> putOutput $ formatJson maxLineLength 0 jsonValue <> "\n"
        Nothing -> hPutStrLn stderr "Failed to parse JSON."
