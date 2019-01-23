{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Bifunctor (bimap)
import Conduit (MonadResource, MonadThrow)
import Data.Conduit (ConduitT, ZipSource(..), (.|), runConduitRes)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (mapMaybe)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, PageWidth(Unbounded), LayoutOptions)
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color(..))
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as P
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

mapDiff :: (a -> b) -> Diff a -> Diff b
mapDiff f d = case d of
    First x -> First $ f x
    Second x -> Second $ f x
    Both x y -> Both (f x) (f y)

lineDiffSource
    :: (MonadResource m, MonadThrow m) => FilePath -> ZipSource m (Maybe Text)
lineDiffSource path = ZipSource $ do
    C.sourceFile path .| C.decodeUtf8 .| C.linesUnbounded .| C.map Just
    C.repeat Nothing

lineDiffInputs
    :: (MonadResource m, MonadThrow m)
    => FilePath -> FilePath -> ConduitT () (Text, Text) m ()
lineDiffInputs original new = do
    getZipSource zippedSource .| C.takeWhile bothEOF .| C.map unwrap
  where
    zippedSource
        :: (MonadResource m, MonadThrow m)
        => ZipSource m (Maybe Text, Maybe Text)
    zippedSource = (,) <$> lineDiffSource original <*> lineDiffSource new

    bothEOF :: (Maybe Text, Maybe Text) -> Bool
    bothEOF (Nothing, Nothing) = False
    bothEOF _ = True

    unwrap :: (Maybe Text, Maybe Text) -> (Text, Text)
    unwrap = bimap (fromMaybe "") (fromMaybe "")

lineDiff :: Monad m => ConduitT (Text, Text) [Diff Text] m ()
lineDiff = C.mapMaybe diffUnequal
  where
    diffUnequal :: (Text, Text) -> Maybe [Diff Text]
    diffUnequal (t1, t2)
        | t1 == t2 = Nothing
        | otherwise = Just . map (mapDiff T.pack) $ diffText t1 t2

    diffText :: Text -> Text -> [Diff [Char]]
    diffText = getGroupedDiff `on` T.unpack

printDiff :: MonadIO m => [Diff Text] -> m ()
printDiff diffs = liftIO $ do
    prettyPrint old
    prettyPrint new
  where
    old, new :: Doc AnsiStyle
    (old, new) = collapse mempty diffs

    layoutStyle :: LayoutOptions
    layoutStyle = P.defaultLayoutOptions{P.layoutPageWidth = Unbounded}

    prettyPrint :: Doc AnsiStyle -> IO ()
    prettyPrint = T.putStrLn . P.renderStrict . P.layoutPretty layoutStyle

    collapse
        :: (Doc AnsiStyle, Doc AnsiStyle)
        -> [Diff Text]
        -> (Doc AnsiStyle, Doc AnsiStyle)
    collapse result [] = result
    collapse prefix (d:ds) = collapse (prefix <> chunk) ds
      where
        chunk = case d of
            Both x y -> (P.pretty x, P.pretty y)
            First t -> (P.annotate (P.color Red) $ P.pretty t, mempty)
            Second t -> (mempty, P.annotate (P.color Green) $ P.pretty t)

main :: IO ()
main = do
    [original, new] <- getArgs
    runConduitRes $
        lineDiffInputs original new .| lineDiff .| C.mapM_ printDiff
