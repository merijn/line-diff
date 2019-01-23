{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Algorithm.Diff (Diff(..), getGroupedDiff)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Conduit (MonadResource, MonadThrow)
import Data.Conduit (ConduitT, Void, ZipSource(..), (.|), runConduitRes)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (mapMaybe)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
    (Doc, Pretty, LayoutOptions, PageWidth(Unbounded))
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color(..))
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as P
import System.Environment (getArgs)

mapDiff :: (a -> b) -> Diff a -> Diff b
mapDiff f d = case d of
    First x -> First $ f x
    Second x -> Second $ f x
    Both x y -> Both (f x) (f y)

diffSource
    :: (MonadResource m)
    => ConduitT ByteString a m () -> FilePath -> ZipSource m (Maybe a)
diffSource convert path = ZipSource $ do
    C.sourceFile path .| convert .| C.map Just
    C.repeat Nothing

diffInputs
    :: (MonadResource m, Monoid a)
    => ConduitT ByteString a m ()
    -> FilePath
    -> FilePath
    -> ConduitT () (a, a) m ()
diffInputs convert original new = do
    getZipSource zippedSource .| C.takeWhile bothEOF .| C.map unwrap
  where
    zippedSource = (,) <$> diffSource convert original
                       <*> diffSource convert new

    bothEOF :: (Maybe a, Maybe a) -> Bool
    bothEOF (Nothing, Nothing) = False
    bothEOF _ = True

    unwrap :: Monoid m => (Maybe m, Maybe m) -> (m, m)
    unwrap = bimap (fromMaybe mempty) (fromMaybe mempty)

diffLine
    :: forall a b m r
     . (Eq a, Eq b, Monad m, Pretty r)
    => (a -> [b]) -> ([b] -> r) -> ConduitT (a, a) [Diff r] m ()
diffLine unpack pack = C.mapMaybe diffUnequal
  where
    diffUnequal :: (a, a) -> Maybe [Diff r]
    diffUnequal (t1, t2)
        | t1 == t2 = Nothing
        | otherwise = Just . map (mapDiff pack) $ diff t1 t2

    diff :: a -> a -> [Diff [b]]
    diff = getGroupedDiff `on` unpack

diffFiles
    :: (Eq a, Eq b, MonadResource m, Monoid a, Pretty r)
    => (a -> [b])
    -> ([b] -> r)
    -> ConduitT ByteString a m ()
    -> FilePath
    -> FilePath
    -> ConduitT () [Diff r] m ()
diffFiles unpack pack convert original new =
    diffInputs convert original new .| diffLine unpack pack

diffAscii
    :: MonadResource m => FilePath -> FilePath -> ConduitT () [Diff Text] m ()
diffAscii = diffFiles BS.unpack (T.decodeUtf8 . BS.pack) C.linesUnboundedAscii

diffText
    :: (MonadResource m, MonadThrow m)
    => FilePath -> FilePath -> ConduitT () [Diff Text] m ()
diffText = diffFiles T.unpack T.pack $ C.decodeUtf8 .| C.linesUnbounded

printDiff :: (MonadIO m, Pretty a) => ConduitT [Diff a] Void m ()
printDiff = C.mapM_ $ liftIO . prettyPrint . collapse
  where
    layoutStyle :: LayoutOptions
    layoutStyle = P.defaultLayoutOptions{P.layoutPageWidth = Unbounded}

    prettyPrint :: Doc AnsiStyle -> IO ()
    prettyPrint = T.putStrLn . P.renderStrict . P.layoutPretty layoutStyle

    collapse :: Pretty a => [Diff a] -> Doc AnsiStyle
    collapse [] = mempty
    collapse (d:ds) = chunk <> collapse ds
      where
        chunk = case d of
            Both t _ -> P.pretty t
            First t -> P.annotate (P.color Red) $ P.pretty t
            Second t -> P.annotate (P.color Green) $ P.pretty t

reportMismatch :: Monad m => ConduitT [Diff Text] Void m Bool
reportMismatch = C.all (const False)

main :: IO ()
main = do
    [original, new] <- getArgs
    runConduitRes $ diffAscii original new .| printDiff
