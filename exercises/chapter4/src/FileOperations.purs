module FileOperations where

import Prelude

import Control.MonadZero(guard)
import Data.Path(Path, ls, isDirectory, size, root)
import Data.Array (head, tail, filter, concatMap, foldl, (:))
import Data.Maybe (Maybe, fromMaybe)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

largestFile :: Path -> Maybe Path
largestFile path = foldl largest first rest
  where
    largest:: Maybe Path -> Path -> Maybe Path
    largest mb a = map (\b ->  if (size a) > (size b) then a else b) mb
    files = onlyFiles path
    first = head files
    rest = fromMaybe [] (tail files)

smallestFile :: Path -> Maybe Path
smallestFile path = foldl smallest first rest
  where
    smallest:: Maybe Path -> Path -> Maybe Path
    smallest mb a = map (\b ->  if (size a) < (size b) then a else b) mb
    files = onlyFiles path
    first = head files
    rest = fromMaybe [] (tail files)

whereIs :: String -> Maybe Path
whereIs path = whereIs' root path
  where
    whereIs':: Path -> String -> Maybe Path
    whereIs' folder path = head $ do
      child <- ls folder
      if isDirectory child
        then whereIs' child path
        else
          guard $ show child == path
          pure folder
