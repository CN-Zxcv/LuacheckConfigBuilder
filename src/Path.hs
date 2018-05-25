module Path (
    --   Iterate(..)
    -- , foldPath
    getRecursiveContents
) where

import System.Directory (listDirectory, doesDirectoryExist, getPermissions)
import Control.Monad (forM)
import System.FilePath ((</>), takeExtensions)

-- isDirectory :: FilePath -> Bool
-- isDirectory path = searchable getPermissions path

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- listDirectory topdir
    let propNames = filter (`notElem` [".svn", "server"]) names
    paths <- forM propNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else if withExtensions [".lua"] path 
                    then return [path]
                    else return []
    return (concat paths)
    where
        withExtensions :: [String] -> FilePath -> Bool
        withExtensions exts path =
            takeExtensions path `elem` exts

        


-- data Iterate acc
--     = Done {unwrap :: acc}
--     | Skip {unwrap :: acc}
--     | Continue {unwrap :: acc}
--     deriving (Show)

-- data PathInfo = PathInfo { 
--     infoPath :: FilePath,
--     infoPermisson :: Maybe Permissions,
--     infoSize :: Maybe FileSize,
--     infoModTime :: Maybe UTCTime
--     } deriving (Eq, Ord, Show)

-- type Iterate acc = acc -> Iterate acc

-- foldPath :: Iterate a -> a -> FilePath -> IO a
-- foldPath iter initAcc path = do
--     endAcc <- fold initAcc path
--     return (unwrap endAcc)
--     where
--         fold acc subpath = listDirectory subpath >>= walk acc subpath
--         walk acc path (name:names) = do
--             let path' = path </> name
--             info <- getInfo path'
--             case iter acc info of
--                 done@(Done _) -> return done
--                 Skip acc' -> walk acc' path names
--                 Continue acc'
--                     | isDirectory info -> do
--                         next <- fold acc' path'
--                         case next of
--                             done@(Done _) -> return Done
--                             acc'' -> walk (unwrap acc'') path names
--                     | otherwise -> walk acc' path names
--         walk acc _ _ = return (Continue acc)

-- isDirectory :: FilePath -> Bool
-- isDirectory path = getPermiss

-- allFileWithExtension :: String -> Iterate [FilePath]
-- allFileWithExtension ext path
--     | 

-- test :: IO ()

