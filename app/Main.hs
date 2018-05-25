
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (listDirectory, doesDirectoryExist)
import Control.Monad (forM)
import System.FilePath ((</>))
import System.IO (hClose, openFile, IOMode(ReadMode, WriteMode))
import Control.Exception (bracket)
import System.Environment (getArgs)
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.DeepSeq
import Data.Foldable (foldlM)

import Path
import qualified LuaParser as P
import Language.Lua as P
import Language.Lua.PrettyPrinter as P
import System.FilePath.Posix (replaceBaseName)

main :: IO ()
main = do
    args <- getArgs
    let path = args !! 0
        configFile = args !! 1
        moduleName = "stds." ++ args !! 2
        moduleVar = P.assignName . T.pack $ moduleName
        outFile = replaceBaseName configFile ".luacheckrc"
    bracket (openFile configFile ReadMode) hClose $ \h -> do
        content <- T.hGetContents h
        let st = P.parseContent content

        paths <- getRecursiveContents $ path
        res <- foldlM buildGlobals [] paths
        let t = P.globalFieldsToLua res
            var = TableConst [NamedField (Name "read_globals") (TableConst t)]
            result = P.replaceAssign st moduleVar var
            doc = P.pprint $ P.Block result Nothing
        putStrLn $ "write to file " ++ outFile
        bracket (openFile outFile WriteMode) hClose $ \h -> do
            T.hPutStr h (T.pack (show doc))
        -- return ()

        -- putStrLn . show . 

-- stdsAssign names
-- withContent :: FilePath -> (T.Text -> a) -> Maybe IO a
-- withContent path act = bracket (openFile path ReadMode) hClose $ \h -> do
--     content <- T.hGetContents h
--     return $ Just (act content)

-- withContents :: [FilePath] -> (T.Text -> a) -> IO [a]
-- withContents ::

-- getFileContents :: [FilePath] -> IO [T.Text]
-- getFileContents paths = forM paths $ \path -> do
--     bracket (openFile path ReadMode) hClose $ \h -> do
--         return $ T.hGetContents h
    

buildGlobals :: [P.GlobalField] -> FilePath -> IO [P.GlobalField]
buildGlobals res path = do
    bracket (openFile path ReadMode) hClose $ \h -> do
        content <- T.hGetContents h
        putStrLn $ "working on " ++ path
        return $ P.mergeGlobalFields res (P.generateGlobalFields . P.parseContent $ content)

path :: FilePath
path = "../../script"

test :: IO ()
test = do
    let st = P.parseContent "module('modname', package.seeall)" 
    -- let st = P.parseContent "function c() end function a.c() end" 
    -- let st = P.parseContent "function a.c() end" 
    -- let st = P.parseContent "function a.b.c() end" 
    -- let st = P.parseContent "module('modulename')"
    putStrLn . show $ st
    -- putStrLn . show $ P.generateGlobalFields st

testFile :: IO ()
testFile = do
    let path = "./test/lua/simple.lua"
    bracket (openFile path ReadMode) hClose $ \h -> do
        content <- T.hGetContents h
        let st = P.parseContent content
        let res = P.mergeGlobalFields [] (P.generateGlobalFields st)
        putStrLn . show $ res
        -- putStrLn . show $ P.generateGlobalFields st
        -- putStrLn . show $ content

testParse :: IO ()
testParse = do 
    let path = "./test/lua/simple.lua"
    bracket (openFile path ReadMode) hClose $ \h -> do
        content <- T.hGetContents h
        putStrLn . show . P.parseContent $ content

-- parseContent :: T.Text -> []

-- parseFile :: FilePath -> [Stat]

-- contentToSyntax :: T.Text -> [Stat]
-- contentToSyntax ::

-- test :: IO ()
-- test = do
--     paths <- getRecursiveContents $ path
--     result <- forM paths $ \path -> 
--         bracket (openFile path ReadMode) hClose $ \h -> do
--             content <- T.hGetContents h
--             -- return $ P.parseLua content
--             content <- B.hGetContents h
--             -- putStrLn . show . take 1 . lines . BC.unpack $ content
--         -- putStrLn path
--     return ()

-- printGlobals :: IO ()
-- printGlobals = do
--     result <- parseFile "test/lua/simple.lua"
--     case result of
--         Right (Block xs whatever) -> do
--             let a = (Block (allGlobals xs) whatever)
--             putStrLn . show $ pprint a
--         _ -> putStrLn "none"
--     -- putStrLn . show $ result

-- printStat :: IO ()
-- printStat = do
--     result <- parseFile "test/lua/simple.lua"
--     case result of
--         Right x -> do
--             putStrLn . show $ x
--             -- putStrLn . show . Lua.pprint $ x
--         _ -> putStrLn "none"