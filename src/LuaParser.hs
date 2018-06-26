{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}

module LuaParser where

import Control.DeepSeq
import Language.Lua
import Language.Lua.PrettyPrinter

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Regex.PCRE as Regex
import qualified Data.Map.Strict as Map

import Text.Regex.PCRE ((=~))

import Debug.Trace

data GlobalField = GlobalField {
      name :: T.Text
    , solved :: Bool
    , readOnly :: Bool
    , otherFields :: Bool
    , fields :: Field
} deriving (Eq, Show)

globalField :: GlobalField
globalField = GlobalField "" False True False Map.empty

build :: Stat -> GlobalField
build x = case x of
    (Assign [a] [b]) -> leftValue a (rightValue b)
    (FunAssign a _) -> leftValue a globalField {solved = True}

-- fromList :: [GlobalField] -> Map.Map T.Text GlobalField
fromList :: [GlobalField] -> Field
fromList xs = Map.fromListWith merge $ map (\x -> (name x, x)) xs

type Field = Map.Map T.Text GlobalField

class GenGlobalField a where
    leftValue :: a -> GlobalField -> GlobalField
    rightValue :: a -> GlobalField

instance GenGlobalField Var where
    leftValue x g = case x of
        (VarName a) -> leftValue a g
        (SelectName a b) -> leftValue a globalField {fields = fromList [leftValue b g]}
        (Select a b) -> if name b' /= "" 
            then leftValue a globalField {fields = fromList [b']}
            else leftValue a b'
            -- leftValue a globalField {fields = [(leftValue b g)]}
            where
                b' = leftValue b g
            -- in
            -- leftValue a globalField {fields = [(leftValue b g)]}
    rightValue x = case x of
        (VarName a) -> rightValue a
        (SelectName a b) -> leftValue a globalField

instance GenGlobalField Name where
    leftValue x g = case x of
        (Name a) -> g {name = a}
    rightValue x = case x of
        (Name a) -> globalField {name = a}
 
instance GenGlobalField Exp where
    leftValue x g = case x of
        String a -> if length t /= 1
            then g {fields = Map.empty}
            else g {name = t !! 0}
            where
                t = splitModuleName a
        _ -> g {fields = Map.empty}
    rightValue x = case x of
        (TableConst xs) -> globalField {solved = True, fields = fromList xs'', otherFields = otherFields}
            where
                xs' = map rightValue xs
                otherFields = any (\x -> name x == "") xs'
                xs'' = filter (\x -> name x /= "") xs'

        _ -> globalField {solved = True}

instance GenGlobalField PrefixExp where
    leftValue x g = case x of
        (PEVar a) -> leftValue a g
    rightValue x = case x of
        (PEVar a) -> rightValue a

instance GenGlobalField TableField where
    leftValue x g = case x of
        _ -> undefined
    rightValue x = case x of
        (NamedField a b) -> leftValue a (rightValue b)
        (ExpField _ _) -> globalField {otherFields = True, solved = True}
        (Field _) -> globalField {otherFields = True, solved = True}

instance GenGlobalField FunName where
    leftValue x g = case x of
        (FunName a [] _) -> leftValue a g
        (FunName a xs _) -> leftValue a (globalField {fields = fromList [leftValue xs g]})
    rightValue x = case x of
        (FunName a xs _) -> undefined

instance (GenGlobalField a) => GenGlobalField [a] where
    leftValue [x] g = leftValue x g
    leftValue (x:xs) g = leftValue x (globalField {fields= fromList [leftValue xs g]})
    leftValue [] g = g
    rightValue _ = undefined
    -- rightValue (x:xs) = rightValue x {solved = True, fields = [rightValue xs]}
    -- rightValue [] = globalField {solved = True}
        
-- test :: IO ()
-- test = do
--     let a = leftValue [Name "a", Name "b", Name "c"] globalField
--     putStrLn . show $ a

allGlobals :: [Stat] -> [Stat]
allGlobals (x:xs) = case x of
    (Assign a _) -> x:allGlobals xs
    (FunAssign a _) -> (FunAssign a emptyFunBody):allGlobals xs
    (LocalAssign [Name a] _) -> allGlobals $ filter (\y -> not (getAssignNameStat y `justSame` Just a)) xs
    _ -> allGlobals xs
allGlobals [] = []

emptyFunBody = FunBody [] False (Block [] Nothing)

justSame :: (Eq a) => Maybe a -> Maybe a -> Bool
justSame (Just a) (Just b) = a == b
justSame _ _ = False

getAssignNamePrefixExp :: PrefixExp -> Maybe T.Text
getAssignNamePrefixExp x = case x of
    (PEVar a) -> getAssignNameVar a
    _ -> Nothing

getAssignNameVar :: Var -> Maybe T.Text
getAssignNameVar x = case x of
    (VarName (Name a)) -> Just a
    (SelectName a _) -> getAssignNamePrefixExp a
    (Select a _) -> getAssignNamePrefixExp a
    -- _ -> Nothing

getAssignNameStat :: Stat -> Maybe T.Text
getAssignNameStat x = case x of
    (Assign [a] _) -> getAssignNameVar a
    (FunAssign (FunName (Name a) _ _) _) -> Just a
    (LocalAssign [Name a] _) -> Just a
    _ -> Nothing


moduleTag = (PEVar (VarName (Name "module")))
packageTag = PrefixExp (PEVar (SelectName (PEVar (VarName (Name "package"))) (Name "seeall")))

-- Hx@2018-05-04 : module("what", package.seeall)
moduleName :: Stat -> Maybe T.Text
moduleName (FunCall (NormalFunCall f (Args (String name:p))))
    -- | f == moduleTag && p == packageTag = Just name
    | f == moduleTag = Just name
    | otherwise = Nothing
moduleName _ = Nothing

generateGlobalFields :: [Stat] -> Field
generateGlobalFields xs@(x:_) = case moduleName x of
    Nothing -> fields
    Just name -> fromNames (splitModuleName name) fields
        where
            fromNames [] fields = fields
            fromNames (x:xs) fields = fromList [globalField {name = x, solved = True, fields = fromNames xs fields}]
    where 
        toPair a = (name a, a)
        fields = Map.fromListWith merge . map (toPair . build) . allGlobals $ xs
generateGlobalFields [] = Map.empty

splitModuleName :: T.Text -> [T.Text]
splitModuleName s = map T.pack (Regex.getAllTextMatches (T.unpack s =~ ("\\w+" :: String)))

parseContent :: T.Text -> [Stat]
parseContent s = case p of
    Right (Block xs _) -> xs
    _ -> []
    where
        p = parseText chunk s

-- sameName :: GlobalField -> GlobalField -> Bool
-- sameName a b = name a == name b

-- mergeGlobalField :: GlobalField -> GlobalField -> GlobalField
-- mergeGlobalField a b = a {readOnly=readOnly',solved=solved',otherFields=otherFields',fields=fields'}
--     where
--         readOnly' = readOnly a || readOnly b
--         solved' = solved a || solved b
--         otherFields' = otherFields a || otherFields b
--         fields' = mergeGlobalFields (fields a) (fields b)
-- 
-- 
-- mergeGlobalFields :: [GlobalField] -> [GlobalField] -> [GlobalField]
-- mergeGlobalFields xs ys =  foldl (\res y -> mergeGlobalFields' res y) xs ys
--     where
--         mergeGlobalFields' (a:as) b = if (sameName a b) 
--             then (mergeGlobalField a b):as
--             else a:(mergeGlobalFields' as b)
--         mergeGlobalFields' [] b = [b {fields = mergeGlobalFields [] (fields b)}]

merge :: GlobalField -> GlobalField -> GlobalField
merge a b = a {readOnly=readOnly',solved=solved',otherFields=otherFields',fields=fields'}
    where
        readOnly' = readOnly a || readOnly b
        solved' = solved a || solved b
        otherFields' = otherFields a || otherFields b
        fields' = Map.unionWith merge (fields a) (fields b)


globalFieldToLua :: GlobalField -> TableField
globalFieldToLua a = if (readOnly' && not otherFields' && null fields')
    then NamedField (Name $ name a) (TableConst [])
    else NamedField (Name $ name a) (TableConst 
        [ NamedField (Name "read_only") (Bool readOnly')
        , NamedField (Name "other_fields") (Bool otherFields')
        , NamedField (Name "fields") (TableConst . globalFieldsToLua $ fields')
        ])
    where
        readOnly' = readOnly a
        otherFields' = otherFields a
        fields' = fields a
    
globalFieldsToLua :: Field -> [TableField]
globalFieldsToLua xs = map globalFieldToLua . filter solved . Map.elems $ xs

-- Hx@2018-05-21 : 任意a.b.c..生成暂时不知道该怎么写，只处理a.b的情况
assignName :: T.Text -> Var
assignName s = SelectName (PEVar (VarName (Name $ t !! 0))) (Name $ t !! 1)
    where
        t = splitModuleName s

replaceAssign :: [Stat] -> Var -> Exp -> [Stat]
replaceAssign (x:xs) name exp = case x of
    Assign [a] _ -> if a == name
        then (Assign [a] [exp]):xs
        else x:replaceAssign xs name exp
    _ -> x:replaceAssign xs name exp