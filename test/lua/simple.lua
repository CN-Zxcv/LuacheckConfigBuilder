
-- Block [FunCall (NormalFunCall (PEVar (VarName (Name "module"))) 
--                               (Args [String "'module_name'",
--                                      PrefixExp (PEVar (SelectName (PEVar (VarName (Name "package"))) (Name "seeall")))]))] Nothing
-- module('module_name', package.seeall)

-- Block [FunCall (NormalFunCall (PEVar (VarName (Name "module"))) 
--                               (Args [String "'module.name'",
--                                      PrefixExp (PEVar (SelectName (PEVar (VarName (Name "package"))) (Name "seeall")))]))] Nothing
-- module('module.name', package.seeall)

-- module()

-- note

-- global_int = 1
-- global_string = ""

-- local local_a = {}
-- local_a = {}

-- a = nil
-- a.b = {}
-- a.b.c = {}
-- a.c = {}


-- --[[
-- denoted_global = 1
-- ]]

-- global_b = {}
-- global_b = {}

-- function global_func()
--     function global_func_in_global_func()
--         function global_func_in_global_func_1()

--         end
--     end
-- end

-- local function local_func()
--     function global_func_in_local_func()
--     end
--     t = {}
-- end

-- local local_table = {
--     a_in_local_table = {},
--     b_in_local_table = {},
-- }

-- global_table = {
--     a_in_global_table = {
--         a_in_global_table_1 = {
--         },
--     },
--     b_in_global_table = {},
-- }

-- Block [FunAssign (FunName (Name "module_name") [Name "func_name"] Nothing) (FunBody [] False (Block [] Nothing))] Nothing
-- function module_name.func_name() end

-- Block [FunAssign (FunName (Name "module_name") [Name "func_name",Name "func_name"] Nothing) (FunBody [] False (Block [] Nothing))] Nothing
-- function module_name.func_name.func_name() end

-- Block [Assign [VarName (Name "a")] [Nil]] Nothing
-- a = nil

-- Block [Assign [SelectName (PEVar (VarName (Name "a"))) (Name "b")] [Nil]] Nothing
-- a.b = nil

-- Block [Assign [SelectName (PEVar (SelectName (PEVar (VarName (Name "a"))) (Name "b"))) (Name "c")] [Nil]] Nothing
-- a.b.c = nil

-- Block [LocalAssign [Name "a"] (Just [Nil])] Nothing
-- local a = nil

-- local a.b = nil

-- Block [LocalAssign [Name "a"] (Just [Nil]),Assign [VarName (Name "a")] [Nil]] Nothing
-- local a = nil
-- a = nil

-- Block [Assign [VarName (Name "a")] [TableConst [NamedField (Name "b") Nil]]] Nothing
-- a = {
--     b = nil
-- }

-- Block [Assign [VarName (Name "a")] [TableConst [ExpField (PrefixExp (PEVar (VarName (Name "b")))) Nil]]] Nothing
-- a = {
--     [b] = nil
-- }

--[=[
[Assign [VarName (Name "a")] [TableConst 
    [ NamedField (Name "read_only") (Bool False)
    , NamedField (Name "other_fields") (Bool False)
    , NamedField (Name "fields") (TableConst 
        [ NamedField (Name "b") (TableConst 
            [ NamedField (Name "read_only") (Bool False)
            , NamedField (Name "other_fields") (Bool False)
            , NamedField (Name "fields") (TableConst [])])
        , NamedField (Name "c") (TableConst 
            [ NamedField (Name "read_only") (Bool False)
            , NamedField (Name "other_fields") (Bool False)
            , NamedField (Name "fields") (TableConst [])
            ]
        )])
    ]
]]
--]=]

-- a = {
--     read_only = false,
--     other_fields = false,
--     fields = {
--         b = {
--             read_only = false,
--             other_fields = false,
--             fields = {},
--         },
--         c = {
--             read_only = false,
--             other_fields = false,
--             fields = {},
--         }
--     },
-- }

module('module', package.seeall)

a = {}
a["b"] = {
    e = nil,
    f = nil,
}
-- a[b] = {
--     c = nil,
--     d = nil,
-- }

-- a = {1,2,4}