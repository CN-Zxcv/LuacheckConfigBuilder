# LuacheckConfigBuilder
auto generate .luacheckrc

### demo, bad code, just works for one of my project...

## what is this

some of my codes are writen this way

one file is a class or a singleton that will loaded on _G[what]

with a loader file, every module could be called in any file in the project, saving many require()

```lua
-- file foo
module('foo')
be_class(_ENV, base)

data = {
    a = 1
    b = 1
}

function new(...)
    return deliver(...)
end

function init(self)
    self.word = ''
end

function ctor(self, word)
    self.word = word
end

function say(self)
    print(self.word)
end

```
```lua

--- file main
load('foo.lua')
-- other modules

local a = foo.new('hello')
a.say()

```

this style seemes not be recommend, but works well in hot reload script

```lua
function load(mod)
    package.loaded[mode] = nil
    return require(mod)
end
```
 
#### but

luacheck not works well for this style project

because one file will use many `outside global` values

like below

```lua

-- file c

function something()
    local a = manager.onething()
    if a == resmng.DEF_WHAT then
        --
    else
        --
    end
end

```
in luacheck everything is `NOT FOUND`

manager / manager.onething / resmng / resmng.DEF_WHAT

### so i need to auto build some global values

since luacheck 0.21, `stds` was added, this works

```lua
-- luacheckconfig
std="max+auto_globals, globals"

stds.auto_globals = {
    -- this part will be filled by builder
}

stds.globals = {
    read_globals = {
        -- some values can`t be auto generated, add it here
    }
}

```

### supported globals

```lua

-- globals wraps in module
module('module')
module('module', package.seeall)

-- this all local `a`
local a = {}
a.c = 1

-- global b and all tableconst under b
b = {}
b.a = {
    c = 1
}
function b.c()
end

-- this cause {other_fields=true}
b[d] = {}

-- global d like this not supported yet, only export d,  no d.a
local c = {a = 1}
d = c

```