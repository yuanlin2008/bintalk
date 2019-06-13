BintalkTypes = {}

local function type_number(v)
    if not v then return 0 end
    assert(type(v) == "number")
    return v 
end

local function type_boolean(v)
    if not v then return false end
    assert(type(v) == "boolean")
    return v 
end

local function type_string(v)
    if not v then return "" end
    assert(type(v) == "string")
    return v 
end

local function type_array(v, type)
    if not v then return {} end
    assert(type(v) == "table")
    local array = {}
    for i,item in ipairs(v) do
        table.insert(array, BintalkTypes[type](item))
    end
    return array 
end

---Create a enum table.
---@param name string @enum name
---@param e table<string, number> @original enum table
---@return table<string, number> @wrapped enum table.
local function create_enum_type(name, e)
    --enum metatable.
    local mt = {}
    mt.__index = function(t, k)
        local v = e[k]
        if v == nil then
            error("Attempt to get invalid enum item:"..name.."."..k)
        end
        return v
    end
    mt.__newindex = function(t, k, v)
        error("Attempt to modify enum item:"..name.."."..k)
    end
    mt.__call = function(t)
        return 0
    end
    mt.__pairs = function(t)
        local function iter(t, k)
            local v
            k, v = next(e, k)
            if v then
                return k,v
            end
        end
        return iter, e, nil
    end
    return setmetatable({}, mt)
end

---Create a user type object factory function.
---@param name string @type name
---@param class table<string, any> @type default property table.
---@return fun() @factory function.
local function create_user_type(name, class)
    -- user type metatable.
    local mt = {}
    mt.__index = function(t, k)
        -- Get default value.
        local dv = class[k]
        if dv == nil then
            error("Attempt to get invalid struct field:"..name.."."..k)
        end
        -- Get value.
        local v = t._values[k]
        if v then
            return v
        end
        return dv
    end
    mt.__newindex = function(t, k, v)
        -- Get default value.
        local dv = class[k]
        if dv == nil then
            error("Attempt to set invalid struct field:"..name.."."..k)
        end
        if type(dv) ~= type(v) then
            error("Attempt to set struct field with invalid value type:"..name.."."..k.." "..type(v))
        end
        rawset(t._values, k, v)
    end
    return function()
        return setmetatable({_values = {}}, mt)
    end
end

BintalkTypes = {
    int64	= type_number,
    uint64 = type_number,
    double = type_number,
    float	= type_number,
	int32	= type_number,
    uint32 = type_number,
    int16	= type_number,
    uint16 = type_number,
    int8	= type_number,
    uint8	= type_number,
    bool	= type_number,
    string	= type_string,
    binary	= type_string,
    array	= type_array,
    enum	= type_number,
    enum16	= type_number,
    _create_enum_type = create_enum_type,
    _create_usertype = create_user_type,
}