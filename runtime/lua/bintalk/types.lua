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
    int64	= function() return 0 end,
    uint64 = function() return 0 end,
    double = function() return 0 end,
    float	= function() return 0 end,
	int32	= function() return 0 end,
    uint32 = function() return 0 end,
    int16	= function() return 0 end,
    uint16 = function() return 0 end,
    int8	= function() return 0 end,
    uint8	= function() return 0 end,
    bool	= function() return false end,
    string	= function() return "" end,
    binary	= function() return "" end,
    array	= function() return {} end,
    enum	= function() return 0 end,
    enum16	= function() return 0 end,
    _create_enum_type = create_enum_type,
    _create_usertype = create_user_type,
}