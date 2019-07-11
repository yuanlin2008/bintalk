---Construct number.
---@param v number
---@return number
local function type_number(v)
    if not v then return 0 end
    assert(type(v) == "number")
    return v 
end

---Construct boolean
---@param v boolean
---@return boolean
local function type_boolean(v)
    if not v then return false end
    assert(type(v) == "boolean")
    return v 
end

---Construct string
---@param v string
---@return string
local function type_string(v)
    if not v then return "" end
    assert(type(v) == "string")
    return v 
end

---Construct array
---@param t any @inner type
---@param v table
local function type_array(t, v)
    if not v then return {} end
    assert(type(v) == "table")
    local array = {}
    for i,item in ipairs(v) do
        table.insert(array, t(item))
    end
    return array 
end

---Enum metatable.
local enum_mt = {}
function enum_mt.__index(t, k)
    local v = t._enum[k]
    if v == nil then
        error("Attempt to get invalid enum item:"..k)
    end
    return v
end
function enum_mt.__newindex(t, k, v)
    error("Attempt to modify enum item:"..k)
end
function enum_mt.__call(t)
    return 0
end
function enum_mt.__pairs(t)
    local function iter(t, k)
        local v
        k, v = next(t._enum, k)
        if v then
            return k,v
        end
    end
    return iter, t._enum, nil
end

---User type metatable.
local struct_mt = {}
function struct_mt.__index(t, k)
    -- Get default value.
    local dv = t._defaults[k]
    if dv == nil then
        error("Attempt to get invalid struct field:"..k)
    end
    -- Get value.
    return rawget(t._values, k)
end
function struct_mt.__newindex(t, k, v)
    -- Get default value.
    local dv = t._defaults[k]
    if dv == nil then
        error("Attempt to set invalid struct field:"..k)
    end
    if type(dv) ~= type(v) then
        error("Attempt to set struct field with invalid value type:"..k.." "..type(v))
    end
    rawset(t._values, k, v)
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
    bool	= type_boolean,
    string	= type_string,
    binary	= type_string,
    enum	= type_number,
    enum16	= type_number,

    array	= type_array,

    _enum_mt = enum_mt,
    _struct_mt = struct_mt,
}