-module(bintalk_prot_reader).

%%----------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------
-export([
	read_mid/1,
	read/4
	]).

%%----------------------------------------------------------------------
%% Func: read_mid/1
%% Returns:
%%		{Value, RestBinary}  	
%%----------------------------------------------------------------------
read_mid(B) ->
	<<V:16/little-unsigned-integer, R/binary>> = B,
	{V, R}.
%%----------------------------------------------------------------------
%% Func: read/4
%% Args: 
%%		Type - data type.
%%		Array Max Size.
%% 		Field Max Size.
%%		Binary
%% Returns:
%%		{Value, RestBinary}  	
%%----------------------------------------------------------------------
%% array.
read(T, ArrMax, ValMax, B) when ArrMax > 0 ->
	{Len, R} = read_dyn_size(B),
	if
		Len =< ArrMax -> 
			rd_array_item(T, Len, ValMax, [], R)
	end;
%% normal types.
read(int64, 0, 0, B) -> 
	<<V:64/little-signed-integer, R/binary>> = B, 
	{V, R};
read(uint64, 0, 0, B) ->
	<<V:64/little-unsigned-integer, R/binary>> = B,
	{V, R};
read(double, 0, 0, B) ->
	<<V:64/little-float, R/binary>> = B,
	{V, R};
read(float, 0, 0, B) ->
	<<V:32/little-float, R/binary>> = B,
	{V, R};
read(int32, 0, 0, B) ->
	<<V:32/little-signed-integer, R/binary>> = B,
	{V, R};
read(uint32, 0, 0, B) ->
	<<V:32/little-unsigned-integer, R/binary>> = B,
	{V, R};
read(int16, 0, 0, B) ->
	<<V:16/little-signed-integer, R/binary>> = B,
	{V, R};
read(uint16, 0, 0, B) ->
	<<V:16/little-unsigned-integer, R/binary>> = B,
	{V, R};
read(int8, 0, 0, B) ->
	<<V:8/little-signed-integer, R/binary>> = B,
	{V, R};
read(uint8, 0, 0, B) ->
	<<V:8/little-unsigned-integer, R/binary>> = B,
	{V, R};
read(bool, 0, 0, B) ->
	case B of
		<<0:8, R/binary>> -> {false, R};
		<<1:8, R/binary>> -> {true, R}
	end;
read(string, 0, ValMax, B) ->
	{Len, R} = read_dyn_size(B),
	if
		Len =< ValMax ->
			<<Str:Len/binary, R1/binary>> = R,
			{binary_to_list(Str), R1}
	end;
%% binary.
read(binary, 0, ValMax, B) ->
	{Len, R} = read_dyn_size(B),
	if
		Len =< ValMax ->
			<<B1:Len/binary, R1/binary>> = R,
			{B1, R1}
	end;
%% enum.
read(enum, 0, ValMax, B) ->
	<<V:8/little-unsigned-integer, R/binary>> = B,
	if V =< ValMax -> {V, R} end;
%% user type.
read(UT, 0, 0, B) ->
	UT:deserialize(B).


%%----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------

rd_array_item(_, 0, _, L, B) ->
	{lists:reverse(L), B};
rd_array_item(T, N, ValMax, L, B) ->
	{V, R} = read(T, 0, ValMax, B),
	rd_array_item(T, N-1, ValMax, [V|L], R).

%% dynamic size
read_dyn_size(<<0:2, S:6, R/binary>>) ->
	{S, R};
read_dyn_size(<<1:2, S:14/big-unsigned-integer, R/binary>>) ->
	{S, R};
read_dyn_size(<<2:2, S:22/big-unsigned-integer, R/binary>>) ->
	{S, R};
read_dyn_size(<<3:2, S:30/big-unsigned-integer, R/binary>>) ->
	{S, R}.