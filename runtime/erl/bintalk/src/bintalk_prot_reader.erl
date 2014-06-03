%% @doc bintalk binary protocol reader.
%%
-module(bintalk_prot_reader).

-export([read_mid/1]).
-export([read_array/4]).
-export([read_int64/2]).
-export([read_uint64/2]).
-export([read_double/2]).
-export([read_float/2]).
-export([read_int32/2]).
-export([read_uint32/2]).
-export([read_int16/2]).
-export([read_uint16/2]).
-export([read_int8/2]).
-export([read_uint8/2]).
-export([read_bool/2]).
-export([read_string/2]).
-export([read_binary/2]).
-export([read_enum/2]).
-export([read_struct/2]).

%% @doc Read a message id.
-spec read_mid(B::binary()) -> {V::integer(), R::binary()}.
read_mid(<<V:16/little-unsigned-integer, R/binary>>) -> {V, R}.


-spec read_array(atom(), non_neg_integer(), any(), binary())->{[any()], binary()}.
read_array(FunName, ArrMax, Val, B)->
	{Len, R} = read_dyn_size(B),
	true = Len =< ArrMax,
	read_array_item(FunName, Len, Val, [], R).

-spec read_int64(0, binary())->{integer(), binary()}.
read_int64(_, <<V:64/little-signed-integer, R/binary>>) -> {V, R}.

-spec read_uint64(0, binary())->{non_neg_integer(), binary()}.
read_uint64(_, <<V:64/little-unsigned-integer, R/binary>>) -> {V, R}.

-spec read_double(0, binary())->{float(), binary()}.
read_double(_, <<V:64/little-float, R/binary>>) -> {V, R}.

-spec read_float(0, binary())->{float(), binary()}.
read_float(_, <<V:32/little-float, R/binary>>) -> {V, R}.

-spec read_int32(0, binary())->{integer(), binary()}.
read_int32(_, <<V:32/little-signed-integer, R/binary>>) -> {V, R}.

-spec read_uint32(0, binary())->{non_neg_integer(), binary()}.
read_uint32(_, <<V:32/little-unsigned-integer, R/binary>>) -> {V, R}.

-spec read_int16(0, binary())->{integer(), binary()}.
read_int16(_, <<V:16/little-signed-integer, R/binary>>) -> {V, R}.

-spec read_uint16(0, binary())->{non_neg_integer(), binary()}.
read_uint16(_, <<V:16/little-unsigned-integer, R/binary>>) -> {V, R}.

-spec read_int8(0, binary())->{integer(), binary()}.
read_int8(_, <<V:8/little-signed-integer, R/binary>>) -> {V, R}.

-spec read_uint8(0, binary())->{non_neg_integer(), binary()}.
read_uint8(_, <<V:8/little-unsigned-integer, R/binary>>) -> {V, R}.

-spec read_bool(0, binary())->{boolean(), binary()}.
read_bool(_, <<0:8, R/binary>>) -> {false, R};
read_bool(_, <<1:8, R/binary>>) -> {true, R}.

-spec read_string(non_neg_integer(), binary())->{string(), binary()}.
read_string(ValMax, B) ->
	{Len, R} = read_dyn_size(B),
	true = Len =< ValMax,
	<<Str:Len/binary, R1/binary>> = R,
	{binary_to_list(Str), R1}.

-spec read_binary(non_neg_integer(), binary())->{binary(), binary()}.
read_binary(ValMax, B) ->
	{Len, R} = read_dyn_size(B),
	true = Len =< ValMax,
	<<B1:Len/binary, R1/binary>> = R,
	{B1, R1}.

-spec read_enum(non_neg_integer(), binary())->{non_neg_integer(), binary()}.
read_enum(ValMax, <<V:8/little-unsigned-integer, R/binary>>) when (V =< ValMax)-> {V, R}.

-spec read_struct(atom(), binary())->{tuple(), binary()}.
read_struct(T, B) -> T:deserialize(B).


%%----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------
read_array_item(_, 0, _, L, B) ->
	{lists:reverse(L), B};
read_array_item(T, N, Val, L, B) ->
	{V, R} = ?MODULE:(T)(Val, B),
	read_array_item(T, N-1, Val, [V|L], R).

read_dyn_size(<<0:2, S:6, R/binary>>) -> {S, R};
read_dyn_size(<<1:2, S:14/big-unsigned-integer, R/binary>>) -> {S, R};
read_dyn_size(<<2:2, S:22/big-unsigned-integer, R/binary>>) -> {S, R};
read_dyn_size(<<3:2, S:30/big-unsigned-integer, R/binary>>) -> {S, R}.
