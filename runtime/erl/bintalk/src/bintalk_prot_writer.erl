%% @doc bintalk binary protocol writer.
%%
-module(bintalk_prot_writer).

-export([write_mid/1]).
-export([write_array/2]).
-export([write_int64/1]).
-export([write_uint64/1]).
-export([write_double/1]).
-export([write_float/1]).
-export([write_int32/1]).
-export([write_uint32/1]).
-export([write_int16/1]).
-export([write_uint16/1]).
-export([write_int8/1]).
-export([write_uint8/1]).
-export([write_bool/1]).
-export([write_string/1]).
-export([write_binary/1]).
-export([write_enum/1]).
-export([write_struct/1]).

-spec write_mid(Mid::integer()) -> binary().
write_mid(Mid)-> <<Mid:16/little-unsigned-integer>>.

-spec write_array(atom(), [any()])-> iolist().
write_array(FunName, L)->
	BLen = write_dyn_size(length(L)),
	BArr = [?MODULE:(FunName)(I) || I <- L],
	[BLen|BArr].

-spec write_int64(integer())-> binary().
write_int64(V) -> <<V:64/little-signed-integer>>.

-spec write_uint64(non_neg_integer())->binary().
write_uint64(V) -> <<V:64/little-unsigned-integer>>.

-spec write_double(integer()|float())->binary().
write_double(V) -> <<V:64/little-float>>.

-spec write_float(integer()|float())->binary().
write_float(V) -> <<V:32/little-float>>.

-spec write_int32(integer())->binary().
write_int32(V) -> <<V:32/little-signed-integer>>.

-spec write_uint32(non_neg_integer())->binary().
write_uint32(V) -> <<V:32/little-unsigned-integer>>.

-spec write_int16(integer())->binary().
write_int16(V) -> <<V:16/little-signed-integer>>.

-spec write_uint16(non_neg_integer())->binary().
write_uint16(V) -> <<V:16/little-unsigned-integer>>.

-spec write_int8(integer())->binary().
write_int8(V) -> <<V:8/little-signed-integer>>.

-spec write_uint8(non_neg_integer())->binary().
write_uint8(V) -> <<V:8/little-unsigned-integer>>.

-spec write_bool(boolean())->binary().
write_bool(true) -> <<1:8>>;
write_bool(false) -> <<0:8>>.

-spec write_string(string()|binary())->iolist().
write_string(V) when is_binary(V) -> [write_dyn_size(byte_size(V)),V];
write_string(V) -> [write_dyn_size(length(V)),V].

-spec write_binary(binary())->iolist().
write_binary(V) -> [write_dyn_size(byte_size(V)),V].

-spec write_enum(non_neg_integer())->binary().
write_enum(V) -> write_uint8(V).

-spec write_struct(tuple())->iolist().
write_struct(V) -> 
	M = element(1, V), 
	M:serialize(V).

%%----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------
write_dyn_size(S) when is_integer(S) andalso S >= 0 ->
	if
		S =< 16#3F ->
			<<0:2, S:6>>;
		S =< 16#3FFF ->
			<<1:2, S:14/big-unsigned-integer>>;
		S =< 16#3FFFFFFF ->
			<<2:2, S:22/big-unsigned-integer>>;
		true ->
			<<3:2, S:30/big-unsigned-integer>>
	end.
