%% @doc bintalk binary protocol writer.
%%
-module(bintalk_prot_writer).

-export([write_mid/1]).
-export([write/3]).

%% @doc Write a message id.
-spec write_mid(Mid::integer()) -> binary().
write_mid(Mid)-> <<Mid:16/little-unsigned-integer>>.


%% @doc Write the value of a specific type.
-spec write(T::atom(), IsArr::boolean(), V::any()) -> iodata().
write(T, true, V) ->
	Len = write_dyn_size(length(V)),
	Array = [write(T, false, I) || I <- V],
	[Len|Array];
write(int64, false, V) -> <<V:64/little-signed-integer>>;
write(uint64, false, V) -> <<V:64/little-unsigned-integer>>;
write(double, false, V) -> <<V:64/little-float>>;
write(float, false, V) -> <<V:32/little-float>>;
write(int32, false, V) -> <<V:32/little-signed-integer>>;
write(uint32, false, V) -> <<V:32/little-unsigned-integer>>;
write(int16, false, V) -> <<V:16/little-signed-integer>>;
write(uint16, false, V) -> <<V:16/little-unsigned-integer>>;
write(int8, false, V) -> <<V:8/little-signed-integer>>;
write(uint8, false, V) -> <<V:8/little-unsigned-integer>>;
write(bool, false, true) -> <<1:8>>;
write(bool, false, false) -> <<0:8>>;
write(string, false, V) -> [write_dyn_size(length(V)),V];
write(binary, false, V) -> [write_dyn_size(byte_size(V)),V];
write(enum, false, V) -> write(uint8, false, V);
write(UT, false, V) -> UT:serialize(V).

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