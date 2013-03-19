%% @doc bintalk binary protocol reader.
%%
-module(bintalk_prot_reader).

-export([read_mid/1]).
-export([read/4]).

%% @doc Read a message id.
-spec read_mid(B::binary()) -> {V::integer(), R::binary()}.
read_mid(<<V:16/little-unsigned-integer, R/binary>>) -> {V, R}.


%% @doc Read the value of a specific type.
-spec read(T::atom(), ArrMax::non_neg_integer(), ValMax::integer(), B::binary()) -> 
		  {V::any(), R::binary()}.
read(T, ArrMax, ValMax, B) when ArrMax > 0 ->
	case read_dyn_size(B) of
		{Len, R} when (Len =< ArrMax) ->
			rd_array_item(T, Len, ValMax, [], R)
	end;
read(int64, 0, 0, <<V:64/little-signed-integer, R/binary>>) -> {V, R};
read(uint64, 0, 0, <<V:64/little-unsigned-integer, R/binary>>) -> {V, R};
read(double, 0, 0, <<V:64/little-float, R/binary>>) -> {V, R};
read(float, 0, 0, <<V:32/little-float, R/binary>>) -> {V, R};
read(int32, 0, 0, <<V:32/little-signed-integer, R/binary>>) -> {V, R};
read(uint32, 0, 0, <<V:32/little-unsigned-integer, R/binary>>) -> {V, R};
read(int16, 0, 0, <<V:16/little-signed-integer, R/binary>>) -> {V, R};
read(uint16, 0, 0, <<V:16/little-unsigned-integer, R/binary>>) -> {V, R};
read(int8, 0, 0, <<V:8/little-signed-integer, R/binary>>) -> {V, R};
read(uint8, 0, 0, <<V:8/little-unsigned-integer, R/binary>>) -> {V, R};
read(bool, 0, 0, <<0:8, R/binary>>) -> {false, R};
read(bool, 0, 0, <<1:8, R/binary>>) -> {true, R};
read(string, 0, ValMax, B) ->
	case read_dyn_size(B) of
		{Len, R} when (Len =< ValMax) ->
			<<Str:Len/binary, R1/binary>> = R,
			{binary_to_list(Str), R1}
	end;
read(binary, 0, ValMax, B) ->
	case read_dyn_size(B) of
		{Len, R} when (Len =< ValMax) ->
			<<B1:Len/binary, R1/binary>> = R,
			{B1, R1}
	end;
read(enum, 0, ValMax, <<V:8/little-unsigned-integer, R/binary>>) when (V =< ValMax)-> {V, R};
read(UT, 0, 0, B) -> UT:deserialize(B).


%%----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------
rd_array_item(_, 0, _, L, B) ->
	{lists:reverse(L), B};
rd_array_item(T, N, ValMax, L, B) ->
	{V, R} = read(T, 0, ValMax, B),
	rd_array_item(T, N-1, ValMax, [V|L], R).

read_dyn_size(<<0:2, S:6, R/binary>>) -> {S, R};
read_dyn_size(<<1:2, S:14/big-unsigned-integer, R/binary>>) -> {S, R};
read_dyn_size(<<2:2, S:22/big-unsigned-integer, R/binary>>) -> {S, R};
read_dyn_size(<<3:2, S:30/big-unsigned-integer, R/binary>>) -> {S, R}.