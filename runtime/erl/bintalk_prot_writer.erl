-module(bintalk_prot_writer).

%%----------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------
-export([
	write_mid/1,
	write/3
	]).

%%-----------------------------------------------------------------
%% Func: write_mid/1
%% Returns: 
%%		 Binary
%%-----------------------------------------------------------------
write_mid(Mid)->
	<<Mid:16/little-unsigned-integer>>.
%%-----------------------------------------------------------------
%% Func: write/3
%% Args: 
%%       Type - data type.
%%		 Array - is array.
%%       Value - data type value.
%% Returns: 
%%		 Binary
%%-----------------------------------------------------------------
%% array.
write(T, true, V) ->
	DSize = write_dyn_size(length(V)),
	Array = [write(T, false, I) || I <- V],
	iolist_to_binary([DSize|Array]);
%% normal types.
write(int64, false, V) ->
	<<V:64/little-signed-integer>>;
write(uint64, false, V) ->
	<<V:64/little-unsigned-integer>>;
write(double, false, V) ->
	<<V:64/little-float>>;
write(float, false, V) ->
	<<V:32/little-float>>;
write(int32, false, V) ->
	<<V:32/little-signed-integer>>;
write(uint32, false, V) ->
	<<V:32/little-unsigned-integer>>;
write(int16, false, V) ->
	<<V:16/little-signed-integer>>;
write(uint16, false, V) ->
	<<V:16/little-unsigned-integer>>;
write(int8, false, V) ->
	<<V:8/little-signed-integer>>;
write(uint8, false, V) ->
	<<V:8/little-unsigned-integer>>;
write(bool, false, V) ->
	case V of
		false -> <<0:8>>;
		true -> <<1:8>>
	end;
write(string, false, V) ->
	iolist_to_binary([write_dyn_size(length(V)),V]);
write(binary, true, V) ->
	iolist_to_binary([write_dyn_size(length(V)),V]);
%% enum
write(enum, false, V) ->
	write(uint8, false, V);
%% user type.
write(UT, false, V) ->
	UT:serialize(V).

%%----------------------------------------------------------------------
%% Local Functions
%%----------------------------------------------------------------------
%% dynamic size.
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