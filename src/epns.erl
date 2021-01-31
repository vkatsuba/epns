%% MIT License

%% Copyright (c) 2021 Viacheslav Katsuba <v.katsuba.dev@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(epns).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([push/2]).

%%% ==================================================================
%%% Macros
%%% ==================================================================

-define(COMMAND_REQ, 1).
-define(EXPIRY, (os:system_time(second) + 604800)).
-define(TOKEN_LENGTH, 32).
-define(TIMEOUT, 500).

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Send push notification
%% @end
%% -------------------------------------------------------------------
-spec push(Provider :: atom(), Data :: maps:map()) -> Result :: tuple().

push(apns, Data) when is_map(Data) ->
    apns(Data);
push(fcm, Data) when is_map(Data) ->
    fcm(Data);
push(_, _) ->
    {error, bad_args}.

%%% ==================================================================
%%% Internal/Private functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Send APNS push notification
%% @end
%% -------------------------------------------------------------------
-spec apns(Data :: maps:map()) -> Result :: tuple().

apns(#{playload := P, key := K, cert := C, url := U, token := T}) ->
    case ssl:connect(U, 2195, [{certfile, C}, {keyfile, K}, {mode, binary}, {verify, verify_none}], ?TIMEOUT) of
        {ok, Socket} ->
            ID = rand:uniform(9999),
            Payload = jsx:encode(P),
            PayloadLen = erlang:byte_size(Payload),
            DeviceToken = binary_to_integer(T, 16),
            Packet = <<?COMMAND_REQ:8, ID:32/big, ?EXPIRY:4/big-unsigned-integer-unit:8,
                       ?TOKEN_LENGTH:16/big, DeviceToken:256/integer, PayloadLen:16/big, Payload/binary>>,
            _ = ssl:send(Socket, Packet),
            _ = ssl:close(Socket, 0),
            {ok, apns};
        {error, Reason} ->
            {error, Reason}
    end.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Send FCM push notification
%% @end
%% -------------------------------------------------------------------
-spec fcm(Data :: maps:map()) -> Result :: tuple().

fcm(#{playload := P, key := K, url := U}) ->
    _ = httpc:set_options([{keep_alive_timeout, 0}]),
    {_, Resp} = httpc:request(post, {U, [{"Authorization", "key=" ++ K}], "application/json", jsx:encode(P)}, [], []),
    handle_status(get_http_resp_code(Resp), get_http_resp_body(Resp)).

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Handler status code
%% @end
%% -------------------------------------------------------------------
-spec handle_status(StatusCode :: integer(), RespBody :: maps:map()) -> Result :: tuple().

handle_status(200, RespBody) ->
    jsx:decode(list_to_binary(RespBody), [return_maps]);
handle_status(201, _) ->
    {ok, #{multicast_id=> <<>>, success => 1, failure => 0, canonical_ids => 0, results => []}};
handle_status(400, RespBody) ->
    {error, RespBody};
handle_status(401, _) ->
    {error, auth_error};
handle_status(500, _) ->
    {error, server_error};
handle_status(_, Reason) ->
    {error, Reason}.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Returns HTTP response code
%% @end
%% -------------------------------------------------------------------
-spec get_http_resp_code(HttpcResult :: tuple()) -> Code :: pos_integer().

get_http_resp_code({{_, Code, _}, _, _}) ->
    Code;
get_http_resp_code({Code, _}) ->
    Code;
get_http_resp_code(_) ->
    0.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Returns HTTP response body
%% @end
%% -------------------------------------------------------------------
-spec get_http_resp_body(HttpcResult :: tuple()) -> Body :: list().

get_http_resp_body({_, _, Body}) ->
    Body;
get_http_resp_body({_, Body}) ->
    Body;
get_http_resp_body(_) ->
    "Not Implemented".