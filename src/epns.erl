-module(epns).

-export([push/2]).

%% -------------------------------------------------------------------
%% @doc
%% Send push notification
%% @end
%% -------------------------------------------------------------------
-spec push(Provider :: atom(), Data :: maps:map()) -> Result :: tuple().

push(fcm, Data) when is_map(Data) ->
    epns_fcm:push(Data);
push(apns, Data) when is_map(Data) ->
    epns_apns:push(Data);
push(_, _) ->
    {error, bad_args}.
