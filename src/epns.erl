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
