# Erlang Push Notifications - epns
**epns** is Erlang Push Notifications. This is a small client library for sending FCM/GCM/APNS Push Notifications

[![Hex.pm Version](http://img.shields.io/hexpm/v/epns.svg?style=flat)](https://hex.pm/packages/epns)

## Goals
**epns**(Erlang Push Notifications) library aims to provide a simple way for push FCM/GCM/APNS notifications.

## Documentation
### Build & Run
```sh
$ git clone https://github.com/vkatsuba/shot.git
$ cd shot
$ wget https://s3.amazonaws.com/rebar3/rebar3
$ chmod u+x ./rebar3
$ ./rebar3 shell
```
### Run `dialyzer`
```sh
$ ./rebar3 dialyzer
```
### Run `xref`
```sh
$ ./rebar3 do xref
```
### Clean `epns`
```sh
$ ./rebar3 clean
```
### Add `epns` to project: [Rebar3](https://www.rebar3.org/)
* Edit file **rebar.config**:
```erlang
{deps, [
    {epns, "1.1.0"},
]}.
```

## Send Push Notifications
### Send FCM by Erlang
```erlang
%%% Useful links:
%%%  * https://firebase.google.com/docs/cloud-messaging/http-server-ref
%%%  * https://developer.clevertap.com/docs/find-your-fcm-sender-id-fcm-server-api-key

FCMData = #{
    key => "TheFcmServerApiKey",
    url => "https://fcm.googleapis.com/fcm/send",
    playload => #{
        to => <<"GoogleUserToken">>,
        priority => <<"high">>,
        data => #{
            <<"title">> => <<"Some Title">>,
            <<"some_custom_field">> => true
        }
    }
}.

epns:push(fcm, FCMData).
```
### Send APNS by Erlang
```erlang
%%% Useful links:
%%%  * https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CreatingtheNotificationPayload.html
%%%  * https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/BinaryProviderAPI.html
%%%  * https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/LegacyNotificationFormat.html

APNSData = #{
    cert => "/full/patch/to/cert.pem",
    key => "/full/patch/to/key.key",
    url => "gateway.push.apple.com",
    token => <<"VoipTokenOfUserDevice">>,
    playload => #{
        aps => #{alert => <<"Some Title Of Alert APNS">>},
        data => #{
            <<"title">> => <<"Some Title">>,
            <<"custom_field">> => <<"some_data">>
        }
    }
}.

epns:push(apns, APNSData).
```

# Support
v.katsuba.dev@gmail.com
