# epns
**epns** is Erlang Push Notifications. This is a small client library for sending FCM/GCM/APNS Push Notifications

## Goals
**epns**(Erlang Push Notifications) library aims to provide a simple way for push FCM/GCM/APNS notifications.

## Documentation
### Build & Run
```sh
$ git clone https://github.com/vkatsuba/epns.git
$ cd epns
$ make
```
### Install `epns` to project: [Rebar3](https://www.rebar3.org/)
* Edit file **rebar.config**:
```
...
{deps, [
  ...
  {epns, {git, "git://github.com/vkatsuba/epns.git", {branch, "master"}}},
  ...
]}.
...
```
* Edit file ***.app.src**:
```
...
  {applications,
   [
    ...,
    epns,
    ...
   ]},
...
```
### FCM
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
     }}}.

epns:push(fcm, FCMData).
```
### APNS
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
     }}}.

epns:push(apns, APNSData).
```

### To be continued ...

## Support
v.katsuba.dev@gmail.com
