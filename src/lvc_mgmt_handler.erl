%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(lvc_mgmt_handler).

-export([init/1, service_available/2, allowed_methods/2, is_authorized/2,
        content_types_provided/2, resource_exists/2, generate_etag/2,
        to_bytes/2]).

-include_lib("rabbitmq_management/include/rabbit_mgmt.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("rabbitmq_lvc/include/rabbit_lvc_plugin.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(DEFAULT_CONTENT, "application/octet-stream").

init(_Config) -> {ok, #context{}}.

service_available(ReqData, Context) ->
    % try to obtain the requested resource
    case get_content(ReqData) of
        {ok, Content} -> {true, ReqData, Context#context{impl=Content}};
        {error, _Reason} -> {false, ReqData, Context}
    end.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context=#context{impl=none}) ->
    % despite the request will fail we need to announce accepted content-types
    {[{?DEFAULT_CONTENT, to_bytes}], ReqData, Context};
content_types_provided(ReqData, Context=#context{impl=#basic_message{content=Content}}) ->
    {#'P_basic'{content_type=ContentType}, _Payload} = rabbit_basic:from_content(Content),
    case ContentType of
        % default content-type as per HTTP standard
        undefined -> {[{?DEFAULT_CONTENT, to_bytes}], ReqData, Context};
        Type -> {[{binary_to_list(Type), to_bytes}], ReqData, Context}
    end.

resource_exists(ReqData, Context=#context{impl=none}) ->
    {false, ReqData, Context};
resource_exists(ReqData, Context=#context{impl=#basic_message{}}) ->
    {true, ReqData, Context}.

generate_etag(ReqData, Context=#context{impl=none}) ->
    {undefined, ReqData, Context};
generate_etag(ReqData, Context=#context{impl=#basic_message{id=Id}}) ->
    {base64:encode_to_string(Id), ReqData, Context}.

to_bytes(ReqData, Context=#context{impl=none}) ->
    rabbit_mgmt_util:not_found(topic_not_found, ReqData, Context);
to_bytes(ReqData, Context=#context{impl=#basic_message{content=Content}}) ->
    {_Props, Payload} = rabbit_basic:from_content(Content),
    {Payload, ReqData, Context}.

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_monitor(ReqData, Context).

% --

get_content(ReqData) ->
    get_content(rabbit_mgmt_util:id(vhost, ReqData),
               rabbit_mgmt_util:id(exchange, ReqData),
               if_none(rabbit_mgmt_util:id(key, ReqData), <<>>)).

get_content(VHost, Exchange, Key) ->
    case catch mnesia:dirty_read(?LVC_TABLE,
                           #cachekey{exchange=rabbit_misc:r(VHost, exchange, Exchange),
                                     routing_key=Key}) of
        [] -> {ok, none};
        [#cached{content=Content}] -> {ok, Content};
        {aborted, Reason} -> {error, Reason}
    end.

if_none(none, Else) -> Else;
if_none(Value, _Else) -> Value.

% end of file
