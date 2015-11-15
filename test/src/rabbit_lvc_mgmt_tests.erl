-module(rabbit_lvc_mgmt_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

lvc_test_() ->
    {setup,
        fun() ->
            {ok, Conn} = amqp_connection:start(#amqp_params_network{}),
            {ok, Ch} = amqp_connection:open_channel(Conn),
            amqp_channel:call(Ch, #'confirm.select'{}),
            exchange_declare(Ch, <<"test-lvc">>),
            {Conn, Ch}
        end,
        fun({Conn, _}) ->
            amqp_connection:close(Conn)
        end,
        fun lvc_mgmt/1}.

exchange_declare(Ch, X) ->
    amqp_channel:call(Ch, #'exchange.declare'{exchange = X,
                                              type     = <<"x-lvc">>}).

lvc_mgmt({_, Ch}) ->
    [?_assertEqual({200, "Body1"},
        apply(
            fun(C) ->
                publish(C, <<"test-lvc">>, <<>>, <<"Body0">>),
                publish(C, <<"test-lvc">>, <<>>, <<"Body1">>),
                request("/api/lvc/%2f/test-lvc")
            end, [Ch])
            ),
    ?_assertEqual({200, "Body2"},
        apply(
            fun(C) ->
                publish(C, <<"test-lvc">>, <<"key1">>, <<"Body2">>),
                request("/api/lvc/%2f/test-lvc/key1")
            end, [Ch])
            )].

publish(Ch, X, RK, Payload) ->
    amqp_channel:cast(Ch, #'basic.publish'{exchange    = X,
                                           routing_key = RK},
                      #amqp_msg{payload = Payload}),
    true = amqp_channel:wait_for_confirms(Ch).

request(Uri) ->
    {ok, {{_V, Code, _R}, _H, Payload}} =
        httpc:request(get, {"http://localhost:15672"++Uri, [auth_header("guest", "guest")]}, [], []),
    {Code, Payload}.

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

% end of file
