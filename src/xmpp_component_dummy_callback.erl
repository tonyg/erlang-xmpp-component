-module(xmpp_component_dummy_callback).

-include("xmpp_component_xml.hrl").
-include("xmpp_component_stanza.hrl").

-export([callback/3]).

callback(S, ConnectorPid, E) ->
    {Header, Body} = xmpp_component_stanza:from_xe(E),
    io:format("--> ~p~n", [{Header, Body}]),
    ok = handle(ConnectorPid, {Header, Body}),
    S.

reply(_ConnectorPid, []) ->
    ok;
reply(ConnectorPid, [V | Rest]) ->
    reply(ConnectorPid, V),
    reply(ConnectorPid, Rest);
reply(ConnectorPid, {Header, Body}) ->
    ConnectorPid ! {send_stanza, xmpp_component_stanza:to_xe({Header, Body})},
    ok.

handle(ConnectorPid, {H, #xmpp_message{body = B}})
  when B =/= undefined ->
    reply(ConnectorPid, {xmpp_component_stanza:flip_header(H, "chat"),
                         #xmpp_message{body = "Acknowledged: " ++ B}});
handle(ConnectorPid, {H = #xmpp_header{type = "subscribe"}, #xmpp_presence{}}) ->
    reply(ConnectorPid,
          [{xmpp_component_stanza:flip_header(H, "subscribed"), #xmpp_presence{}},
           {xmpp_component_stanza:flip_header(H, undefined), #xmpp_presence{}}]);
handle(ConnectorPid, {H = #xmpp_header{type = "unsubscribe"}, #xmpp_presence{}}) ->
    reply(ConnectorPid,
          [{xmpp_component_stanza:flip_header(H, "unavailable"), #xmpp_presence{}},
           {xmpp_component_stanza:flip_header(H, "unsubscribed"), #xmpp_presence{}}]);
handle(ConnectorPid, {H, #xmpp_iq{}}) ->
    reply(ConnectorPid,
          {xmpp_component_stanza:flip_header(H, "error"),
           #xmpp_error{stanza_kind = "iq",
                       error_type = "cancel",
                       condition = "feature-not-implemented",
                       text = "Not yet implemented"}});
handle(_ConnectorPid, Item) ->
    io:format("IGNORING ~p~n", [Item]),
    ok.
