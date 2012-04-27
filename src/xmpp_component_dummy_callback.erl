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

active_message(Body) ->
    #xmpp_message{body = Body,
                  extensions = [#xe{nsuri = "http://jabber.org/protocol/chatstates",
                                    localName = "active"}]}.

handle(ConnectorPid, {H, #xmpp_message{body = undefined, extensions = Xs}}) ->
    case xmpp_component_xml:lookup_children("http://jabber.org/protocol/chatstates", Xs) of
        [#xe{localName = "composing"} | _] ->
            reply(ConnectorPid, {xmpp_component_stanza:flip_header(H, "chat"),
                                 active_message("You are typing!")});
        [#xe{localName = "paused"} | _] ->
            reply(ConnectorPid, {xmpp_component_stanza:flip_header(H, "chat"),
                                 active_message("You have paused!")});
        _ ->
            ok
    end;
handle(ConnectorPid, {H, #xmpp_message{body = B}})
  when B =/= undefined ->
    reply(ConnectorPid, {xmpp_component_stanza:flip_header(H, "chat"),
                         active_message("Acknowledged: " ++ B)});
handle(ConnectorPid, {H = #xmpp_header{type = "subscribe"}, #xmpp_presence{}}) ->
    reply(ConnectorPid,
          [{xmpp_component_stanza:flip_header(H, "subscribed"), #xmpp_presence{}},
           {xmpp_component_stanza:flip_header(H, undefined), #xmpp_presence{}}]);
handle(ConnectorPid, {H = #xmpp_header{type = "unsubscribe"}, #xmpp_presence{}}) ->
    reply(ConnectorPid,
          [{xmpp_component_stanza:flip_header(H, "unavailable"), #xmpp_presence{}},
           {xmpp_component_stanza:flip_header(H, "unsubscribed"), #xmpp_presence{}}]);
handle(ConnectorPid, {H = #xmpp_header{type = Type}, #xmpp_iq{element = Element}}) ->
    case Type of
        "result" ->
            %% No reply permitted. Unsolicited result, too, which is weird! Just ignore it.
            ok;
        _ ->
            %% "get" or "set". "error" is dealt with elsewhere during parsing.
            case handle_iq(H, Element) of
                {reply, ReplyElement} ->
                    reply(ConnectorPid, {xmpp_component_stanza:flip_header(H, "result"),
                                         #xmpp_iq{element = ReplyElement}});
                no_reply ->
                    ok;
                {error, ErrorType, Condition, Text} ->
                    reply(ConnectorPid, {xmpp_component_stanza:flip_header(H, "error"),
                                         #xmpp_error{stanza_kind = "iq",
                                                     error_type = ErrorType,
                                                     condition = Condition,
                                                     text = Text,
                                                     extensions = [Element]}})
            end
    end;
handle(_ConnectorPid, Item) ->
    io:format("IGNORING ~p~n", [Item]),
    ok.

handle_iq(#xmpp_header{type = "get", to = To},
          #xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "query"}) ->
    {reply,
     #xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "query",
         children = [#xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "identity",
                         attributes = [#xa{localName = "category", value = "robot"},
                                       #xa{localName = "type", value = "erlang"},
                                       #xa{localName = "name", value = To}]},
                     #xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "feature",
                         attributes = [#xa{localName = "var",
                                           value = "http://jabber.org/protocol/disco#info"},
                                       #xa{localName = "var", value = "vcard-temp"}]}]}};
handle_iq(#xmpp_header{type = "get", to = To}, #xe{nsuri = "vcard-temp", localName = "vCard"}) ->
    {reply,
     #xe{nsuri = "vcard-temp", localName = "vCard",
         children = [#xe{nsuri = "vcard-temp", localName = "FN",
                         children = ["Robot " ++ To]}]}};
handle_iq(_, _) ->
    {error, "cancel", "feature-not-implemented", "Not yet implemented"}.
