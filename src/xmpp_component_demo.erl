%% Copyright (C) 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>
%%
%% This file is part of erlang-xmpp-component.
%%
%% erlang-xmpp-component is free software: you can redistribute it
%% and/or modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation, either version 3 of
%% the License, or (at your option) any later version.
%%
%% erlang-xmpp-component is distributed in the hope that it will be
%% useful, but WITHOUT ANY WARRANTY; without even the implied warranty
%% of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with erlang-xmpp-component.  If not, see
%% <http://www.gnu.org/licenses/>.

-module(xmpp_component_demo).

-behaviour(xmpp_component).
-export([init/2, terminate/2, code_change/3, connection_change/2,
         disco_info/2, disco_items/2, vcard/2,
         jid_exists/2,
         handle_iq/6, handle_message/3, handle_presence/6,
         handle_stanza/3]).

-export([start/1]).

-include("xmpp_component_xml.hrl").
-include("xmpp_component_stanza.hrl").

-record(state, {connector_pid}).

send_to(ConnectorPid, Header, Body) ->
    ok = gen_server:cast(ConnectorPid, {xmpp, Header, Body}).

send(Header, Body, State = #state{connector_pid = ConnectorPid}) ->
    ok = send_to(ConnectorPid, Header, Body),
    State.

active_message(Body) ->
    #xmpp_message{body = Body,
                  extensions = [#xe{nsuri = "http://jabber.org/protocol/chatstates",
                                    localName = "active"}]}.

%%---------------------------------------------------------------------------

%% This is used to start the demo application.
start([Hostname, PortStr, Secret, ComponentName]) ->
    gen_server:start(xmpp_component_connector,
                     [Hostname, list_to_integer(PortStr), Secret, ComponentName, ?MODULE, []],
                     []).

%%---------------------------------------------------------------------------

init(ConnectorPid, _Args) ->
    {ok, #state{connector_pid = ConnectorPid}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connection_change(_ConnectedOrDisconnected, State) ->
    {ok, State}.

disco_info(_Who, State) ->
    {ok,
     [#disco_identity{category = "robot", type = "erlang", name = "Demo Robot"}],
     [?NS_XMPP_DISCO_ITEMS, ?NS_VCARD_TEMP],
     State}.

disco_items(_Who, State) ->
    {ok, [], State}.

vcard(Who, State) ->
    {ok, [#xe{nsuri = "vcard-temp", localName = "FN", children = ["Robot " ++ Who]}], State}.

jid_exists(_Jid, State) ->
    {true, State}.

handle_iq(_From, _To, _Type, _Header, _Element, State) ->
    {error, {"cancel", "feature-not-implemented", "Not yet implemented"}, State}.

handle_message(H, #xmpp_message{body = undefined, extensions = Xs}, State) ->
    case xmpp_component_xml:lookup_children("http://jabber.org/protocol/chatstates", Xs) of
        [#xe{localName = "composing"} | _] ->
            {ok, send(xmpp_component_stanza:flip_header(H, "chat"),
                      active_message("You are typing!"),
                      State)};
        [#xe{localName = "paused"} | _] ->
            {ok, send(xmpp_component_stanza:flip_header(H, "chat"),
                      active_message("You have paused!"),
                      State)};
        [#xe{localName = "active"} | _] ->
            {ok, send(xmpp_component_stanza:flip_header(H, "chat"),
                      active_message("You are no longer typing!"),
                      State)};
        _ ->
            {ok, State}
    end;
handle_message(H, #xmpp_message{body = B}, State)
  when B =/= undefined ->
    {ok, send(xmpp_component_stanza:flip_header(H, "chat"),
              active_message("Acknowledged: " ++ B),
              State)};
handle_message(_Header, _Body, State) ->
    io:format("IGNORING ~p~n", [{_Header, _Body}]),
    {ok, State}.

handle_presence(_From, _To, "subscribe", H, _Body,
                State = #state{connector_pid = ConnectorPid}) ->
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, "subscribed"), #xmpp_presence{}),
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, undefined), #xmpp_presence{}),
    {ok, State};
handle_presence(_From, _To, "unsubscribe", H, _Body,
                State = #state{connector_pid = ConnectorPid}) ->
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, "unavailable"), #xmpp_presence{}),
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, "unsubscribed"), #xmpp_presence{}),
    {ok, State};
handle_presence(_From, _To, "probe", H, _Body,
                State = #state{connector_pid = ConnectorPid}) ->
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, undefined), #xmpp_presence{}),
    {ok, State};
handle_presence(_From, _To, _Type, _Header, _Body, State) ->
    io:format("IGNORING ~p~n", [{_Header, _Body}]),
    {ok, State}.

handle_stanza(_Header, _Body, State) ->
    io:format("IGNORING ~p~n", [{_Header, _Body}]),
    {ok, State}.
