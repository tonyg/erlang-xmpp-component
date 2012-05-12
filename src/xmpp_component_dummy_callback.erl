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

-module(xmpp_component_dummy_callback).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

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

init([ConnectorPid | _Args]) ->    
    {ok, #state{connector_pid = ConnectorPid}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({iq, "get", To, _Header, #xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "query"}},
            _From, State) ->
    I = #xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "query",
            children = [#xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "identity",
                            attributes = [#xa{localName = "category", value = "robot"},
                                          #xa{localName = "type", value = "erlang"},
                                          #xa{localName = "name", value = To}]},
                        #xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "feature",
                            attributes = [#xa{localName = "var",
                                              value = "http://jabber.org/protocol/disco#info"}]},
                        #xe{nsuri = ?NS_XMPP_DISCO_INFO, localName = "feature",
                            attributes = [#xa{localName = "var",
                                              value = "vcard-temp"}]}]},
    {reply, {ok, I}, State};
handle_call({iq, "get", To, _Header, #xe{nsuri = "vcard-temp", localName = "vCard"}},
            _From, State) ->
    {reply, {ok, #xe{nsuri = "vcard-temp", localName = "vCard",
                     children = [#xe{nsuri = "vcard-temp", localName = "FN",
                                     children = ["Robot " ++ To]}]}}, State};
handle_call({iq, _IqType, _IqTo, _Header, _Element}, _From, State) ->
    {reply, {error, "cancel", "feature-not-implemented", "Not yet implemented"}, State};
handle_call(_Request, _From, State) ->
    {stop, {bad_call, _Request}, State}.

handle_cast({xmpp, H, #xmpp_message{body = undefined, extensions = Xs}}, State) ->
    case xmpp_component_xml:lookup_children("http://jabber.org/protocol/chatstates", Xs) of
        [#xe{localName = "composing"} | _] ->
            {noreply, send(xmpp_component_stanza:flip_header(H, "chat"),
                           active_message("You are typing!"),
                           State)};
        [#xe{localName = "paused"} | _] ->
            {noreply, send(xmpp_component_stanza:flip_header(H, "chat"),
                           active_message("You have paused!"),
                           State)};
        _ ->
            {noreply, State}
    end;
handle_cast({xmpp, H, #xmpp_message{body = B}}, State)
  when B =/= undefined ->
    {noreply, send(xmpp_component_stanza:flip_header(H, "chat"),
                   active_message("Acknowledged: " ++ B),
                   State)};
handle_cast({xmpp, H = #xmpp_header{type = "subscribe"}, #xmpp_presence{}},
            State = #state{connector_pid = ConnectorPid}) ->
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, "subscribed"), #xmpp_presence{}),
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, undefined), #xmpp_presence{}),
    {noreply, State};
handle_cast({xmpp, H = #xmpp_header{type = "unsubscribe"}, #xmpp_presence{}},
            State = #state{connector_pid = ConnectorPid}) ->
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, "unavailable"), #xmpp_presence{}),
    send_to(ConnectorPid, xmpp_component_stanza:flip_header(H, "unsubscribed"), #xmpp_presence{}),
    {noreply, State};
handle_cast({xmpp, _Header, _Body}, State) ->
    io:format("IGNORING ~p~n", [{_Header, _Body}]),
    {noreply, State};
handle_cast({connected_to_server, _}, State) ->
    {noreply, State};
handle_cast(_Request, State) ->
    {stop, {bad_cast, _Request}, State}.

handle_info(_Message, State) ->
    {stop, {bad_info, _Message}, State}.
