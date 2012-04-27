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

-module(xmpp_component_connector).

-include("xmpp_component_xml.hrl").

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/1]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%---------------------------------------------------------------------------

-record(state, {server_host, server_port, component_name, shared_secret,
                callback_mfa, callback_state,
                parser_pid, sock, state, stream_id, sax_stack}).

ensure_connected(State = #state{server_host = ServerHost,
                                server_port = ServerPort,
                                sock = disconnected}) ->
    {ok, Sock} = gen_tcp:connect(ServerHost, ServerPort,
                                 [binary,
                                  {packet, 0},
                                  {active, true}]),
    next_action(State#state{sock = Sock, state = new_connection});
ensure_connected(State) ->
    State.

next_action(State = #state{component_name = ComponentName,
                           sock = Sock,
                           state = new_connection}) ->
    ok = gen_tcp:send(Sock,
                      ["<stream:stream xmlns='", ?NS_JABBER_COMPONENT_ACCEPT, "' ",
                       "xmlns:stream='http://etherx.jabber.org/streams' ",
                       "to='", ComponentName, "'>"]),
    State#state{state = waiting_for_server_stream_header};
next_action(State) ->
    State.

send_xml(V, State = #state{sock = Sock}) ->
    ok = gen_tcp:send(Sock, xmpp_component_xml:to_xml(V)),
    State.

handle_sax_event(startDocument, State) -> State;
handle_sax_event({startPrefixMapping, _Prefix, _URI}, State) -> State;
handle_sax_event({endPrefixMapping, _Prefix}, State) -> State;
handle_sax_event({ignorableWhitespace, _Str}, State) -> State;
handle_sax_event({startElement, "http://etherx.jabber.org/streams", "stream", _, Attrs},
                 State = #state{shared_secret = Secret,
                                state = waiting_for_server_stream_header}) ->
    {value, {_, _, _, Id}} = lists:keysearch("id", 3, Attrs),
    %% HT http://www.enchantedage.com/hex-format-hash-for-md5-sha1-sha256-and-sha512
    %% for the following two lines:
    <<HandshakeBin:160/big-unsigned-integer>> = crypto:sha(Id ++ Secret),
    HandshakeIOList = io_lib:format("~40.16.0b", [HandshakeBin]),
    send_xml(#xe{localName="handshake", nsuri = ?NS_JABBER_COMPONENT_ACCEPT,
                 children=[HandshakeIOList]},
             State#state{state = waiting_for_server_handshake});
handle_sax_event({startElement, NSURI, LocalName, {Prefix, _}, Attrs},
                 State = #state{sax_stack = Stack}) ->
    State#state{sax_stack = [{[], {Prefix, NSURI, LocalName, translate_attrs(Attrs)}} | Stack]};
handle_sax_event({characters, Str},
                 State = #state{sax_stack = [{ChildrenRev, ParentInfo} | StackTail]}) ->
    case ChildrenRev of
        [] ->
            State#state{sax_stack = [{[Str | ChildrenRev], ParentInfo} | StackTail]};
        [#xe{} | _] ->
            State#state{sax_stack = [{[Str | ChildrenRev], ParentInfo} | StackTail]};
        [PreviousStr | OtherChildrenRev] ->
            State#state{sax_stack = [{[[PreviousStr, Str] | OtherChildrenRev], ParentInfo}
                                     | StackTail]}
    end;
handle_sax_event({endElement, NSURI, LocalName, {Prefix, _}},
                 State = #state{sax_stack = [{ChildrenRev, {Prefix, NSURI, LocalName, Attrs}}
                                             | StackTail]}) ->
    Element = #xe{prefix = Prefix,
                  nsuri = NSURI,
                  localName = LocalName,
                  attributes = Attrs,
                  children = lists:reverse(ChildrenRev)},
    case StackTail of
        [] ->
            handle_stanza(Element, State#state{sax_stack = StackTail});
        [{SiblingsRev, ParentInfo} | StackTailTail] ->
            State#state{sax_stack = [{[Element | SiblingsRev], ParentInfo} | StackTailTail]}
    end;
handle_sax_event(E, _State = #state{sax_stack = Stack}) ->
    exit({unhandled_sax_event, E, Stack}).

translate_attrs(Attrs) ->
    [#xa{prefix = P, nsuri = N, localName = L, value = V} || {N, P, L, V} <- Attrs].

handle_stanza(#xe{nsuri = ?NS_JABBER_COMPONENT_ACCEPT, localName = "handshake"},
              State = #state{state = waiting_for_server_handshake}) ->
    State#state{state = running};
handle_stanza(E, State = #state{callback_mfa = {M,F,A}, callback_state = OldCallbackState}) ->
    NewCallbackState = apply(M, F, [OldCallbackState, self(), E | A]),
    io:format("STANZA ~p~n~s~n", [E, lists:flatten(xmpp_component_xml:to_xml(E))]),
    State#state{callback_state = NewCallbackState}.

%%---------------------------------------------------------------------------

parser_main(ConnectorPid) ->
    ContFun = fun (State) ->
                      receive
                          {received_bytes, Bytes} ->
                              {Bytes, State}
                      end
              end,
    EventFun = fun (Event, _Location, State) ->
                       ConnectorPid ! {sax_event, Event},
                       State
               end,
    xmerl_sax_parser:stream(<<>>,
                            [{continuation_fun, ContFun},
                             {event_fun, EventFun}]),
    exit(xml_stream_ended).

%%---------------------------------------------------------------------------

init([ServerHost, ServerPort, ComponentName, SharedSecret, CallbackMFA, CallbackState]) ->
    ConnectorPid = self(),
    {ok, ensure_connected(#state{server_host = ServerHost,
                                 server_port = ServerPort,
                                 component_name = ComponentName,
                                 shared_secret = SharedSecret,
                                 callback_mfa = CallbackMFA,
                                 callback_state = CallbackState,
                                 parser_pid = spawn_link(fun () -> parser_main(ConnectorPid) end),
                                 sock = disconnected,
                                 sax_stack = []})}.

terminate(Reason, _State = #state{parser_pid = ParserPid}) ->
    exit(ParserPid, {connector_terminated, Reason}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {stop, {bad_call, _Request}, State}.

handle_cast(_Request, State) ->
    {stop, {bad_cast, _Request}, State}.

handle_info({tcp, _Sock, Bytes}, State = #state{parser_pid = ParserPid}) ->
    ParserPid ! {received_bytes, Bytes},
    {noreply, State};
handle_info({sax_event, SaxEvent}, State) ->
    {noreply, handle_sax_event(SaxEvent, State)};
handle_info({send_stanza, E}, State) ->
    io:format("SENDING ~p~n~s~n", [E, lists:flatten(xmpp_component_xml:to_xml(E))]),
    {noreply, send_xml(E, State)};
handle_info(_Message, State) ->
    {stop, {bad_info, _Message}, State}.
