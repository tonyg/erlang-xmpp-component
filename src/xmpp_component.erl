-module(xmpp_component).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {init, 2}, %% same as gen_server, with addition of connector PID as first arg
     {terminate, 2}, %% same as gen_server
     {code_change, 3}, %% same as gen_server
     {connection_change, 2}, %% connection state, state
     {jid_exists, 2}, %% jid name, state
     %% OPTIONAL {disco_info, 2}, %% to, state
     %% OPTIONAL {disco_items, 2}, %% to, state
     %% OPTIONAL {vcard, 2}, %% to, state
     {handle_iq, 6}, %% from, to, type, header, element, state
     {handle_message, 3}, %% header, body, state
     {handle_presence, 6}, %% from, to, type, header, presence item, state
     {handle_stanza, 3} %% header, element, state
    ];
behaviour_info(_Other) ->
    undefined.
