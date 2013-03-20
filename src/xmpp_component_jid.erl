%% -*- erlang -*-
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

-module(xmpp_component_jid).

-include("xmpp_component_stanza.hrl").

-export([split/1, unsplit/1, strip_resource/1, equal_modulo_resource/2]).

split(JidBin) when is_binary(JidBin) ->
    case re:run(JidBin, "([^@]*)@([^/]*)(/(.*))?") of
        {match, [_, {LocalStart, LocalLen}, {DomainStart, DomainLen}]} ->
            #jid{local = binary_to_list(binary:part(JidBin, LocalStart, LocalLen)),
                 domain = binary_to_list(binary:part(JidBin, DomainStart, DomainLen)),
                 resource = undefined};
        {match, [_,
                 {LocalStart, LocalLen},
                 {DomainStart, DomainLen},
                 _,
                 {ResourceStart, ResourceLen}]} ->
            #jid{local = binary_to_list(binary:part(JidBin, LocalStart, LocalLen)),
                 domain = binary_to_list(binary:part(JidBin, DomainStart, DomainLen)),
                 resource = binary_to_list(binary:part(JidBin, ResourceStart, ResourceLen))}
    end;
split(JidStr) when is_list(JidStr) ->
    split(iolist_to_binary(JidStr)).

unsplit(#jid{local = Local, domain = Domain, resource = undefined}) ->
    list_to_binary(Local ++ "@" ++ Domain);
unsplit(#jid{local = Local, domain = Domain, resource = Resource}) ->
    list_to_binary(Local ++ "@" ++ Domain ++ "/" ++ Resource).

strip_resource(Jid = #jid{}) ->
    Jid#jid{resource = undefined};
strip_resource(Jid) when is_binary(Jid) orelse is_list(Jid) ->
    unsplit((split(Jid))#jid{resource = undefined}).

equal_modulo_resource(#jid{local = L, domain = D}, #jid{local = L, domain = D}) -> true;
equal_modulo_resource(_, _) -> false.
