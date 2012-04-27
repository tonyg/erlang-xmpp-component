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

-record(jid, {local, domain, resource}).
-record(xmpp_header, {to, from, id, type}).
-record(xmpp_message, {subject, thread, body, extensions = []}).
-record(xmpp_presence, {show, status, priority = 0, extensions = []}).
-record(xmpp_iq, {element}).
-record(xmpp_error, {stanza_kind, error_type, condition, text, extensions = []}).

-define(NS_XMPP_STANZAS, "urn:ietf:params:xml:ns:xmpp-stanzas").
-define(NS_XMPP_DISCO_INFO, "http://jabber.org/protocol/disco#info").
