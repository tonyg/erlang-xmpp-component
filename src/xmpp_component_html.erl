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

-module(xmpp_component_html).

-include("xmpp_component_stanza.hrl").
-include("xmpp_component_xml.hrl").

-export([html_im_xe/1, html_im/2, t/2, t/3, link/2]).

html_im_xe(Children) ->
    #xe{nsuri = "http://jabber.org/protocol/xhtml-im",
        localName = "html",
        children =
            [#xe{nsuri = "http://www.w3.org/1999/xhtml",
                 localName = "body",
                 children = Children}]}.

html_im(Text, XhtmlChildren) ->
    #xmpp_message{body = Text,
                  extensions = [html_im_xe(XhtmlChildren)]}.

t(Tag, Kids) ->
    t(Tag, [], Kids).

t(Tag, Attributes, Kids) ->
    #xe{nsuri = "http://www.w3.org/1999/xhtml",
        localName = Tag,
        attributes = [#xa{localName = K, value = V} || {K, V} <- Attributes],
        children = Kids}.

link(Href, Child) ->
    t("a", [{"href", Href}], [Child]).
