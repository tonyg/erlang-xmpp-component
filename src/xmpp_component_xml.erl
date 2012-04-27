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

-module(xmpp_component_xml).

-include("xmpp_component_xml.hrl").

-export([to_xml/1, to_xml/2]).
-export([lookup_attr/2, lookup_attr/3]).
-export([lookup_attr_val/2, lookup_attr_val/3]).
-export([lookup_attr_default/3, lookup_attr_default/4]).
-export([lookup_child/3, lookup_children/3, lookup_children/2]).

to_xml(V) ->
    to_xml(V, [{?NS_JABBER_COMPONENT_ACCEPT, ""}]).

to_xml(#xe{prefix = Prefix,
           nsuri = NSURI,
           localName = LocalName,
           attributes = Attrs,
           children = Children}, NSMap) ->
    {ExtraPrefixes1, NSMap1} = extend_nsmap(Prefix, NSURI, "", NSMap),
    TagStr = make_tagstr(Prefix, LocalName),
    {XmerlAttrs, {ExtraPrefixes2, NSMap2}} = xa_to_xml(Attrs, NSMap1),
    Start = ["<", TagStr, ExtraPrefixes1, ExtraPrefixes2, XmerlAttrs],
    case [to_xml(Child, NSMap2) || Child <- Children] of
        [] ->
            [Start, "/>"];
        FormattedChildren ->
            [Start, ">", FormattedChildren, "</", TagStr, ">"]
    end;
to_xml(IOList, _NSMap) ->
    xmerl_lib:export_text(IOList).

make_tagstr("", LocalName) ->
    LocalName;
make_tagstr(Prefix, LocalName) ->
    [Prefix, ":", LocalName].

extend_nsmap(Prefix, NSURI, Prefixes, NSMap) ->
    case lists:keysearch(Prefix, 2, NSMap) of
        {value, {NSURI, _}} ->
            %% Prefix already correctly bound.
            {Prefixes, NSMap};
        {value, {_, _}} ->
            %% Prefix bound to something else! Override.
            extend_nsmap1(Prefix, NSURI, Prefixes, NSMap);
        false ->
            %% Prefix unbound.
            extend_nsmap1(Prefix, NSURI, Prefixes, NSMap)
    end.

extend_nsmap1("", NSURI, Prefixes, NSMap) ->
    {[" xmlns=\"", xmerl_lib:export_attribute(NSURI), "\"", Prefixes],
     [{NSURI, ""} | NSMap]};
extend_nsmap1(Prefix, NSURI, Prefixes, NSMap) ->
    {[" xmlns:", Prefix, "=\"", xmerl_lib:export_attribute(NSURI), "\"", Prefixes],
     [{NSURI, Prefix} | NSMap]}.

% Prefix is a suggestion for attributes. It's only ever used when the
% NSURI is not already defined, AND when there's no clash with the
% prefix. The URI, if empty, means *no prefix*, no matter what. If
% nonempty, then look at maybe using the prefix; if there's no prefix,
% cons one up.

xa_to_xml([],
          NSMap) ->
    {"", {"", NSMap}};
xa_to_xml([#xa{prefix = Prefix0, nsuri = NSURI0, localName = LocalName, value = Value} | Rest],
          NSMap) ->
    {XmerlAttrs, {Prefixes1, NSMap1}} = xa_to_xml(Rest, NSMap),
    case xa_prefix(Prefix0, NSURI0, NSMap) of
        no_prefix ->
            {[" ", LocalName, "=\"", xmerl_lib:export_attribute(Value), "\"", XmerlAttrs],
             {Prefixes1, NSMap1}};
        {Prefix, NSURI} ->
            {[" ", make_tagstr(Prefix, LocalName), "=\"",
              xmerl_lib:export_attribute(Value), "\"", XmerlAttrs],
             extend_nsmap(Prefix, NSURI, Prefixes1, NSMap1)}
    end.

xa_prefix(_Prefix, "", _NSMap) -> no_prefix;
xa_prefix("", NSURI, NSMap) ->
    case lists:keysearch(NSURI, 1, NSMap) of
        {value, {_, Prefix}} ->
            {Prefix, NSURI};
        false ->
            {unused_prefix(0, NSMap), NSURI}
    end;
xa_prefix(Prefix, NSURI, NSMap) ->
    case lists:keysearch(Prefix, 2, NSMap) of
        {value, {NSURI, _}} ->
            % Match.
            {Prefix, NSURI};
        {value, {_, _}} ->
            % Already in use. Avoid this prefix.
            xa_prefix("", NSURI, NSMap);
        false ->
            % Prefix not in use.
            {Prefix, NSURI}
    end.

unused_prefix(N, NSMap) ->
    Prefix = "ns" ++ integer_to_list(N),
    case lists:keysearch(Prefix, 2, NSMap) of
        {value, _} ->
            unused_prefix(N + 1, NSMap);
        false ->
            Prefix
    end.

lookup_attr(Key, #xe{attributes = Attrs}) ->
    lookup_attr(Key, Attrs);
lookup_attr({NSURI, LocalName}, Attrs) ->
    lookup_attr(NSURI, LocalName, Attrs);
lookup_attr(LocalName, Attrs) ->
    lookup_attr("", LocalName, Attrs).

lookup_attr(NSURI, LocalName, #xe{attributes = Attrs}) ->
    lookup_attr(NSURI, LocalName, Attrs);
lookup_attr(_NSURI, _LocalName, []) ->
    not_found;
lookup_attr(NSURI, LocalName, [A = #xa{nsuri = NSURI, localName = LocalName} | _]) ->
    {found, A};
lookup_attr(NSURI, LocalName, [#xa{} | Rest]) ->
    lookup_attr(NSURI, LocalName, Rest).

extract_val(not_found) -> not_found;
extract_val({found, #xa{value = V}}) -> {found, V}.

lookup_attr_val(Key, E) -> extract_val(lookup_attr(Key, E)).
lookup_attr_val(NSURI, LocalName, E) -> extract_val(lookup_attr(NSURI, LocalName, E)).

extract_default(not_found, D) -> D;
extract_default({found, #xa{value = V}}, _D) -> V.

lookup_attr_default(Key, E, D) -> extract_default(lookup_attr(Key, E), D).
lookup_attr_default(NSURI, LocalName, E, D) -> extract_default(lookup_attr(NSURI, LocalName, E), D).

lookup_child(NSURI, LocalName, #xe{children = Kids}) ->
    lookup_child(NSURI, LocalName, Kids);
lookup_child(_NSURI, _LocalName, []) ->
    not_found;
lookup_child(NSURI, LocalName, [E = #xe{nsuri = NSURI, localName = LocalName} | _]) ->
    {found, E};
lookup_child(NSURI, LocalName, [_ | Rest]) ->
    lookup_child(NSURI, LocalName, Rest).

lookup_children(NSURI, LocalName, #xe{children = Kids}) ->
    lookup_children(NSURI, LocalName, Kids);
lookup_children(NSURI, LocalName, Kids) ->
    [K || K = #xe{nsuri = N, localName = L} <- Kids, N =:= NSURI, L =:= LocalName].

lookup_children(NSURI, #xe{children = Kids}) ->
    lookup_children(NSURI, Kids);
lookup_children(NSURI, Kids) ->
    [K || K = #xe{nsuri = N} <- Kids, N =:= NSURI].
