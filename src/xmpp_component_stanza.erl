-module(xmpp_component_stanza).

-include("xmpp_component_xml.hrl").
-include("xmpp_component_stanza.hrl").

-export([flip_header/1, flip_header/2]).
-export([from_xe/1, to_xe/1]).
-export([list_to_jid/1, jid_to_list/1]).

flip_header(H = #xmpp_header{type = Type}) ->
    flip_header(H, Type).

flip_header(H = #xmpp_header{to = To, from = From}, Type) ->
    H#xmpp_header{to = From, from = To, type = Type}.

from_xe(E) ->
    Header = extract_header(E),
    case Header#xmpp_header.type of
        "error" ->
            {found, ErrorE} =
                xmpp_component_xml:lookup_child(?NS_JABBER_COMPONENT_ACCEPT, "error", E),
            {found, ErrorType} = xmpp_component_xml:lookup_attr_val("type", ErrorE),
            {Condition, Text, Extensions} = analyze_error(ErrorE#xe.children),
            {Header, #xmpp_error{stanza_kind = E#xe.localName,
                                 error_type = ErrorType,
                                 condition = Condition,
                                 text = Text,
                                 extensions = Extensions}};
        _ ->
            case {E#xe.nsuri, E#xe.localName} of
                {?NS_JABBER_COMPONENT_ACCEPT, "message"} -> {Header, analyze_message(E)};
                {?NS_JABBER_COMPONENT_ACCEPT, "presence"} -> {Header, analyze_presence(E)};
                {?NS_JABBER_COMPONENT_ACCEPT, "iq"} -> {Header, analyze_iq(E)};
                _ -> {Header, E}
            end
    end.

analyze_error([]) ->
    {undefined, undefined, []};
analyze_error([E = #xe{nsuri = ?NS_XMPP_STANZAS, localName = "text"} | Rest]) ->
    {C, _T, X} = analyze_error(Rest),
    {C, lists:flatten(E#xe.children), X};
analyze_error([E = #xe{nsuri = ?NS_XMPP_STANZAS, localName = LocalName} | Rest]) ->
    {C, T, X} = analyze_error(Rest),
    case lists:member(LocalName, ["bad-request",
                                  "conflict",
                                  "feature-not-implemented",
                                  "forbidden",
                                  "gone",
                                  "internal-server-error",
                                  "item-not-found",
                                  "jid-malformed",
                                  "not-acceptable",
                                  "not-allowed",
                                  "not-authorized",
                                  "payment-required",
                                  "recipient-unavailable",
                                  "redirect",
                                  "registration-required",
                                  "remote-server-not-found",
                                  "remote-server-timeout",
                                  "resource-constraint",
                                  "service-unavailable",
                                  "subscription-required",
                                  "undefined-condition",
                                  "unexpected-request"]) of
        true -> {LocalName, T, X};
        false -> {C, T, [E | X]}
    end;
analyze_error([E | Rest]) ->
    {C, T, X} = analyze_error(Rest),
    {C, T, [E | X]}.

string_child(NSURI, LocalName, E) ->
    case xmpp_component_xml:lookup_child(NSURI, LocalName, E) of
        not_found ->
            undefined;
        {found, Child} ->
            lists:flatten(Child#xe.children)
    end.

int_child(NSURI, LocalName, E) ->
    case string_child(NSURI, LocalName, E) of
        undefined -> 0;
        Str -> case catch list_to_integer(Str) of
                   {'EXIT', _} -> 0;
                   V -> V
               end
    end.

analyze_message(E) ->
    #xmpp_message{subject = string_child(?NS_JABBER_COMPONENT_ACCEPT, "subject", E),
                  thread = string_child(?NS_JABBER_COMPONENT_ACCEPT, "thread", E),
                  body = string_child(?NS_JABBER_COMPONENT_ACCEPT, "body", E),
                  extensions = extract_extensions(E)}.

analyze_presence(E) ->
    #xmpp_presence{show = string_child(?NS_JABBER_COMPONENT_ACCEPT, "show", E),
                   status = string_child(?NS_JABBER_COMPONENT_ACCEPT, "status", E),
                   priority = int_child(?NS_JABBER_COMPONENT_ACCEPT, "priority", E),
                   extensions = extract_extensions(E)}.

analyze_iq(E) ->
    #xmpp_iq{children = E#xe.children}.

extract_extensions(#xe{children = Children}) ->
    [C || C = #xe{nsuri = NSURI} <- Children, NSURI =/= ?NS_JABBER_COMPONENT_ACCEPT].

extract_header(#xe{attributes = Attrs}) ->
    #xmpp_header{to =
                     list_to_jid(xmpp_component_xml:lookup_attr_default("to", Attrs, undefined)),
                 from =
                     list_to_jid(xmpp_component_xml:lookup_attr_default("from", Attrs, undefined)),
                 id =
                     xmpp_component_xml:lookup_attr_default("id", Attrs, undefined),
                 type =
                     xmpp_component_xml:lookup_attr_default("type", Attrs, undefined)}.

list_to_jid(X) -> X. %%% FIXME
jid_to_list(X) -> X. %%% FIXME

to_xe({Header, Body}) ->
    wrap_header(Header, body_to_xe(Body)).

body_to_xe(E = #xe{}) -> E;
body_to_xe(#xmpp_message{subject = Subject,
                         thread = Thread,
                         body = Body,
                         extensions = Extensions}) ->
    #xe{nsuri = ?NS_JABBER_COMPONENT_ACCEPT, localName = "message",
        children = add_kid(?NS_JABBER_COMPONENT_ACCEPT, "subject", Subject,
                           add_kid(?NS_JABBER_COMPONENT_ACCEPT, "thread", Thread,
                                   add_kid(?NS_JABBER_COMPONENT_ACCEPT, "body", Body,
                                           Extensions)))};
body_to_xe(#xmpp_presence{show = Show,
                          status = Status,
                          priority = Priority,
                          extensions = Extensions}) ->
    PriorityStr = case Priority of
                      0 -> undefined;
                      _ -> integer_to_list(Priority)
                  end,
    #xe{nsuri = ?NS_JABBER_COMPONENT_ACCEPT, localName = "presence",
        children = add_kid(?NS_JABBER_COMPONENT_ACCEPT, "show", Show,
                           add_kid(?NS_JABBER_COMPONENT_ACCEPT, "status", Status,
                                   add_kid(?NS_JABBER_COMPONENT_ACCEPT, "priority", PriorityStr,
                                           Extensions)))};
body_to_xe(#xmpp_iq{children = Children}) ->
    #xe{nsuri = ?NS_JABBER_COMPONENT_ACCEPT, localName = "presence",
        children = Children};
body_to_xe(#xmpp_error{stanza_kind = Kind,
                       error_type = Type,
                       condition = Condition,
                       text = Text,
                       extensions = Extensions}) ->
    #xe{nsuri = ?NS_JABBER_COMPONENT_ACCEPT, localName = Kind,
        children = [#xe{nsuri = ?NS_JABBER_COMPONENT_ACCEPT, localName = "error",
                        attributes = [#xa{localName = "type", value = Type}],
                        children = [#xe{nsuri = ?NS_XMPP_STANZAS, localName = Condition} |
                                    add_kid(?NS_XMPP_STANZAS, "text", Text,
                                            Extensions)]}]}.

add_kid(_NSURI, _LocalName, undefined, Rest) ->
    Rest;
add_kid(NSURI, LocalName, Value, Rest) ->
    [#xe{nsuri = NSURI, localName = LocalName, children = [Value]} | Rest].

wrap_header(#xmpp_header{to = To, from = From, id = Id, type = Type}, E) ->
    E#xe{attributes = add_attr("to", To,
                               add_attr("from", From,
                                        add_attr("id", Id,
                                                 add_attr("type", Type,
                                                          E#xe.attributes))))}.

add_attr(_LocalName, undefined, Rest) ->
    Rest;
add_attr(LocalName, Value, Rest) ->
    [#xa{localName = LocalName, value = Value} | Rest].
