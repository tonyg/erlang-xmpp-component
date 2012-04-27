-record(jid, {local, domain, resource}).
-record(xmpp_header, {to, from, id, type}).
-record(xmpp_message, {subject, thread, body, extensions = []}).
-record(xmpp_presence, {show, status, priority = 0, extensions = []}).
-record(xmpp_iq, {element}).
-record(xmpp_error, {stanza_kind, error_type, condition, text, extensions = []}).

-define(NS_XMPP_STANZAS, "urn:ietf:params:xml:ns:xmpp-stanzas").
-define(NS_XMPP_DISCO_INFO, "http://jabber.org/protocol/disco#info").
