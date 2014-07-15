# XMPP Component Protocol (XEP-0114) Library for Erlang

## Running the demo component

Any server that supports XEP-0114 will do. For example, when initially
developing this library, both ejabberd and openfire worked just fine.

### Openfire

Perform a basic installation and configuration of Openfire.

When logged in to the admin console, under the "Server" tab, under
"Server Settings", there's an "External Components" option. Select
that.

Enable the "External Components" service.

By default, the demo external component will use the following
settings to contact the main XMPP server. You can update the port
number and shared secret in the Openfire admin console, or override
the demo's defaults by editing the Makefile or supplying new values on
the `make` command-line.

| Setting                   | Makefile variable | Default value  |
| ---                       | ---               | ---            |
| Hostname to connect to    | `DEMOSERVER`      | `localhost`    |
| Port number to connect to | `DEMOPORT`        | `8888`         |
| Secret shared with server | `DEMOSECRET`      | `secret`       |
| XMPP domain of component  | `DEMODOMAIN`      | `e.`*hostname* |

The `DEMODOMAIN` setting needs some explanation. If your XMPP server
is responsible for managing JIDs ending in `@xmpp.example.com`, for
instance, then the extension should take responsibility for JIDs
ending in `@e.xmpp.example.com`. There's no good way of determining
the correct JID domain, so by default we guess that it is exactly the
output of `hostname -f`.

You will almost always need to supply `DEMODOMAIN` unless you are
developing against a non-production XMPP server running on localhost.

To start the demo external component, make sure the XMPP server is
running, and run `make run`. For example, to start the demo with
default settings,

    make run

To start the demo, using port 5275 (Openfire's default) to contact the
XMPP server,

    make run DEMOPORT=5275

To start the demo component, making it claim responsibility for JIDs
ending in `e.xmpp.example.com`,

    make run DEMODOMAIN=e.xmpp.example.com

## Interacting with the demo component

The demo component is not much more than a simple echo service.

Assuming it is configured to use JID domain `e.xmpp.example.com`,

 - Try using service discovery on `e.xmpp.example.com`.

 - Try chatting to any JID in `e.xmpp.example.com`; for example, `bot@e.xmpp.example.com`.

 - Try adding any JID in the domain to your roster.

## Copyright and license

	Copyright (C) 2012, 2014 Tony Garnock-Jones <tonygarnockjones@gmail.com>

	erlang-xmpp-component is free software: you can redistribute it
	and/or modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation, either version 3 of
	the License, or (at your option) any later version.

	erlang-xmpp-component is distributed in the hope that it will be
	useful, but WITHOUT ANY WARRANTY; without even the implied warranty
	of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with erlang-xmpp-component.  If not, see
	<http://www.gnu.org/licenses/>.
