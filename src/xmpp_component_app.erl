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

-module(xmpp_component_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
    ok = application:start(crypto),
    ok = application:start(xmerl),
    ok = application:start(xmpp_component).

%% ===================================================================
%% Application callbacks
%% ===================================================================

get_env(Key) ->
    case application:get_env(Key) of
        undefined ->
            Report = {missing_configuration_key, xmpp_component, Key},
            error_logger:error_report(Report),
            exit(Report);
        {ok, Val} ->
            Val
    end.

start(_StartType, _StartArgs) ->
    xmpp_component_sup:start_link(get_env(server_host),
                                  get_env(server_port),
                                  get_env(shared_secret),
                                  get_env(component_name),
                                  get_env(handler_module),
                                  get_env(handler_module_args)).

stop(_State) ->
    ok.
