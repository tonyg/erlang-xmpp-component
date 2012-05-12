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

-module(xmpp_component_sup).

-behaviour(supervisor).

%% API
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Hostname, Port, Secret, ComponentName, HandlerModule, HandlerModuleArgs) ->
    supervisor:start_link(
      {local, ?MODULE}, ?MODULE,
      [Hostname, Port, Secret, ComponentName, HandlerModule, HandlerModuleArgs]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args = [_Hostname, _Port, _Secret, _ComponentName, HandlerModule, _HandlerModuleArgs]) ->
    {ok, { {one_for_one, 5, 10},
           [
            {xmpp_component_connector, {xmpp_component_connector, start_link, [Args]},
             permanent, 5000, worker, [xmpp_component_connector, HandlerModule]}
           ]} }.
