
-module(esyslog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_handler/2, del_handler/1]).

%% Supervisor callbacks
-export([init/1]).

-include("esyslog.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).
-define(EVENT(Handler, Opts), {Handler, {gen_event, add_sup_handler, [?EVENT_HANDLER, Handler, Opts]}, permanent, 5000, worker, [Handler]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    load_handlers(),
    {ok, PID}.

add_handler(Handler, Opts) ->
    supervisor:start_child(?MODULE, ?EVENT(Handler, Opts)). 

del_handler(Handler) ->
    supervisor:terminate_child(?MODULE, Handler),
    supervisor:delete_child(?MODULE, Handler),
    ok.  

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(gen_event, [{local, ?EVENT_HANDLER}]),
        ?CHILD(esyslog_server, [])
    ]} }.

%% ===================================================================
%% Internal functions
%% ===================================================================

load_handlers() ->
    {ok, Handlers} = application:get_env(esyslog, handlers),
    lists:foreach(fun(Handler) ->
        lager:info("Loading handler: ~p~n", [Handler]),
        Opts = case application:get_env(esyslog, Handler) of
            {ok, Any} -> Any;
            undefined -> []
        end,
        add_handler(Handler, Opts)
    end, Handlers).
