-module(esyslog_event_disk).
-author('bombadil@bosqueviejo.net').

-behaviour(gen_event).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {fd, file}).

-include("esyslog.hrl").

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @doc Creates an event manager.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @spec add_handler() -> ok | {'EXIT',Reason} | term()
%% @doc Adds an event handler
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?EVENT_HANDLER, ?MODULE, []).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Path = case proplists:get_value(path_log, Args) of
        undefined -> "/var/log";
        Any -> Any
    end,
    {ok, reopen(#state{file=Path ++ "/esyslog.log"})}.

reopen(State) ->
    {Year, Month, Day} = erlang:date(),
    File =
        filename:dirname(State#state.file) ++
        "/esyslog." ++
        io_lib:format("~w~.2.0w~.2.0w", [Year, Month, Day]) ++
        ".log",
    #state{file=File, fd=case State#state.file of
        File ->
            State#state.fd;
        _ ->
            case State#state.fd of
                [] -> ok;
                Any -> file:close(Any)
            end,
            {ok, Fdes} = file:open(File, [append, raw]),
            Fdes
    end}.

%%--------------------------------------------------------------------
%% @spec
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% @doc an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%% @end
%%--------------------------------------------------------------------
handle_event({syslog, _Severity, _Facility, DateTime, Machine, Message}, State) ->
    NewState = reopen(State),
    % TODO: add filter for Severity and Facility
    file:write(NewState#state.fd, io_lib:format("~s ~s ~s~n", [DateTime, Machine, Message])),
    {ok, NewState};

handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% @doc Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @spec
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% @doc This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    case State#state.fd of
        [] -> ok;
        Any -> file:close(Any)
    end.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
