-module(esyslog_server).
-author('bombadil@bosqueviejo.net').

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Port = 514, %% TODO: let configure it.
    {ok, Socket} = gen_udp:open(Port),
    lager:info("Listen in port ~p~n", [Port]),
    {ok, #state{socket=Socket}}.

%%--------------------------------------------------------------------
%% @spec
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    lager:debug("Received via CALL (from: ~p): ~p~n", [From, Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    lager:debug("Received via CAST: ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {udp, _Fd, IpTuple, Port, Msg} = Info,
    Ip = parse_ip(IpTuple),
    lager:debug("Received via INFO (from: ~p:~p): ~p~n", [Ip, Port, Msg]),
    Pattern = "<([0-9]+)> *([A-Z][a-z]{2} [0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}) (.+?) (.*)",
    {match, [SF, DateTime, Machine, Message]=_Data} = re:run(Msg, Pattern, [{capture, all_but_first, list}]),
    {_,Sev,_,Fac} = parse_facility_severity(SF),
    lager:debug("Parseo '~p.~p' (from: ~p): ~p~n", [Fac,Sev,Machine,Message]),
    gen_event:notify(logger_event_manager, {syslog, Sev, Fac, DateTime, Machine, Message}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    lager:info("Terminated.~n"),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    lager:info("Code change: OldVsn=~p; Extra=~p~n", [OldVsn, Extra]),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parse_ip({A,B,C,D} = Ip) when is_tuple(Ip) ->
    integer_to_list(A) ++ "." ++
    integer_to_list(B) ++ "." ++
    integer_to_list(C) ++ "." ++
    integer_to_list(D).

parse_facility(Facility) when is_integer(Facility) ->
    {ok, Facilities} = application:get_env(esyslog, facilities),
    proplists:get_value(Facility, Facilities).

parse_severity(Severity) when is_integer(Severity) ->
    {ok, Severities} = application:get_env(esyslog, severities),
    case proplists:get_value(Severity, Severities) of
        undefined -> nothing;
        Any -> Any
    end.

parse_facility_severity(Data) when is_list(Data) ->
    parse_facility_severity(list_to_integer(Data));

parse_facility_severity(Data) when is_integer(Data) ->
    {Severity, Facility} = {Data rem 8, Data div 8},
    {Severity, parse_severity(Severity), Facility, parse_facility(Facility)}.
