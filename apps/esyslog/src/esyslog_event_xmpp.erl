-module(esyslog_event_xmpp).
-author('bombadil@bosqueviejo.net').

-behaviour(gen_event).

-define(SERVER, ?MODULE).

%% API
-export([
    start_link/0, 
    add_handler/0, 
    add_subscription/1, 
    del_subscription/1, 
    get_subscriptions/0
]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-export([process_message/1, process_presence/1, process_iq/1]).

-record(state, {
    jid :: binary(),
    subscriptions :: ordsets:ordset(binary())
}).

-include_lib("ecomponent/include/ecomponent.hrl").
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

get_subscriptions() ->
    gen_event:call(?EVENT_HANDLER, ?MODULE, get).

add_subscription(JID) when is_binary(JID) ->
    gen_event:call(?EVENT_HANDLER, ?MODULE, {add, JID}). 

del_subscription(JID) when is_binary(JID) ->
    gen_event:call(?EVENT_HANDLER, ?MODULE, {del, JID}). 

is_subscription(JID) when is_binary(JID) ->
    gen_event:call(?EVENT_HANDLER, ?MODULE, {is_sub, JID}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, JID} = application:get_env(ecomponent, jid),
    lager:info("XMMP JID: ~s~n", [JID]),
    {ok, #state{
        jid=JID, 
        subscriptions=case application:get_env(esyslog, xmpp) of 
        {ok, XmppConf} ->
            ordsets:from_list(proplists:get_value(subscriptions, XmppConf, []));
        _ ->
            []
    end}}.

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
handle_event({syslog, _Severity, _Facility, DateTime, Machine, Message}, #state{jid=JID}=State) ->
    From = exmpp_jid:parse(JID),
    Body = io_lib:format("~s ~s ~s", [DateTime, Machine, Message]),
    lists:foreach(fun(Subs) ->
        To = exmpp_jid:parse(Subs), 
        Chat = exmpp_stanza:set_jids(exmpp_message:chat(Body), From, To),
        ecomponent:send_message(Chat) 
    end, State#state.subscriptions),
    {ok, State};

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
handle_call({add, JID}, #state{subscriptions=Subs}=State) ->
    {ok, ok, State#state{subscriptions=ordsets:add_element(JID, Subs)}};
handle_call({del, JID}, #state{subscriptions=Subs}=State) ->
    {ok, ok, State#state{subscriptions=ordsets:del_element(JID, Subs)}};
handle_call(get, #state{subscriptions=Subs}=State) ->
    {ok, Subs, State};
handle_call({is_sub, JID}, #state{subscriptions=Subs}=State) ->
    {ok, ordsets:is_element(JID, Subs), State};
handle_call(_Request, State) ->
    {ok, ok, State}.

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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% ecomponent functions
%%--------------------------------------------------------------------

process_iq(#params{ns=NS, iq=IQ}) ->
    IQerror = exmpp_iq:error(IQ, 'bad-request'),
    ecomponent:send(IQerror, NS, ?MODULE).

process_message(#message{from=From, xmlel=XmlEl}=Message) ->
    lager:debug("message = ~p~n", [Message]),
    JID = exmpp_jid:make(From),
    case trim(exmpp_message:get_body(XmlEl)) of 
        <<"/help">> ->
            send_help_message(JID);
        <<"/logout">> ->
            del_subscription(exmpp_jid:to_binary(JID)),
            send_message(JID, <<"unsuscribed">>);
        <<"/login">> ->
            add_subscription(exmpp_jid:to_binary(JID)),
            send_message(JID, <<"suscribed">>);
        Unknown ->
            lager:debug("unknown command: ~p~n", [Unknown]),
            send_message(JID, <<"Uknonwn command">>),
            send_help_message(JID)
    end,
    ok.

process_presence(_Presence) ->
    ok.

send_help_message(JID) ->
    send_message(JID, <<"
        /help   to see the help
        /login  to suscribe
        /logout to unsuscribe
    ">>).

send_message(JID, Body) ->
    Stanza = exmpp_message:chat(Body),
    Chat = exmpp_stanza:set_recipient(Stanza, JID),
    ecomponent:send_message(Chat).

trim(Bin= <<C,BinTail/binary>>) ->
    case is_whitespace(C) of
        true -> trim(BinTail);
        false -> trim_tail(Bin)
    end.

trim_tail(<<C>>) ->
    case is_whitespace(C) of
        true -> false;
        false -> <<C>>
    end;
trim_tail(<<C,Bin/binary>>) ->
    case trim_tail(Bin) of
        false -> trim_tail(<<C>>);
        BinTail -> <<C,BinTail/binary>>
    end.

is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_) -> false.
