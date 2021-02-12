-module(proxy_manager_v2).
-behaviour(gen_server).

-export([start/0]).
-export([stop/0]).
-export([set_proxies/1]).
-export([list_proxies/0]).
-export([get_proxy/1]).
-export([ban_proxy/2]).
-export([unban_proxy/2]).
-export([banned_proxies/0]).
-export([banned_proxies/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]
).

-type proxy()  :: term().
-type netloc() :: term().

-record(state, {
        proxies=[],
        proxiesCount,
        netloc,
        client_proc
    }
).


start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).    


stop()-> gen_server:call(?MODULE, stop).


init([]) ->
    {ok, #state{proxies=[], proxiesCount=0, netloc=#{}}}.


-spec set_proxies([proxy()]) -> ok.
%% @doc
%% Initialize subsystem state with given proxies
%% @end

set_proxies(Proxies) ->
    gen_server:call(?MODULE, {set_proxies, Proxies}).


-spec list_proxies() -> [proxy()].
%% @doc
%% List proxies
%% @end

list_proxies() ->
    gen_server:call(?MODULE, {list_proxies}).


-spec get_proxy(netloc()) -> {ok, proxy()} | {error, no_proxies}.
%% @doc
%% Return clean proxy for given netloc or error, if no proxies available
%% @end

get_proxy(Netloc) ->
    gen_server:call(?MODULE, {get_proxy, Netloc}).


-spec ban_proxy(proxy(), netloc()) -> ok.
%% @doc
%% Ban proxy for netloc
%% @end

ban_proxy(Proxy, Netloc) ->
    gen_server:call(?MODULE, {ban_proxy, Proxy, Netloc}).


-spec unban_proxy(proxy(), netloc()) -> ok.
%% @doc
%% Unban proxy for netloc
%% @end

unban_proxy(Proxy, Netloc) ->
    gen_server:call(?MODULE, {unban_proxy, Proxy, Netloc}).


-spec banned_proxies() -> [{netloc(), [proxy()]}].
%% @doc
%% List banned proxies per netloc
%% @end
banned_proxies() ->
    gen_server:call(?MODULE, {banned_proxies}).


-spec banned_proxies(netloc()) -> [proxy()].
%% @doc
%% List banned proxies for netloc
%% @end
banned_proxies(Netloc) ->
    gen_server:call(?MODULE, {banned_proxies, Netloc}).

%% helper functions
get_random_proxy(ProxyList, ProxyLen) -> 
    rand:seed(exsp),
    lists:nth(rand:uniform(ProxyLen), ProxyList).

get_random_proxy_with_banned(_ProxyList, ProxyLen, _BannedProxis, BannedProxiesLen) when ProxyLen == BannedProxiesLen ->
    {error, no_proxies};
get_random_proxy_with_banned(ProxyList, ProxyLen, BannedProxis, BannedProxiesLen) ->
    Proxy = get_random_proxy(ProxyList, ProxyLen),
    case sets:is_element(Proxy, BannedProxis) of 
        true -> get_random_proxy_with_banned(ProxyList, ProxyLen, BannedProxis, BannedProxiesLen);
        _-> {ok, Proxy}
    end.


get_proxy_helper(State, Netloc) -> 
    ProxyList = State#state.proxies,
    ProxyLen = State#state.proxiesCount,

    Proxy = case maps:find(Netloc, State#state.netloc) of
                % we have or had banned proxies for netloc
                {ok, BannedProxis} -> 
                    case sets:size(BannedProxis) of 
                        % no banned proxies, all unbanned 
                        0 -> get_random_proxy(ProxyList, ProxyLen);
                        Len -> get_random_proxy_with_banned(ProxyList, ProxyLen, BannedProxis, Len)
                    end;
                % no banned proxied for netloc yet
                error -> get_random_proxy(ProxyList, ProxyLen)
             end,
    ?MODULE ! {selected_proxy, Proxy}.

handle_call({set_proxies, Proxies}, _From, State)-> 
    ProxList = lists:append(State#state.proxies,Proxies),

    % eliminate duplicate proxies using set
    ProxiesSet = sets:from_list(ProxList),
    ProxiesList = sets:to_list(ProxiesSet),

    ProxiesLength = length(ProxiesList),
    NewState = State#state{proxies=ProxiesList, proxiesCount=ProxiesLength},
    {reply, ok, NewState};


handle_call({list_proxies}, _From, State)-> 
    {reply, State#state.proxies, State};


handle_call({get_proxy, _Netloc}, _From, #state{proxiesCount = 0} = State)-> 
    {reply, {error, no_proxies}, State};

handle_call({get_proxy, Netloc}, From, State)-> 
    gen_server:reply(From, ok),
    get_proxy_helper(State, Netloc),
    NewState = State#state{client_proc = From},
    {noreply, NewState};


handle_call({ban_proxy, Proxy, Netloc}, _From, State)-> 
    NetlocMap = State#state.netloc,

    Reply = case lists:member(Proxy, State#state.proxies) of
                % given proxy not in proxy pool
                false -> {reply, ok, State};
                _-> case maps:find(Netloc, NetlocMap) of
                        {ok, BannedProxies} -> case sets:is_element(Proxy, BannedProxies) of 
                                                    % proxy already exists in banned set
                                                    true -> {reply, ok, State};
                                                    _-> UpdatedBannedProxies = sets:add_element(Proxy, BannedProxies),
                                                        UpdatedNetlocMap = maps:update(Netloc, UpdatedBannedProxies, NetlocMap),
                                                        {reply, ok, State#state{netloc=UpdatedNetlocMap}}
                                                end;
                        % adding a proxy to banned set for the first time
                        _-> BannedProxies = sets:new(),
                            UpdatedBannedProxies = sets:add_element(Proxy, BannedProxies),
                            UpdatedNetlocMap = maps:put(Netloc, UpdatedBannedProxies, NetlocMap),
                            {reply, ok, State#state{netloc=UpdatedNetlocMap}}
                    end 
            end,
    Reply;


handle_call({unban_proxy, Proxy, Netloc}, _From, State)-> 
    NetlocMap = State#state.netloc,

    Reply = case lists:member(Proxy, State#state.proxies) of
                false -> {reply, ok, State};
                _-> case maps:find(Netloc, NetlocMap) of
                        {ok, BannedProxies} -> UpdatedBannedProxies = sets:del_element(Proxy, BannedProxies),
                                                UpdatedNetlocMap = maps:update(Netloc, UpdatedBannedProxies, NetlocMap),
                                                {reply, ok, State#state{netloc=UpdatedNetlocMap}};
                        _-> {reply, ok, State}
                    end
            end,
    Reply;


handle_call({banned_proxies}, _From, State)-> 
    NetlocMap = State#state.netloc,

    Reply = case maps:size(NetlocMap) of 
                0 -> {reply, [], State};
                _->  Fun = fun(K,V,AccIn) -> lists:append(AccIn, [{K, sets:to_list(V)}]) end,
                    {reply, maps:fold(Fun,[],NetlocMap), State}
            end,
    Reply;


handle_call({banned_proxies, Netloc}, _From, State)-> 
    NetlocMap = State#state.netloc,

    Reply = case maps:find(Netloc, NetlocMap) of
                {ok, BannedProxies} -> {reply, sets:to_list(BannedProxies), State};
                _-> {reply, [], State}
            end,
    Reply;


handle_call(stop, _From, State) -> 
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({selected_proxy, Proxy}, State = #state{client_proc = From}) ->
    gen_server:reply(From, Proxy),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(normal, _State)-> ok.


code_change(_OldVsn, State, _Extra)->
    {ok, State}.