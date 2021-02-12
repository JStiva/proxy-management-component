-module(proxy_manager_tests).

-include_lib("eunit/include/eunit.hrl").

proxy_manager_test_() ->
    {foreach,
     fun() ->
        {ok, Pid} = proxy_manager:start(),
        Pid
     end,
     fun(Pid) ->
        %erlang:exit(Pid, kill),
        %erlang:monitor(process, Pid),
        %receive {'DOWN', _, _, _, _} -> ok end
        proxy_manager:stop()
     end,
     [
        {"set proxies", fun set_proxies/0},
        {"list proxies", fun list_proxies/0},
        {"get proxy", fun get_proxy/0},
        {"ban proxy", fun ban_proxy/0},
        {"unban proxy", fun unban_proxy/0}
    ]}.



set_proxies() ->
    Proxies = [proxy0, proxy1, proxy2],
    ?assertEqual(ok, proxy_manager:set_proxies(Proxies)),
    ?assertEqual(Proxies, lists:sort(proxy_manager:list_proxies())).


list_proxies() ->
    %% empty state means empty list of proxies
    ?assertEqual([], proxy_manager:list_proxies()).


get_proxy() ->
    ?assertEqual({error, no_proxies}, proxy_manager:get_proxy(some_netloc)),
    Proxies = [proxy0, proxy1],
    proxy_manager:set_proxies(Proxies),
    {ok, Proxy} = proxy_manager:get_proxy(some_netloc),
    ?assert(lists:member(Proxy, Proxies)).


ban_proxy() ->
    %% banning non-existing proxy is allowed, but it should not appear in ban lists
    ?assertEqual(ok, proxy_manager:ban_proxy(proxy0, some_netloc)),
    ?assertEqual([], proxy_manager:banned_proxies()),

    proxy_manager:set_proxies([proxy0, proxy1]),
    ?assertEqual(ok, proxy_manager:ban_proxy(proxy0, some_netloc)),
    ?assertEqual({ok, proxy1}, proxy_manager:get_proxy(some_netloc)),
    ?assertEqual(ok, proxy_manager:ban_proxy(proxy1, some_netloc)),
    ?assertEqual({error, no_proxies}, proxy_manager:get_proxy(some_netloc)),
    ?assertEqual([proxy0, proxy1], lists:sort(proxy_manager:banned_proxies(some_netloc))).


unban_proxy() ->
    %% unbanning non-existing proxy is allowed, but it should not appear in proxy listings
    ?assertEqual(ok, proxy_manager:unban_proxy(proxy0, some_netloc)),
    ?assertEqual([], proxy_manager:list_proxies()),

    proxy_manager:set_proxies([proxy0, proxy1]),
    proxy_manager:ban_proxy(proxy0, some_netloc),
    proxy_manager:ban_proxy(proxy1, some_netloc),
    proxy_manager:unban_proxy(proxy0, some_netloc),
    ?assertEqual({ok, proxy0}, proxy_manager:get_proxy(some_netloc)).
