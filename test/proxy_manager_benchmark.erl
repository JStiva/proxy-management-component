-module(proxy_manager_benchmark).
-export([bench/2]).

bench(ProxyNumber, Times) ->
    io:format("----------------------------------~n"),
    io:format("Start benchmarking ~n"),
    io:format("Test with ~p proxies, ~p times ~n",[ProxyNumber, Times]),
    io:format("~n----------------------------------~n"),
    test_proxy_manager(ProxyNumber, Times).

median(Unsorted) ->
    Sorted = lists:sort(Unsorted),
    Length = length(Sorted),
    Mid = Length div 2,
    Rem = Length rem 2,
    (lists:nth(Mid + Rem, Sorted) + lists:nth(Mid + 1, Sorted)) / 2.

test_proxy_manager(NumberOfProxies, Times) ->
    Proxies = lists:seq(1, NumberOfProxies),
    proxy_manager:start(),

    io:format("Test set proxy ~n"),
    {T1, S1} = test_set_proxy(Proxies, Times, set_proxies, [Proxies]),
    Avg1 = S1 / Times,
    Min1 = lists:min(T1),
    Med1 = median(T1),
    io:format("Min: ~p, Avg: ~p, Med: ~p", [Min1, Avg1, Med1]),
    io:format("~n----------------------------------~n"),

    io:format("Test list proxies ~n"),
    {T2, S2} = test_list_proxies(Proxies, Times, list_proxies, []),
    Avg2 = S2 / Times,
    Min2 = lists:min(T2),
    Med2 = median(T2),
    io:format("Min: ~p, Avg: ~p, Med: ~p", [Min2, Avg2, Med2]),
    io:format("~n----------------------------------~n"),

    io:format("Test get proxy ~n"),
    {T3, S3} = test_get_proxy(Proxies, Times, get_proxy, [netloc1]),
    Avg3 = S3 / Times,
    Min3 = lists:min(T3),
    Med3 = median(T3),
    io:format("Min: ~p, Avg: ~p, Med: ~p", [Min3, Avg3, Med3]),
    io:format("~n----------------------------------~n"),

    io:format("Test ban proxy ~n"),
    {T4, S4} = test_ban_proxy(Proxies, Times, ban_proxy, [5, netloc1]),
    Avg4 = S4 / Times,
    Min4 = lists:min(T4),
    Med4 = median(T4),
    io:format("Min: ~p, Avg: ~p, Med: ~p", [Min4, Avg4, Med4]),
    io:format("~n----------------------------------~n"),

    io:format("Test unban proxy ~n"),
    {T5, S5} = test_unban_proxy(Proxies, Times,  unban_proxy, [5, netloc1]),
    Avg5 = S5 / Times,
    Min5 = lists:min(T5),
    Med5 = median(T5),
    io:format("Min: ~p, Avg: ~p, Med: ~p", [Min5, Avg5, Med5]),
    io:format("~n----------------------------------~n"),

    io:format("Test get all banned proxies ~n"),
    {T6, S6} = test_get_all_banned_proxies(Proxies, Times, banned_proxies, []),
    Avg6 = S6 / Times,
    Min6 = lists:min(T6),
    Med6 = median(T6),
    io:format("Min: ~p, Avg: ~p, Med: ~p", [Min6, Avg6, Med6]),
    io:format("~n----------------------------------~n"),

    io:format("Test get banned proxies by netloc ~n"),
    {T7, S7} = test_get_all_banned_proxies_by_netloc(Proxies, Times, banned_proxies, [netloc1]),
    Avg7 = S7 / Times,
    Min7 = lists:min(T7),
    Med7 = median(T7),
    io:format("Min: ~p, Avg: ~p, Med: ~p", [Min7, Avg7, Med7]),
    io:format("~n----------------------------------~n").


test_module(_Proxies, 0, Result, Sum, _Fun, _Args) ->
    proxy_manager:stop(),
    {Result, Sum};
test_module(Proxies, Times, Result, Sum, Fun, Args) ->
    {Time, _Value} = timer:tc(proxy_manager, Fun, Args),
    NewSum = Sum + Time,
    test_module(Proxies, Times-1, [Time | Result], NewSum, Fun, Args).


test_set_proxy(Proxies, Times, Fun, Args) ->
    proxy_manager:start(),
    test_module(Proxies, Times, [], 0, Fun, Args).


test_list_proxies(Proxies, Times, Fun, Args) -> 
    proxy_manager:start(),
    proxy_manager:set_proxies(Proxies),
    test_module(Proxies, Times, [], 0, Fun, Args).


test_get_proxy(Proxies, Times, Fun, Args) -> 
    proxy_manager:start(),
    proxy_manager:set_proxies(Proxies),
    test_module(Proxies, Times, [], 0, Fun, Args).


test_ban_proxy(Proxies, Times, Fun, Args) ->
    proxy_manager:start(),
    proxy_manager:set_proxies(Proxies), 
    test_module(Proxies, Times, [], 0, Fun, Args).


test_unban_proxy(Proxies, Times, Fun, Args) ->
    proxy_manager:start(),
    proxy_manager:set_proxies(Proxies),
    test_module(Proxies, Times, [], 0, Fun, Args).


test_get_all_banned_proxies(Proxies, Times, Fun, Args) -> 
    proxy_manager:start(),
    proxy_manager:set_proxies(Proxies),
    proxy_manager:ban_proxy(5, netloc1),
    proxy_manager:ban_proxy(4, netloc1),
    proxy_manager:ban_proxy(3, netloc1),
    proxy_manager:ban_proxy(5, netloc2),
    proxy_manager:ban_proxy(5, netloc3),
    proxy_manager:ban_proxy(7, netloc3),
    proxy_manager:ban_proxy(9, netloc3),
    proxy_manager:ban_proxy(8, netloc4),
    proxy_manager:ban_proxy(2, netloc5),
    test_module(Proxies, Times, [], 0, Fun, Args).


test_get_all_banned_proxies_by_netloc(Proxies, Times, Fun, Args) ->
    proxy_manager:start(),
    proxy_manager:set_proxies(Proxies),
    proxy_manager:ban_proxy(5, netloc1),
    proxy_manager:ban_proxy(4, netloc1),
    proxy_manager:ban_proxy(3, netloc1),
    test_module(Proxies, Times, [], 0, Fun, Args).
