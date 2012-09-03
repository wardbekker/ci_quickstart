-module(ci_quickstart_math).

-export([addition/2]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

addition(X, Y) ->
    X + Y.

-ifdef(TEST).

%% EUNIT TEST

simple_test() ->
    ?assertEqual(4, addition(2,2)),
    ?assertNotEqual(3, addition(1,1)).


%% PROPER tests

proper_test() ->
    ?assertEqual(
       [],
       proper:module(?MODULE, [long_result])).

prop_sum() ->
    ?FORALL({X, Y}, {int(), int()}, addition(X,Y) - Y == X).

-endif.
