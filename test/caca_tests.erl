-module(caca_tests).
-include_lib("eunit/include/eunit.hrl").

create_canvas_fail_1_test() ->
    ?_assertException(error, function_clause, caca:create_canvas(-1, 0)).

create_canvas_fail_2_test() ->
    ?_assertException(error, function_clause, caca:create_canvas(0, -1)).

create_canvas_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    ?assertEqual(0, caca:get_canvas_width(R)),
    ?assertEqual(0, caca:get_canvas_height(R)),
    ?assertEqual(ok, caca:free_canvas(R)).

create_display_fail_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    ?_assertException(error, function_clause, caca:create_display(1)),
    ?assertEqual(ok, caca:free_canvas(R)).

create_display_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:free_display(D)),
    ?assertEqual(ok, caca:free_canvas(R)).

create_display_with_driver_fail_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    ?_assertException(error, function_clause, caca:create_display_with_driver(R, 1)),
    ?assertEqual(ok, caca:free_canvas(R)).

create_display_with_driver_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display_with_driver(R, "ncurses"),
    ?assertEqual(ok, caca:free_display(D)),
    ?assertEqual(ok, caca:free_canvas(R)).

