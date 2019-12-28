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

set_canvas_size_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    ?assertEqual(ok, caca:set_canvas_size(R, 80, 40)),
    ?assertEqual(80, caca:get_canvas_width(R)),
    ?assertEqual(40, caca:get_canvas_height(R)),
    ?assertEqual(ok, caca:free_canvas(R)).

gotoxy_canvas_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    caca:set_canvas_size(R, 80, 40),
    ?assertEqual(ok, caca:gotoxy(R, 10 , 10)),
    ?assertEqual(10, caca:wherex(R)),
    ?assertEqual(10, caca:wherey(R)),
    ?assertEqual(ok, caca:free_canvas(R)).

put_char_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    caca:set_canvas_size(R, 80, 40),
    ?assertEqual(1, caca:put_char(R, 10 , 10, 16#41)),
    ?assertEqual(16#41, caca:get_char(R, 10 , 10)),
    ?assertEqual(ok, caca:free_canvas(R)).

draw_line_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_line(R, 10, 10, 20, 20, 16#2B)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_thin_line_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_thin_line(R, 10, 10, 20, 20)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_polyline_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_polyline(R, [10, 20, 10, 20], [10, 10, 20, 20], 16#2B)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_thin_polyline_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_thin_polyline(R, [10, 20, 10, 20], [10, 10, 20, 20])),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

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

free_display_wrong_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display_with_driver(R, "ncurses"),
    ?_assertException(error, function_clause, caca:free_display(R)),
    ?assertEqual(ok, caca:free_display(D)),
    ?assertEqual(ok, caca:free_canvas(R)).

refresh_display_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display_with_driver(R, "ncurses"),
    ?assertEqual(1, caca:put_char(R, 10 , 10, 16#41)),
    ?assertEqual(ok, caca:refresh_display(D)),
    ?assertEqual(ok, caca:free_display(D)),
    ?assertEqual(ok, caca:free_canvas(R)).

clear_canvas_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display_with_driver(R, "ncurses"),
    ?assertEqual(ok, caca:clear_canvas(R)),
    ?assertEqual(ok, caca:refresh_display(D)),
    ?assertEqual(ok, caca:free_display(D)),
    ?assertEqual(ok, caca:free_canvas(R)).

set_handle_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display_with_driver(R, "ncurses"),
    ?assertEqual(ok, caca:set_canvas_handle(R, 10, 10)),
    ?assertEqual(10, caca:get_canvas_handle_x(R)),
    ?assertEqual(10, caca:get_canvas_handle_y(R)),
    ?assertEqual(ok, caca:refresh_display(D)),
    ?assertEqual(ok, caca:free_display(D)),
    ?assertEqual(ok, caca:free_canvas(R)).

