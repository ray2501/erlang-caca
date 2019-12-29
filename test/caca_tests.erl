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

put_str_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    caca:set_canvas_size(R, 80, 40),
    ?assertEqual(ok, caca:put_str(R, 15, 10, "Hello Erlang!")),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

put_attr_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    caca:set_canvas_size(R, 80, 40),
    ?assertEqual(ok, caca:put_attr(R, 15, 10, 16#55FF44)),
    ?assertEqual(16#55FF44, caca:get_attr(R, 15, 10)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

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

draw_circle_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_circle(R, 20, 10, 3, 16#2B)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_ellipse_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_ellipse(R, 25, 15, 3, 4, 16#2B)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_thin_ellipse_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_thin_ellipse(R, 25, 15, 3, 4)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

fill_ellipse_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:fill_ellipse(R, 30, 15, 4, 5, 16#2B)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_box_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_box(R, 30, 15, 4, 5, 16#2E)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_thin_box_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_thin_box(R, 20, 15, 4, 5)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_cp437_box_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_cp437_box(R, 20, 15, 4, 5)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

fill_box_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:fill_box(R, 30, 15, 4, 5, 16#2E)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_triangle_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_triangle(R, 10, 15, 15, 20, 20, 10, 16#2E)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

draw_thin_triangle_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:draw_thin_triangle(R, 10, 15, 15, 20, 20, 10)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

fill_triangle_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:fill_triangle(R, 10, 15, 15, 20, 20, 10, 16#2E)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

get_frame_count_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(1, caca:get_frame_count(R)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

set_frame_name_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:set_frame_name(R, "current")),
    ?assertEqual("current", caca:get_frame_name(R)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

create_frame_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:create_frame(R, 0)),
    ?assertEqual(ok, caca:set_frame(R, 1)),
    ?assertEqual(2, caca:get_frame_count(R)),
    ?assertEqual(ok, caca:free_frame(R, 1)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

create_display_fail_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    ?_assertException(error, function_clause, caca:create_display(1)),
    ?assertEqual(ok, caca:free_canvas(R)).

create_display_0_test() ->
    {ok, D} = caca:create_display(),
    {ok, R} =  caca:get_canvas(D),
    ?assertEqual(ok, caca:free_display(D)).

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

set_display_driver_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:set_display_driver(D, "ncurses")),
    ?assertEqual("ncurses", caca:get_display_driver(D)),
    caca:free_display(D),
    caca:free_canvas(R).

set_display_time_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    ?assertEqual(ok, caca:set_display_time(D, 1000)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

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

