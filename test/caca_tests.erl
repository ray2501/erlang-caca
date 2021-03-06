-module(caca_tests).
-include_lib("eunit/include/eunit.hrl").
-include("caca.hrl").

create_canvas_fail_1_test() ->
    ?_assertException(error, function_clause, caca:create_canvas(-1, 0)).

create_canvas_fail_2_test() ->
    ?_assertException(error, function_clause, caca:create_canvas(0, -1)).

get_font_list_test() ->
     ?assertNotEqual([], caca:get_font_list()).

get_display_driver_list_test() ->
     ?assertNotEqual(#{}, caca:get_display_driver_list()).

get_export_list_test() ->
     ?assertNotEqual(#{}, caca:get_export_list()).

get_import_list_test() ->
     ?assertNotEqual(#{}, caca:get_import_list()).

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

set_attr_test() ->
    {ok, R} = caca:create_canvas(0, 0),
    {ok, D} = caca:create_display(R),
    caca:set_canvas_size(R, 80, 40),
    ?assertEqual(ok, caca:set_attr(R, ?UNDERLINE)),
    caca:put_str(R, 11, 6, "Hello Erlang"),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R).

blit_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    {ok, D} = caca:create_display(R),
    caca:set_color_ansi(R, ?BLUE, ?YELLOW),
    caca:put_str(R, 1, 1, "Hello Erlang"),
    caca:draw_line(R, 10, 10, 20, 20, 16#2B),
    {ok, Src} = caca:create_canvas(60, 30),
    caca:draw_polyline(Src, [10, 20, 10, 20], [10, 10, 20, 20], 16#2B),
    ?assertEqual(ok, caca:blit(R, 20, 10, Src)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R),
    caca:free_canvas(Src).

blit_2_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    {ok, D} = caca:create_display(R),
    caca:draw_line(R, 10, 10, 20, 20, 16#2B),
    {ok, Src} = caca:create_canvas(60, 30),
    caca:set_color_ansi(Src, ?LIGHTCYAN, ?CYAN),
    caca:draw_polyline(Src, [1, 40, 20, 40], [40, 1, 20, 40], 16#2B),
    {ok, Mask} = caca:create_canvas(60, 30),
    caca:set_color_ansi(Mask, ?BLUE, ?YELLOW),
    caca:clear_canvas(Mask),
    caca:set_color_ansi(Mask, ?WHITE, ?WHITE),
    caca:draw_thin_box(Mask, 20, 20, 25, 25),
    ?assertEqual(ok, caca:blit(R, 20, 10, Src, Mask)),
    caca:refresh_display(D),
    caca:free_display(D),
    caca:free_canvas(R),
    caca:free_canvas(Src),
    caca:free_canvas(Mask).

set_canvas_boundaries_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    caca:set_color_ansi(R, ?BLUE, ?YELLOW),
    caca:put_str(R, 20, 10, "Hello Erlang"),
    ?assertEqual(ok, caca:set_canvas_boundaries(R, 10, 5, 60, 30)),
    caca:free_canvas(R).

invert_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:invert(R)),
    caca:free_canvas(R).

flip_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:flip(R)),
    caca:free_canvas(R).

flop_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:flop(R)),
    caca:free_canvas(R).

rotate_180_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:rotate_180(R)),
    caca:free_canvas(R).

rotate_left_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:rotate_left(R)),
    caca:free_canvas(R).

rotate_right_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:rotate_right(R)),
    caca:free_canvas(R).

stretch_left_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:stretch_left(R)),
    caca:free_canvas(R).

stretch_right_canvas_test() ->
    {ok, R} = caca:create_canvas(80, 40),
    ?assertEqual(ok, caca:stretch_right(R)),
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
    caca:set_color_ansi(R, ?RED, ?LIGHTGREEN),
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

set_display_title_test() ->
    {ok, D} = caca:create_display(),
    ?assertEqual(ok, caca:set_display_title(D, "Hello Erlang")),
    caca:refresh_display(D),
    caca:free_display(D).

set_display_cursor_test() ->
    {ok, D} = caca:create_display(),
    ?assertEqual(ok, caca:set_cursor(D, 0)),
    caca:refresh_display(D),
    ?assertEqual(ok, caca:set_cursor(D, 1)),
    caca:refresh_display(D),
    caca:free_display(D).

set_display_mouse_test() ->
    {ok, D} = caca:create_display(),
    ?assertEqual(ok, caca:set_mouse(D, 0)),
    caca:refresh_display(D),
    ?assertEqual(ok, caca:set_mouse(D, 1)),
    caca:refresh_display(D),
    caca:free_display(D).

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

create_event_test() ->
    {ok, E} = caca:create_event(),
    ?assertEqual(ok, caca:free_event(E)).

create_dither_test() ->
    {ok, Dither} = caca:create_dither(32, 80, 40, 320, 
				 16#ff00, 16#ff0000, 16#ff000000, 16#ff),
    ?assertEqual(ok, caca:free_dither(Dither)).

set_dither_brightness_test() ->
    {ok, Dither} = caca:create_dither(32, 80, 40, 320, 
				 16#ff00, 16#ff0000, 16#ff000000, 16#ff),
    caca:set_dither_brightness(Dither, 5.0),
    ?assertEqual(5.0, caca:get_dither_brightness(Dither)),
    caca:free_dither(Dither).

set_dither_gamma_test() ->
    {ok, Dither} = caca:create_dither(32, 80, 40, 320,
                                 16#ff00, 16#ff0000, 16#ff000000, 16#ff),
    caca:set_dither_gamma(Dither, 15.0),
    ?assertEqual(15.0, caca:get_dither_gamma(Dither)),
    caca:free_dither(Dither).

set_dither_contrast_test() ->
    {ok, Dither} = caca:create_dither(32, 80, 40, 320,
                                 16#ff00, 16#ff0000, 16#ff000000, 16#ff),
    caca:set_dither_contrast(Dither, 15.0),
    ?assertEqual(15.0, caca:get_dither_contrast(Dither)),
    caca:free_dither(Dither).

dither_bitmap_test() ->
    {ok, C} = caca:create_canvas(8, 2),
    {ok, F} = caca:load_font(lists:nth(1, caca:get_font_list())),
    W = caca:get_canvas_width(C) * caca:get_font_width(F),
    H = caca:get_canvas_height(C) * caca:get_font_height(F),
    Size = 4 * W * H,
    Bin = binary:copy(list_to_binary([16#0]), Size),
    {ok, Dither} = caca:create_dither(32, 80, 40, 320,
                                 16#ff00, 16#ff0000, 16#ff000000, 16#ff),
    ?assertEqual(ok, caca:dither_bitmap(C, 0, 0, W, H, Dither, Bin)),
    caca:free_font(F),
    caca:free_canvas(C).

