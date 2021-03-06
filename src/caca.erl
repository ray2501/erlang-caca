%% ----------------------------------
%% Erlang bindings for libcaca
%% ----------------------------------

-module(caca).

%% API exports
-export([caca_version/0, get_display_driver_list/0, get_export_list/0, get_import_list/0]).
-export([get_font_list/0, rand/2]).
-export([create_canvas/2, free_canvas/1]).
-export([set_canvas_size/3, get_canvas_width/1, get_canvas_height/1]).
-export([gotoxy/3, wherex/1, wherey/1, put_char/4, get_char/3, put_str/4]).
-export([get_attr/3, set_attr/2, unset_attr/2, toggle_attr/2, put_attr/4]).
-export([set_color_ansi/3, set_color_argb/3, clear_canvas/1]).
-export([set_canvas_handle/3, get_canvas_handle_x/1, get_canvas_handle_y/1]).
-export([blit/4, blit/5, set_canvas_boundaries/5]).
-export([invert/1, flip/1, flop/1, rotate_180/1]).
-export([rotate_left/1, rotate_right/1, stretch_left/1, stretch_right/1]).
-export([draw_line/6, draw_thin_line/5, draw_polyline/4, draw_thin_polyline/3]).
-export([draw_circle/5, draw_ellipse/6, draw_thin_ellipse/5, fill_ellipse/6]).
-export([draw_box/6, draw_thin_box/5, draw_cp437_box/5, fill_box/6]).
-export([draw_triangle/8, draw_thin_triangle/7, fill_triangle/8]).
-export([get_frame_count/1, set_frame/2, get_frame_name/1, set_frame_name/2]).
-export([create_frame/2, free_frame/2]).
-export([import_canvas_from_memory/3, import_canvas_from_file/3, import_area_from_memory/5]).
-export([import_area_from_file/5, export_canvas_to_memory/2, export_area_to_memory/6]).
-export([create_display/0, create_display/1, create_display_with_driver/2, free_display/1]).
-export([get_display_driver/1, set_display_driver/2, get_canvas/1]).
-export([refresh_display/1, get_display_time/1, set_display_time/2]).
-export([get_display_width/1, get_display_height/1, set_display_title/2]).
-export([set_mouse/2, set_cursor/2]).
-export([create_event/0, free_event/1, get_event/4]).
-export([get_event_type/1, get_event_key_ch/1, get_event_key_utf32/1, get_event_key_utf8/1]).
-export([get_event_mouse_button/1, get_event_mouse_x/1, get_event_mouse_y/1]).
-export([get_event_resize_width/1, get_event_resize_height/1]).
-export([create_dither/8, free_dither/1, set_dither_brightness/2, get_dither_brightness/1]).
-export([set_dither_gamma/2, get_dither_gamma/1, set_dither_contrast/2, get_dither_contrast/1]).
-export([set_dither_antialias/2, get_dither_antialias/1, get_dither_antialias_list/1]).
-export([set_dither_color/2, get_dither_color/1, get_dither_color_list/1]).
-export([set_dither_charset/2, get_dither_charset/1, get_dither_charset_list/1]).
-export([set_dither_algorithm/2, get_dither_algorithm/1, get_dither_algorithm_list/1]).
-export([dither_bitmap/7]).
-export([load_font/1, get_font_width/1, get_font_height/1, free_font/1, render_canvas/6]).
-on_load(init/0).

-define(APPNAME, caca).
-define(LIBNAME, "caca_nif").

%%====================================================================
%% API functions
%%====================================================================

%% @doc Return the libcaca version
-spec caca_version() -> string() | {error, any()}.

caca_version() ->
    not_loaded(?LINE).

%% @doc Get available display drivers
-spec get_display_driver_list() -> Map::#{} | {error, any()}.

get_display_driver_list() ->
    not_loaded(?LINE).

%% @doc Get available export formats
-spec get_export_list() -> Map::#{} | {error, any()}.

get_export_list() ->
    not_loaded(?LINE).

%% @doc Get available import formats
-spec get_import_list() -> Map::#{} | {error, any()}.

get_import_list() ->
    not_loaded(?LINE).

%% @doc Get available fonts
-spec get_font_list() -> list(string) | {error, any()}.

get_font_list() ->
    not_loaded(?LINE).

%% @doc Generate a random integer within a range
-spec rand(Min::integer(), Max::integer()) -> integer() | {error, any()}.

rand(_, _) ->
    not_loaded(?LINE).

%% @doc Initialise a libcaca canvas
-spec create_canvas(Width::integer(), Height::integer()) -> {ok, reference()} | {error, any()}.

create_canvas(_, _) ->
    not_loaded(?LINE).

%% @doc Free a libcaca canvas
-spec free_canvas(Canvas::reference()) -> ok | {error, any()}.

free_canvas(_) ->
    not_loaded(?LINE).

%% @doc Resize a canvas
-spec set_canvas_size(Canvas::reference(), Width::integer(), Height::integer()) -> 
    ok | {error, any()}.

set_canvas_size(_,_,_) ->
    not_loaded(?LINE).

%% @doc Get the canvas width
-spec get_canvas_width(Canvas::reference()) -> integer() | {error, any()}.

get_canvas_width(_) ->
    not_loaded(?LINE).

%% @doc Get the canvas height
-spec get_canvas_height(Canvas::reference()) -> integer() | {error, any()}.

get_canvas_height(_) ->
    not_loaded(?LINE).

%% @doc Set cursor position
-spec gotoxy(Canvas::reference(), X::integer(), Y::integer()) -> 
    ok | {error, any()}.

gotoxy(_,_,_) ->
    not_loaded(?LINE).

%% @doc Get X cursor position
-spec wherex(Canvas::reference()) -> integer() | {error, any()}.

wherex(_) ->
    not_loaded(?LINE).

%% @doc Get Y cursor position
-spec wherey(Canvas::reference()) -> integer() | {error, any()}.

wherey(_) ->
    not_loaded(?LINE).

%% @doc Print an ASCII or Unicode character
-spec put_char(Canvas::reference(), X::integer(), Y::integer(), Char::char()) -> integer() | {error, any()}.

put_char(_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Get the Unicode character at the given coordinates
-spec get_char(Canvas::reference(), X::integer(), Y::integer()) -> char() | {error, any()}.

get_char(_,_,_) ->
    not_loaded(?LINE).

%% @doc Print a string
-spec put_str(Canvas::reference(), X::integer(), Y::integer(), S::string()) -> ok | {error, any()}.

put_str(_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Get the text attribute at the given coordinates
-spec get_attr(Canvas::reference(), X::integer(), Y::integer()) -> integer() | {error, any()}.

get_attr(_,_,_) ->
    not_loaded(?LINE).

%% @doc Set the default character attribute
-spec set_attr(Canvas::reference(), Attr::integer()) -> ok | {error, any()}.

set_attr(_,_) ->
    not_loaded(?LINE).
%% @doc Unset flags in the default character attribute
-spec unset_attr(Canvas::reference(), Attr::integer()) -> ok | {error, any()}.

unset_attr(_,_) ->
    not_loaded(?LINE).

%% @doc Toggle flags in the default character attribute
-spec toggle_attr(Canvas::reference(), Attr::integer()) -> ok | {error, any()}.

toggle_attr(_,_) ->
    not_loaded(?LINE).

%% @doc Set the character attribute at the given coordinates
-spec put_attr(Canvas::reference(), X::integer(), Y::integer(), Attr::integer()) -> ok | {error, any()}.

put_attr(_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Set the default colour pair for text (ANSI version)
-spec set_color_ansi(Canvas::reference(), Fg::integer(), Bg::integer()) -> ok | {error, any()}.

set_color_ansi(_,_,_) ->
    not_loaded(?LINE).

%% @doc Set the default colour pair for text (truecolor version)
-spec set_color_argb(Canvas::reference(), Fg::integer(), Bg::integer()) -> ok | {error, any()}.

set_color_argb(_,_,_) ->
    not_loaded(?LINE).

%% @doc Clear the canvas
-spec clear_canvas(Canvas::reference()) -> ok | {error, any()}.

clear_canvas(_) ->
    not_loaded(?LINE).

%% @doc Blit a canvas onto another one
-spec blit(Canvas::reference(), X1::integer(), Y1::integer(), 
           CanvasSrc::reference()) -> 
    ok | {error, any()}.

blit(_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Blit a canvas onto another one
-spec blit(Canvas::reference(), X1::integer(), Y1::integer(), 
           CanvasSrc::reference(), CanvasMask::reference()) -> 
    ok | {error, any()}.

blit(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Set a canvas' new boundaries
-spec set_canvas_boundaries(Canvas::reference(), X::integer(), Y::integer(), 
           W::integer(), H::integer()) -> 
    ok | {error, any()}.

set_canvas_boundaries(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Invert a canvas' colours
-spec invert(Canvas::reference()) -> 
    ok | {error, any()}.

invert(_) ->
    not_loaded(?LINE).

%% @doc Flip a canvas horizontally
-spec flip(Canvas::reference()) -> 
    ok | {error, any()}.

flip(_) ->
    not_loaded(?LINE).

%% @doc Flip a canvas vertically
-spec flop(Canvas::reference()) -> 
    ok | {error, any()}.

flop(_) ->
    not_loaded(?LINE).

%% @doc Rotate a canvas
-spec rotate_180(Canvas::reference()) -> 
    ok | {error, any()}.

rotate_180(_) ->
    not_loaded(?LINE).

%% @doc Rotate a canvas, 90 degrees counterclockwise
-spec rotate_left(Canvas::reference()) -> 
    ok | {error, any()}.

rotate_left(_) ->
    not_loaded(?LINE).

%% @doc Rotate a canvas, 90 degrees counterclockwise
-spec rotate_right(Canvas::reference()) -> 
    ok | {error, any()}.

rotate_right(_) ->
    not_loaded(?LINE).

%% @doc Rotate and stretch a canvas, 90 degrees counterclockwise
-spec stretch_left(Canvas::reference()) -> 
    ok | {error, any()}.

stretch_left(_) ->
    not_loaded(?LINE).

%% @doc Rotate and stretch a canvas, 90 degrees clockwise
-spec stretch_right(Canvas::reference()) -> 
    ok | {error, any()}.

stretch_right(_) ->
    not_loaded(?LINE).

%% @doc Draw a line on the canvas using the given character
-spec draw_line(Canvas::reference(), X1::integer(), Y1::integer(), 
		X2::integer(), Y2::integer(), Char::char()) -> 
    ok | {error, any()}.

draw_line(_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a thin line on the canvas, using ASCII art
-spec draw_thin_line(Canvas::reference(), X1::integer(), Y1::integer(), 
		X2::integer(), Y2::integer()) -> 
    ok | {error, any()}.

draw_thin_line(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a polyline
-spec draw_polyline(Canvas::reference(), 
                    X_List::list(integer), Y_List::list(integer), Char::char()) ->
    ok | {error, any()}.

draw_polyline(_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a thin line on the canvas, using ASCII art
-spec draw_thin_polyline(Canvas::reference(), 
                        X_List::list(integer), Y_List::list(integer)) -> 
    ok | {error, any()}.

draw_thin_polyline(_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a circle on the canvas using the given character
-spec draw_circle(Canvas::reference(), X::integer(), Y::integer(), 
                  Radius::integer(), Char::char()) -> 
    ok | {error, any()}.

draw_circle(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw an ellipse on the canvas using the given character
-spec draw_ellipse(Canvas::reference(), X::integer(), Y::integer(), 
                  A::integer(), B::integer(), Char::char()) -> 
    ok | {error, any()}.

draw_ellipse(_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a thin ellipse on the canvas
-spec draw_thin_ellipse(Canvas::reference(), X::integer(), Y::integer(), 
                  A::integer(), B::integer()) -> 
    ok | {error, any()}.

draw_thin_ellipse(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Fill an ellipse on the canvas using the given character
-spec fill_ellipse(Canvas::reference(), X::integer(), Y::integer(), 
                  A::integer(), B::integer(), Char::char()) -> 
    ok | {error, any()}.

fill_ellipse(_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a box on the canvas using the given character
-spec draw_box(Canvas::reference(), X::integer(), Y::integer(), 
                  W::integer(), H::integer(), Char::char()) -> 
    ok | {error, any()}.

draw_box(_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a thin box on the canvas
-spec draw_thin_box(Canvas::reference(), X::integer(), Y::integer(), 
                  W::integer(), H::integer()) -> 
    ok | {error, any()}.

draw_thin_box(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a box on the canvas using CP437 characters
-spec draw_cp437_box(Canvas::reference(), X::integer(), Y::integer(), 
                  W::integer(), H::integer()) -> 
    ok | {error, any()}.

draw_cp437_box(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Fill a box on the canvas using the given character
-spec fill_box(Canvas::reference(), X::integer(), Y::integer(), 
                  W::integer(), H::integer(), Char::char()) -> 
    ok | {error, any()}.

fill_box(_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a triangle on the canvas using the given character
-spec draw_triangle(Canvas::reference(), X1::integer(), Y1::integer(), 
                  X2::integer(), Y2::integer(), 
                  X3::integer(), Y3::integer(), 
                  Char::char()) -> 
    ok | {error, any()}.

draw_triangle(_,_,_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Draw a thin triangle on the canvas
-spec draw_thin_triangle(Canvas::reference(), X1::integer(), Y1::integer(), 
                  X2::integer(), Y2::integer(), 
                  X3::integer(), Y3::integer()) -> 
    ok | {error, any()}.

draw_thin_triangle(_,_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Fill a triangle on the canvas using the given character
-spec fill_triangle(Canvas::reference(), X1::integer(), Y1::integer(), 
                  X2::integer(), Y2::integer(), 
                  X3::integer(), Y3::integer(), 
                  Char::char()) -> 
    ok | {error, any()}.

fill_triangle(_,_,_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Set cursor handle
-spec set_canvas_handle(Canvas::reference(), X::integer(), Y::integer()) -> 
    ok | {error, any()}.

set_canvas_handle(_,_,_) ->
    not_loaded(?LINE).

%% @doc Get X handle position
-spec get_canvas_handle_x(Canvas::reference()) -> integer() | {error, any()}.

get_canvas_handle_x(_) ->
    not_loaded(?LINE).

%% @doc Get Y handle position
-spec get_canvas_handle_y(Canvas::reference()) -> integer() | {error, any()}.

get_canvas_handle_y(_) ->
    not_loaded(?LINE).

%% @doc Get the number of frames in a canvas
-spec get_frame_count(Canvas::reference()) -> integer() | {error, any()}.

get_frame_count(_) ->
    not_loaded(?LINE).

%% @doc Activate a given canvas frame
-spec set_frame(Canvas::reference(), Id::integer()) -> ok | {error, any()}.

set_frame(_,_) ->
    not_loaded(?LINE).

%% @doc Get the current frame's name
-spec get_frame_name(Canvas::reference()) -> string() | {error, any()}.

get_frame_name(_) ->
    not_loaded(?LINE).

%% @doc Set the current frame's name
-spec set_frame_name(Canvas::reference(), Name::string()) -> ok | {error, any()}.

set_frame_name(_,_) ->
    not_loaded(?LINE).

%% @doc Add a frame to a canvas
-spec create_frame(Canvas::reference(), Id::integer()) -> ok | {error, any()}.

create_frame(_,_) ->
    not_loaded(?LINE).

%% @doc Remove a frame from a canvas
-spec free_frame(Canvas::reference(), Id::integer()) -> ok | {error, any()}.

free_frame(_,_) ->
    not_loaded(?LINE).

%% @doc Import a memory buffer into a canvas
-spec import_canvas_from_memory(Canvas::reference(), Data::binary(), Format::string()) -> 
    ok | {error, any()}.

import_canvas_from_memory(_,_,_) ->
    not_loaded(?LINE).

%% @doc Import a file into a canvas
-spec import_canvas_from_file(Canvas::reference(), Filename::string(), Format::string()) -> 
    ok | {error, any()}.

import_canvas_from_file(_,_,_) ->
    not_loaded(?LINE).

%% @doc Import a memory buffer into a canvas area
-spec import_area_from_memory(Canvas::reference(), X::integer(), Y::integer(),
                              Data::binary(), Format::string()) ->
    ok | {error, any()}.

import_area_from_memory(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Import a file into a canvas area
-spec import_area_from_file(Canvas::reference(), X::integer(), Y::integer(),
                            Filename::string(), Format::string()) -> 
    ok | {error, any()}.

import_area_from_file(_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Export a canvas into a foreign format
-spec export_canvas_to_memory(Canvas::reference(), Format::string()) -> binary() | {error, any()}.

export_canvas_to_memory(_,_) ->
    not_loaded(?LINE).

%% @doc Export a canvas portion into a foreign format
-spec export_area_to_memory(Canvas::reference(), X::integer(), Y::integer(), 
                  W::integer(), H::integer(), Format::string()) -> 
    binary() | {error, any()}.

export_area_to_memory(_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Attach a caca graphical context to a caca canvas
-spec create_display() -> {ok, reference()} | {error, any()}.

create_display() ->
    not_loaded(?LINE).

%% @doc Attach a caca graphical context to a caca canvas
-spec create_display(Canvas::reference()) -> {ok, reference()} | {error, any()}.

create_display(_) ->
    not_loaded(?LINE).

%% @doc Attach a specific caca graphical context to a caca canvas
-spec create_display_with_driver(Canvas::reference(), Driver::string()) -> {ok, reference()} | {error, any()}.

create_display_with_driver(_, _) ->
    not_loaded(?LINE).

%% @doc Detach a caca graphical context from a caca backend context
-spec free_display(Display::reference()) -> ok | {error, any()}.

free_display(_) ->
    not_loaded(?LINE).

%% @doc Return a caca graphical context's current output driver
-spec get_display_driver(Display::reference()) -> string() | {error, any()}.

get_display_driver(_) ->
    not_loaded(?LINE).

%% @doc Set the output driver
-spec set_display_driver(Display::reference(), Driver::string()) -> string() | {error, any()}.

set_display_driver(_,_) ->
    not_loaded(?LINE).

%% @doc Get the canvas attached to a caca graphical context
-spec get_canvas(Display::reference()) -> {ok, reference()} | {error, any()}.

get_canvas(_) ->
    not_loaded(?LINE).

%% @doc Flush pending changes and redraw the screen
-spec refresh_display(Display::reference()) -> ok | {error, any()}.

refresh_display(_) ->
    not_loaded(?LINE).

%% @doc Get the display's average rendering time
-spec get_display_time(Display::reference()) -> integer() | {error, any()}.

get_display_time(_) ->
    not_loaded(?LINE).

%% @doc Set the refresh delay
-spec set_display_time(Display::reference(), Usec::integer()) -> ok | {error, any()}.

set_display_time(_,_) ->
    not_loaded(?LINE).

%% @doc Get the display width
-spec get_display_width(Display::reference()) -> integer() | {error, any()}.

get_display_width(_) ->
    not_loaded(?LINE).

%% @doc Get the display height
-spec get_display_height(Display::reference()) -> integer() | {error, any()}.

get_display_height(_) ->
    not_loaded(?LINE).

%% @doc Set the display title
-spec set_display_title(Display::reference(), Title::string()) -> ok | {error, any()}.

set_display_title(_,_) ->
    not_loaded(?LINE).

%% @doc Show or hide the mouse pointer
-spec set_mouse(Display::reference(), Usec::integer()) -> ok | {error, any()}.

set_mouse(_,_) ->
    not_loaded(?LINE).

%% @doc Show or hide the cursor
-spec set_cursor(Display::reference(), Usec::integer()) -> ok | {error, any()}.

set_cursor(_,_) ->
    not_loaded(?LINE).

%% @doc Malloc a caca_event Struct memory
-spec create_event() -> {ok, reference()} | {error, any()}.

create_event() ->
    not_loaded(?LINE).

%% @doc Malloc a caca_event Struct memory
-spec free_event(Event::reference()) -> ok | {error, any()}.

free_event(_) ->
    not_loaded(?LINE).

%% @doc Get the next mouse or keyboard input event
-spec get_event(Display::reference(), Mask::integer(), Event::reference(), Timeout::integer()) -> 
    ok | {error, any()}.

get_event(_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Return an event's type
-spec get_event_type(Event::reference()) -> integer() | {error, any()}.

get_event_type(_) ->
    not_loaded(?LINE).

%% @doc Return a key press or key release event's value
-spec get_event_key_ch(Event::reference()) -> integer() | {error, any()}.

get_event_key_ch(_) ->
    not_loaded(?LINE).

%% @doc Return a key press or key release event's Unicode value
-spec get_event_key_utf32(Event::reference()) -> integer() | {error, any()}.

get_event_key_utf32(_) ->
    not_loaded(?LINE).

%% @doc Return a key press or key release event's UTF-8 value
-spec get_event_key_utf8(Event::reference()) -> string() | {error, any()}.

get_event_key_utf8(_) ->
    not_loaded(?LINE).

%% @doc Return a mouse press or mouse release event's button
-spec get_event_mouse_button(Event::reference()) -> integer() | {error, any()}.

get_event_mouse_button(_) ->
    not_loaded(?LINE).

%% @doc Return a mouse motion event's X coordinate
-spec get_event_mouse_x(Event::reference()) -> integer() | {error, any()}.

get_event_mouse_x(_) ->
    not_loaded(?LINE).

%% @doc Return a mouse motion event's Y coordinate
-spec get_event_mouse_y(Event::reference()) -> integer() | {error, any()}.

get_event_mouse_y(_) ->
    not_loaded(?LINE).

%% @doc Return a mouse motion event's Y coordinate
-spec get_event_resize_width(Event::reference()) -> integer() | {error, any()}.

get_event_resize_width(_) ->
    not_loaded(?LINE).

%% @doc Return a mouse motion event's Y coordinate
-spec get_event_resize_height(Event::reference()) -> integer() | {error, any()}.

get_event_resize_height(_) ->
    not_loaded(?LINE).

%% @doc Create an internal dither object
-spec create_dither(Bpp::integer(), Width::integer(), Height::integer(),
                    Pitch::integer(), Rmask::integer(), Gmask::integer(), 
                    Bmask::integer(), Amask::integer()) -> 
    {ok, reference()} | {error, any()}.

create_dither(_,_,_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Free the memory associated with a dither
-spec free_dither(Dither::reference()) ->  ok | {error, any()}.

free_dither(_) ->
    not_loaded(?LINE).

%% @doc Set the brightness of a dither object
-spec set_dither_brightness(Dither::reference(), Value::float()) ->  ok | {error, any()}.

set_dither_brightness(_,_) ->
    not_loaded(?LINE).

%% @doc Get the brightness of a dither object
-spec get_dither_brightness(Dither::reference()) ->  float() | {error, any()}.

get_dither_brightness(_) ->
    not_loaded(?LINE).

%% @doc Set the gamma of a dither object
-spec set_dither_gamma(Dither::reference(), Value::float()) ->  ok | {error, any()}.

set_dither_gamma(_,_) ->
    not_loaded(?LINE).

%% @doc Get the gamma of a dither object
-spec get_dither_gamma(Dither::reference()) ->  float() | {error, any()}.

get_dither_gamma(_) ->
    not_loaded(?LINE).

%% @doc Set the contrast of a dither object
-spec set_dither_contrast(Dither::reference(), Value::float()) ->  ok | {error, any()}.

set_dither_contrast(_,_) ->
    not_loaded(?LINE).

%% @doc Get the contrast of a dither object
-spec get_dither_contrast(Dither::reference()) ->  float() | {error, any()}.

get_dither_contrast(_) ->
    not_loaded(?LINE).

%% @doc Set dither antialiasing
-spec set_dither_antialias(Dither::reference(), Value::string()) ->  ok | {error, any()}.

set_dither_antialias(_,_) ->
    not_loaded(?LINE).

%% @doc Get current antialiasing method
-spec get_dither_antialias(Dither::reference()) ->  string() | {error, any()}.

get_dither_antialias(_) ->
    not_loaded(?LINE).

%% @doc Get available antialiasing methods
-spec get_dither_antialias_list(Dither::reference()) ->  list(string) | {error, any()}.

get_dither_antialias_list(_) ->
    not_loaded(?LINE).

%% @doc Choose colours used for dithering
-spec set_dither_color(Dither::reference(), Value::string()) ->  ok | {error, any()}.

set_dither_color(_,_) ->
    not_loaded(?LINE).

%% @doc Get current colour mode
-spec get_dither_color(Dither::reference()) ->  string() | {error, any()}.

get_dither_color(_) ->
    not_loaded(?LINE).

%% @doc Get available colour modes
-spec get_dither_color_list(Dither::reference()) ->  list(string) | {error, any()}.

get_dither_color_list(_) ->
    not_loaded(?LINE).

%% @doc Choose characters used for dithering
-spec set_dither_charset(Dither::reference(), Value::string()) ->  ok | {error, any()}.

set_dither_charset(_,_) ->
    not_loaded(?LINE).

%% @doc Get available dither character sets
-spec get_dither_charset(Dither::reference()) ->  string() | {error, any()}.

get_dither_charset(_) ->
    not_loaded(?LINE).

%% @doc Get current character set
-spec get_dither_charset_list(Dither::reference()) ->  list(string) | {error, any()}.

get_dither_charset_list(_) ->
    not_loaded(?LINE).

%% @doc Set dithering algorithm
-spec set_dither_algorithm(Dither::reference(), Value::string()) ->  ok | {error, any()}.

set_dither_algorithm(_,_) ->
    not_loaded(?LINE).

%% @doc Get current dithering algorithm
-spec get_dither_algorithm(Dither::reference()) ->  string() | {error, any()}.

get_dither_algorithm(_) ->
    not_loaded(?LINE).

%% @doc Get dithering algorithms
-spec get_dither_algorithm_list(Dither::reference()) ->  list(string) | {error, any()}.

get_dither_algorithm_list(_) ->
    not_loaded(?LINE).

%% @doc Dither a bitmap on the canvas
-spec dither_bitmap(Canvas::reference(), X::integer(), Y::integer(),
                    W::integer(), H::integer(), Dither::reference(), 
                    Pixels::binary()) ->  ok | {error, any()}.

dither_bitmap(_,_,_,_,_,_,_) ->
    not_loaded(?LINE).

%% @doc Load a font from memory for future use
-spec load_font(Fontname::string()) -> {ok, reference()} | {error, any()}.

load_font(_) ->
    not_loaded(?LINE).

%% @doc Get a font's standard glyph width
-spec get_font_width(Font::reference()) -> integer() | {error, any()}.

get_font_width(_) ->
    not_loaded(?LINE).

%% @doc Get a font's standard glyph height
-spec get_font_height(Font::reference()) -> integer() | {error, any()}.

get_font_height(_) ->
    not_loaded(?LINE).

%% @doc Free a font structure
-spec free_font(Font::reference()) -> ok | {error, any()}.

free_font(_) ->
    not_loaded(?LINE).

%% @doc Render the canvas onto an image buffer
-spec render_canvas(Canvas::reference(), Font::reference(), Buf::binary(), 
                    Width::integer(), Height::integer(), Pitch::integer()) -> 
    ok | {error, any()}.

render_canvas(_,_,_,_,_,_) ->
    not_loaded(?LINE).

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

