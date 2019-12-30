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
-export([draw_line/6, draw_thin_line/5, draw_polyline/4, draw_thin_polyline/3]).
-export([draw_circle/5, draw_ellipse/6, draw_thin_ellipse/5, fill_ellipse/6]).
-export([draw_box/6, draw_thin_box/5, draw_cp437_box/5, fill_box/6]).
-export([draw_triangle/8, draw_thin_triangle/7, fill_triangle/8]).
-export([get_frame_count/1, set_frame/2, get_frame_name/1, set_frame_name/2]).
-export([create_frame/2, free_frame/2]).
-export([create_display/0, create_display/1, create_display_with_driver/2, free_display/1]).
-export([get_display_driver/1, set_display_driver/2, get_canvas/1]).
-export([refresh_display/1, get_display_time/1, set_display_time/2]).
-export([get_display_width/1, get_display_height/1, set_display_title/2]).
-export([load_font/1, get_font_width/1, get_font_height/1, free_font/1]).
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

