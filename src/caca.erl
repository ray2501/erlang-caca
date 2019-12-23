%% ----------------------------------
%% Erlang bindings for libcaca
%% ----------------------------------

-module(caca).

%% API exports
-export([get_display_driver_list/0, get_export_list/0, get_import_list/0]).
-export([get_font_list/0, rand/2]).
-export([create_canvas/2, free_canvas/1]).
-on_load(init/0).

-define(APPNAME, caca).
-define(LIBNAME, "caca_nif").

%%====================================================================
%% API functions
%%====================================================================

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

