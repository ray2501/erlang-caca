#include "erl_nif.h"
#include <string.h>
#include <assert.h>
#include <caca.h>

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);

static ErlNifResourceType* RES_TYPE;

typedef struct {
    caca_canvas_t *canvas;
    caca_display_t *display;
} CACA;

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL, "caca_res",
                                                     NULL,
                                                     ERL_NIF_RT_CREATE, NULL);

    if(rt == NULL) return -1;
    assert(rt != NULL);
    RES_TYPE = rt;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM
get_display_driver_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM erl_map;    
    char const * const * list = NULL;
    int count = 0;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }
    
    list = caca_get_display_driver_list();
    if(!list) return mk_error(env, "return_error");

    if (list) {
        erl_map = enif_make_new_map(env);

        // The list actually is a key-value map, so I return a map.
        for(count = 0; list[count] != NULL; count += 2) {
            ERL_NIF_TERM key;
            ERL_NIF_TERM val;

            key = enif_make_string(env, list[count], ERL_NIF_LATIN1);
            val = enif_make_string(env, list[count + 1], ERL_NIF_LATIN1);
            if (!enif_make_map_put(env, erl_map, key, val, &erl_map)) {
                return mk_error(env, "map_error");
            }       
        }

        return erl_map;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_export_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM erl_map;    
    char const * const * list = NULL;
    int count = 0;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }
    
    list = caca_get_export_list();
    if(!list) return mk_error(env, "return_error");

    if (list) {
        erl_map = enif_make_new_map(env);

        // The list actually is a key-value map, so I return a map.
        for(count = 0; list[count] != NULL; count += 2) {
            ERL_NIF_TERM key;
            ERL_NIF_TERM val;

            key = enif_make_string(env, list[count], ERL_NIF_LATIN1);
            val = enif_make_string(env, list[count + 1], ERL_NIF_LATIN1);
            if (!enif_make_map_put(env, erl_map, key, val, &erl_map)) {
                return mk_error(env, "map_error");
            }       
        }

        return erl_map;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_import_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM erl_map;    
    char const * const * list = NULL;
    int count = 0;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }
    
    list = caca_get_import_list();
    if(!list) return mk_error(env, "return_error");

    if (list) {
        erl_map = enif_make_new_map(env);

        // The list actually is a key-value map, so I return a map.
        for(count = 0; list[count] != NULL; count += 2) {
            ERL_NIF_TERM key;
            ERL_NIF_TERM val;

            key = enif_make_string(env, list[count], ERL_NIF_LATIN1);
            val = enif_make_string(env, list[count + 1], ERL_NIF_LATIN1);
            if (!enif_make_map_put(env, erl_map, key, val, &erl_map)) {
                return mk_error(env, "map_error");
            }       
        }

        return erl_map;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_font_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM erl_list;
    char const * const * list = NULL;
    int count = 0;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }

    list = caca_get_font_list();
    if(!list) return mk_error(env, "return_error");

    if (list) {
        erl_list = enif_make_list(env, 0);
        for(count = 0; list[count] != NULL; count ++) {
            erl_list = enif_make_list_cell(env,
                                           enif_make_string(env, list[count], ERL_NIF_LATIN1),
                                           erl_list);
        }

        return erl_list;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
erand(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int min = 0, max = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[0], &min))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &max))
    {
        return enif_make_badarg(env);
    }

    result = caca_rand(min, max);
    return enif_make_int(env, result);
}

static ERL_NIF_TERM
create_canvas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    caca_canvas_t *canvas = NULL;
    ERL_NIF_TERM ret;
    int width = 0, height = 0;
    CACA* res;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[0], &width))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &height))
    {
        return enif_make_badarg(env);
    }

    if(width < 0 || height < 0) {
        return enif_make_badarg(env);
    }

    canvas = caca_create_canvas(width, height);
    if(!canvas) return mk_error(env, "error");

    res = enif_alloc_resource(RES_TYPE, sizeof(CACA));
    if(res == NULL) return mk_error(env, "alloc_error");
    res->canvas = NULL;
    res->display = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->canvas = canvas;

    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
free_canvas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        caca_free_canvas (res->canvas);
        res->canvas = NULL;
        assert(res->canvas == NULL);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
set_canvas_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int width = 0, height = 0;
    int result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &width))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &height))
    {
        return enif_make_badarg(env);
    }

    if(width < 0 || height < 0) {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        result = caca_set_canvas_size(res->canvas, width, height);
        if(result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_canvas_width(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int width = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        width = caca_get_canvas_width(res->canvas);
        ret = enif_make_int(env, width);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_canvas_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int height = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        height = caca_get_canvas_height(res->canvas);
        ret = enif_make_int(env, height);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
gotoxy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        caca_gotoxy(res->canvas, x, y);

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
wherex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        x = caca_wherex(res->canvas);
        ret = enif_make_int(env, x);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
wherey(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int y = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        y = caca_wherey(res->canvas);
        ret = enif_make_int(env, y);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
put_char(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0;
    uint32_t mychar = 0;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 4)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[3], &mychar))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        result = caca_put_char(res->canvas, x, y, mychar);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_char(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0;
    uint32_t mychar = 0;
    ERL_NIF_TERM ret;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        mychar = caca_get_char(res->canvas, x, y);
        ret = enif_make_uint(env, mychar);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_color_ansi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int fg = 0, bg = 0;
    int result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &fg))
    {
        return enif_make_badarg(env);
    }

    if (fg < 0 || fg > 32) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &bg))
    {
        return enif_make_badarg(env);
    }

    if (bg < 0 || bg > 32) {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->canvas) {
        result = caca_set_color_ansi(res->canvas, (uint8_t) fg, (uint8_t) bg);
        if (result < 0) {
            return mk_error(env, "alloc_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
create_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    caca_display_t *display = NULL;
    ERL_NIF_TERM ret;
    CACA* can;
    CACA* res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &can))
    {
        return enif_make_badarg(env);
    }

    if(!can->canvas) {
        return enif_make_badarg(env);
    }

    display = caca_create_display(can->canvas);
    if(!display) return mk_error(env, "error");

    res = enif_alloc_resource(RES_TYPE, sizeof(CACA));
    if(res == NULL) return mk_error(env, "alloc_error");
    res->canvas = NULL;
    res->display = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->display = display;

    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
create_display_with_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char *buffer = NULL;
    unsigned int length = 0;
    caca_display_t *display = NULL;
    ERL_NIF_TERM ret;
    CACA* can;
    CACA* res;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &can))
    {
        return enif_make_badarg(env);
    }

    if(!can->canvas) {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    buffer = (char *) malloc(sizeof(char) * length + 1);
    if(!buffer) {
        return mk_error(env, "no_memory");
    }

    (void)memset(buffer, '\0', length + 1);

    if (enif_get_string(env, argv[1], buffer, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(buffer) free(buffer);
        return enif_make_badarg(env);
    }

    display = caca_create_display(can->canvas);
    if(!display) {
        if(buffer) free(buffer);
        return mk_error(env, "function_error");
    }

    res = enif_alloc_resource(RES_TYPE, sizeof(CACA));
    if(res == NULL) {
        if(buffer) free(buffer);
        return mk_error(env, "alloc_error");
    }
    res->canvas = NULL;
    res->display = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->display = display;

    if(buffer) free(buffer);
    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
free_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->display) {
        caca_free_display (res->display);
        res->display = NULL;
        assert(res->display == NULL);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
refresh_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->display) {
        caca_refresh_display (res->display);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ErlNifFunc nif_funcs[] = {
    {"get_display_driver_list", 0, get_display_driver_list},
    {"get_export_list", 0, get_export_list},
    {"get_import_list", 0, get_import_list},
    {"get_font_list", 0, get_font_list},
    {"rand", 2, erand},
    {"create_canvas", 2, create_canvas},
    {"free_canvas", 1, free_canvas},
    {"set_canvas_size", 3, set_canvas_size},
    {"get_canvas_width", 1, get_canvas_width},
    {"get_canvas_height", 1, get_canvas_height},
    {"gotoxy", 3, gotoxy},
    {"wherex", 1, wherex},
    {"wherey", 1, wherey},
    {"put_char", 4, put_char},
    {"get_char", 3, get_char},
    {"set_color_ansi", 3, set_color_ansi},
    {"create_display", 1, create_display},
    {"create_display_with_driver", 2, create_display_with_driver},
    {"free_display", 1, free_display},
    {"refresh_display", 1, refresh_display},
};

ERL_NIF_INIT(caca, nif_funcs, &load, &reload, NULL, NULL)
