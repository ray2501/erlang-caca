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
    caca_font_t *font;
    caca_event_t *event;
    caca_dither_t *dither;
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
caca_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM verTerm;
    const char *version = NULL;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }

    version = caca_get_version();
    if(!version) return mk_error(env, "return_error");

    if (version) {
         verTerm = enif_make_string(env, version, ERL_NIF_LATIN1);
         return verTerm;
    }

    return mk_error(env, "error");
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
    res->font = NULL;
    res->event = NULL;
    res->dither = NULL;

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

    if(res->canvas == NULL) {
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

    if(res->canvas) {
        mychar = caca_get_char(res->canvas, x, y);
        ret = enif_make_uint(env, mychar);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
put_str(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0;
    char *buffer = NULL;
    unsigned int length = 0;

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

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[3], &length)) {
        return enif_make_badarg(env);
    }

    if (length < 1) {
        return enif_make_badarg(env);
    }

    buffer = (char *) malloc(sizeof(char) * length + 1);
    if(!buffer) {
        return mk_error(env, "no_memory");
    }

    (void)memset(buffer, '\0', length + 1);

    if (enif_get_string(env, argv[3], buffer, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(buffer) free(buffer);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_put_str(res->canvas, x, y, buffer);
        if(buffer) free(buffer);
        return mk_atom(env, "ok");
    }

    if(buffer) free(buffer);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_attr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0;
    uint32_t result = 0;

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

    if(res->canvas) {
        result = caca_get_attr(res->canvas, x, y);
        return enif_make_uint(env, (unsigned int) result);
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_attr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    uint32_t attr = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[1], (unsigned int *) &attr))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_set_attr(res->canvas, attr);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
unset_attr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    uint32_t attr = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[1], (unsigned int *) &attr))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_unset_attr(res->canvas, attr);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
toggle_attr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    uint32_t attr = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[1], (unsigned int *) &attr))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_toggle_attr(res->canvas, attr);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
put_attr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0;
    uint32_t attr = 0;

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

    if (!enif_get_uint(env, argv[3], (unsigned int *) &attr))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_put_attr(res->canvas, x, y, attr);
        return mk_atom(env, "ok");
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
set_color_argb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    if (fg < 0 || fg > 0xffff) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &bg))
    {
        return enif_make_badarg(env);
    }

    if (bg < 0 || bg > 0xffff) {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_set_color_argb(res->canvas, (uint16_t) fg, (uint16_t) bg);
        if (result < 0) {
            return mk_error(env, "alloc_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
clear_canvas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    if(res->canvas) {
        caca_clear_canvas(res->canvas);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_canvas_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0;
    int result = 0;

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

    if(res->canvas) {
        result = caca_set_canvas_handle(res->canvas, x, y);
        if(result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_canvas_handle_x(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    if(res->canvas) {
        x = caca_get_canvas_handle_x(res->canvas);
        ret = enif_make_int(env, x);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_canvas_handle_y(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    if(res->canvas) {
        y = caca_get_canvas_handle_y(res->canvas);
        ret = enif_make_int(env, y);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
blit_4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    CACA *src;
    int x = 0, y = 0;
    int result = 0;

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

    if(!enif_get_resource(env, argv[3], RES_TYPE, (void **) &src))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas && src->canvas) {
        result = caca_blit(res->canvas, x, y, src->canvas, NULL);
        if (result < 0) {
            return mk_error(env, "function_error");
        }
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
blit_5(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    CACA *src;
    CACA *mask;
    int x = 0, y = 0;
    int result = 0;

    if(argc != 5)
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

    if(!enif_get_resource(env, argv[3], RES_TYPE, (void **) &src))
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[4], RES_TYPE, (void **) &mask))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas && src->canvas && mask->canvas) {
        result = caca_blit(res->canvas, x, y, src->canvas, mask->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_canvas_boundaries(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, w = 0, h = 0;
    int result = 0;

    if(argc != 5)
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

    if (!enif_get_int(env, argv[3], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &h))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_set_canvas_boundaries(res->canvas, x, y, w, h);
        if (result < 0) {
            return mk_error(env, "function_error");
        }
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
invert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_invert(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
flip(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_flip(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
flop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_flop(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
rotate_180(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_rotate_180(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
rotate_left(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_rotate_left(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
rotate_right(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_rotate_right(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
stretch_left(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_stretch_left(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
stretch_right(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_stretch_right(res->canvas);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_line(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0;
    uint32_t mychar = 0;

    if(argc != 6)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &x2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &y2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[5], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_line(res->canvas, x1, y1, x2, y2, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_thin_line(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0;

    if(argc != 5)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &x2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &y2))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_thin_line(res->canvas, x1, y1, x2, y2);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_polyline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int *xarray = NULL, *yarray = NULL;
    uint32_t mychar;
    int count = 0;
    ERL_NIF_TERM list, hd, tl;
    unsigned int lengthx = 0, lengthy = 0;

    if(argc != 4)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_list_length(env, argv[1], &lengthx)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_list_length(env, argv[2], &lengthy)) {
        return enif_make_badarg(env);
    }

    if(lengthx == 0 || lengthy == 0) {
        return enif_make_badarg(env);
    }

    if (lengthx != lengthy) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[3], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    list = argv[1];
    count = 0;
    xarray = (int*) malloc(sizeof(int) * lengthx);
    if(!xarray) return mk_error(env, "malloc_error");
    while (enif_get_list_cell(env, list, &hd, &tl)) {
        if (!enif_get_int(env, hd, &xarray[count])) {
             if(xarray) free(xarray);
             return enif_make_badarg(env);
        }

        count++;
        list = tl;
    }

    list = argv[2];
    count = 0;
    yarray = (int*) malloc(sizeof(int) * lengthy);
    if(!yarray) return mk_error(env, "malloc_error");
    while (enif_get_list_cell(env, list, &hd, &tl)) {
        if (!enif_get_int(env, hd, &yarray[count])) {
             if(yarray) free(yarray);
             return enif_make_badarg(env);
        }

        count++;
        list = tl;
    }

    if(res->canvas) {
        caca_draw_polyline(res->canvas, xarray, yarray, lengthx - 1, mychar);

        if(xarray) free(xarray);
        if(yarray) free(yarray);
        return mk_atom(env, "ok");
    }

    if(xarray) free(xarray);
    if(yarray) free(yarray);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_thin_polyline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int *xarray = NULL, *yarray = NULL;
    int count = 0;
    ERL_NIF_TERM list, hd, tl;
    unsigned int lengthx = 0, lengthy = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_list_length(env, argv[1], &lengthx)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_list_length(env, argv[2], &lengthy)) {
        return enif_make_badarg(env);
    }

    if(lengthx == 0 || lengthy == 0) {
        return enif_make_badarg(env);
    }

    if (lengthx != lengthy) {
        return enif_make_badarg(env);
    }

    list = argv[1];
    count = 0;
    xarray = (int*) malloc(sizeof(int) * lengthx);
    if(!xarray) return mk_error(env, "malloc_error");
    while (enif_get_list_cell(env, list, &hd, &tl)) {
        if (!enif_get_int(env, hd, &xarray[count])) {
             if(xarray) free(xarray);
             return enif_make_badarg(env);
        }

        count++;
        list = tl;
    }

    list = argv[2];
    count = 0;
    yarray = (int*) malloc(sizeof(int) * lengthy);
    if(!yarray) return mk_error(env, "malloc_error");
    while (enif_get_list_cell(env, list, &hd, &tl)) {
        if (!enif_get_int(env, hd, &yarray[count])) {
             if(yarray) free(yarray);
             return enif_make_badarg(env);
        }

        count++;
        list = tl;
    }

    if(res->canvas) {
        caca_draw_thin_polyline(res->canvas, xarray, yarray, lengthx - 1);

        if(xarray) free(xarray);
        if(yarray) free(yarray);
        return mk_atom(env, "ok");
    }

    if(xarray) free(xarray);
    if(yarray) free(yarray);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_circle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, r = 0;
    uint32_t mychar = 0;

    if(argc != 5)
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

    if (!enif_get_int(env, argv[3], &r))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[4], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_circle(res->canvas, x, y, r, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_ellipse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, a = 0, b = 0;
    uint32_t mychar = 0;

    if(argc != 6)
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

    if (!enif_get_int(env, argv[3], &a))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &b))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[5], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_ellipse(res->canvas, x, y, a, b, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_thin_ellipse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, a = 0, b = 0;

    if(argc != 5)
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

    if (!enif_get_int(env, argv[3], &a))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &b))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_thin_ellipse(res->canvas, x, y, a, b);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
fill_ellipse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, a = 0, b = 0;
    uint32_t mychar = 0;

    if(argc != 6)
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

    if (!enif_get_int(env, argv[3], &a))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &b))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[5], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_fill_ellipse(res->canvas, x, y, a, b, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_box(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, w = 0, h = 0;
    uint32_t mychar = 0;

    if(argc != 6)
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

    if (!enif_get_int(env, argv[3], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &h))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[5], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_box(res->canvas, x, y, w, h, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_thin_box(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, w = 0, h = 0;

    if(argc != 5)
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

    if (!enif_get_int(env, argv[3], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &h))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_thin_box(res->canvas, x, y, w, h);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_cp437_box(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, w = 0, h = 0;

    if(argc != 5)
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

    if (!enif_get_int(env, argv[3], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &h))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_cp437_box(res->canvas, x, y, w, h);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
fill_box(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x = 0, y = 0, w = 0, h = 0;
    uint32_t mychar = 0;

    if(argc != 6)
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

    if (!enif_get_int(env, argv[3], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &h))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[5], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_fill_box(res->canvas, x, y, w, h, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_triangle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0, x3 = 0, y3 = 0;
    uint32_t mychar = 0;

    if(argc != 8)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &x2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &y2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[5], &x3))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[6], &y3))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[7], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_triangle(res->canvas, x1, y1, x2, y2, x3, y3, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
draw_thin_triangle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0, x3 = 0, y3 = 0;

    if(argc != 7)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &x2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &y2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[5], &x3))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[6], &y3))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_draw_thin_triangle(res->canvas, x1, y1, x2, y2, x3, y3);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
fill_triangle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int x1 = 0, y1 = 0, x2 = 0, y2 = 0, x3 = 0, y3 = 0;
    uint32_t mychar = 0;

    if(argc != 8)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &x1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &y1))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &x2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &y2))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[5], &x3))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[6], &y3))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[7], (unsigned int*) &mychar))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        caca_fill_triangle(res->canvas, x1, y1, x2, y2, x3, y3, mychar);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_frame_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    CACA* res;
    int result = 0;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_get_frame_count(res->canvas);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_frame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    int id = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &id))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_set_frame(res->canvas, id);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_frame_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    CACA* res;
    const char *result = NULL;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_get_frame_name(res->canvas);
        ret = enif_make_string(env, result, ERL_NIF_LATIN1);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_frame_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    char * name = NULL;
    unsigned int length = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    name = (char *) malloc(sizeof(char) * length + 1);
    if(!name) {
        return mk_error(env, "no_memory");
    }

    (void)memset(name, '\0', length + 1);

    if (enif_get_string(env, argv[1], name, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(name) free(name);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_set_frame_name(res->canvas, name);
        if(name) free(name);

        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    if(name) free(name);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
create_frame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    int id = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &id))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        /* The frame index indicates where the frame should be inserted. 
         * Valid values range from 0 to the current canvas frame count. 
         * If the frame index is greater than or equals the current canvas 
         * frame count, the new frame is appended at the end of the canvas. 
         * If the frame index is less than zero, the new frame is inserted 
         * at index 0.
         */
        result = caca_create_frame(res->canvas, id);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
free_frame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    int id = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &id))
    {
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        /* The frame index indicates the frame to delete. 
         * Valid values range from 0 to the current canvas 
         * frame count minus 1.
         */
        result = caca_free_frame(res->canvas, id);
        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
import_canvas_from_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    char *fmt = NULL;
    unsigned int length = 0;
    ErlNifBinary buf = {0};
    int result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &buf))
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[2], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    fmt = (char *) malloc(sizeof(char) * length + 1);
    if(!fmt) {
        return mk_error(env, "no_memory");
    }

    (void)memset(fmt, '\0', length + 1);

    if (enif_get_string(env, argv[2], fmt, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(fmt) free(fmt);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_import_canvas_from_memory(res->canvas, buf.data, buf.size, fmt);
        if(fmt) free(fmt);

        if (result < 0) {
            return mk_error(env, "function_error");
        } else if(result == 0) {
            return mk_error(env, "no_memory_error");
        }

        return mk_atom(env, "ok");
    }

    if(fmt) free(fmt);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
import_canvas_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    char *filename = NULL;
    char *fmt = NULL;
    unsigned int length = 0;
    int result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    filename = (char *) malloc(sizeof(char) * length + 1);
    if(!filename) {
        return mk_error(env, "no_memory");
    }

    (void)memset(filename, '\0', length + 1);

    if (enif_get_string(env, argv[1], filename, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(filename) free(filename);
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[2], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    fmt = (char *) malloc(sizeof(char) * length + 1);
    if(!fmt) {
        return mk_error(env, "no_memory");
    }

    (void)memset(fmt, '\0', length + 1);

    if (enif_get_string(env, argv[2], fmt, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(fmt) free(fmt);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_import_canvas_from_file(res->canvas, filename, fmt);
        if(filename) free(filename);
        if(fmt) free(fmt);

        if (result < 0) {
            return mk_error(env, "function_error");
        } else if(result == 0) {
            return mk_error(env, "no_memory_error");
        }

        return mk_atom(env, "ok");
    }

    if(filename) free(filename);
    if(fmt) free(fmt);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
import_area_from_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    char *fmt = NULL;
    unsigned int length = 0;
    int x = 0, y = 0;
    ErlNifBinary buf = {0};
    int result = 0;

    if(argc != 5)
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

    if (!enif_inspect_binary(env, argv[3], &buf))
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[4], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    fmt = (char *) malloc(sizeof(char) * length + 1);
    if(!fmt) {
        return mk_error(env, "no_memory");
    }

    (void)memset(fmt, '\0', length + 1);

    if (enif_get_string(env, argv[4], fmt, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(fmt) free(fmt);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_import_area_from_memory(res->canvas, x, y, buf.data, buf.size, fmt);
        if(fmt) free(fmt);

        if (result < 0) {
            return mk_error(env, "function_error");
        } else if(result == 0) {
            return mk_error(env, "no_memory_error");
        }

        return mk_atom(env, "ok");
    }

    if(fmt) free(fmt);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
import_area_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    char *filename = NULL;
    char *fmt = NULL;
    int x = 0, y = 0;
    unsigned int length = 0;
    int result = 0;

    if(argc != 5)
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

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[3], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    filename = (char *) malloc(sizeof(char) * length + 1);
    if(!filename) {
        return mk_error(env, "no_memory");
    }

    (void)memset(filename, '\0', length + 1);

    if (enif_get_string(env, argv[3], filename, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(filename) free(filename);
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[4], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    fmt = (char *) malloc(sizeof(char) * length + 1);
    if(!fmt) {
        return mk_error(env, "no_memory");
    }

    (void)memset(fmt, '\0', length + 1);

    if (enif_get_string(env, argv[4], fmt, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(fmt) free(fmt);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        result = caca_import_area_from_file(res->canvas, x, y, filename, fmt);
        if(filename) free(filename);
        if(fmt) free(fmt);

        if (result < 0) {
            return mk_error(env, "function_error");
        } else if(result == 0) {
            return mk_error(env, "no_memory_error");
        }

        return mk_atom(env, "ok");
    }

    if(filename) free(filename);
    if(fmt) free(fmt);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
export_canvas_to_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    char *fmt = NULL;
    unsigned int length = 0;
    int buffer_length = 0;
    void *buffer = NULL;
    ErlNifBinary result_bin;
    ERL_NIF_TERM result;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    fmt = (char *) malloc(sizeof(char) * length + 1);
    if(!fmt) {
        return mk_error(env, "no_memory");
    }

    (void)memset(fmt, '\0', length + 1);

    if (enif_get_string(env, argv[1], fmt, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(fmt) free(fmt);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        buffer = caca_export_canvas_to_memory(res->canvas, fmt, (size_t *) &buffer_length);
        if(fmt) free(fmt);

        if (buffer==NULL) {
            return mk_error(env, "function_error");
        }

        /* 
         * Prepare to return a binary term
         */
        if(!enif_alloc_binary(buffer_length, &result_bin)) {
            return mk_error(env, "malloc_error");
        }

        memcpy(result_bin.data, buffer, buffer_length);
        result = enif_make_binary(env, &result_bin);

        return result;
    }

    if(fmt) free(fmt);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
export_area_to_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA* res;
    char *fmt = NULL;
    unsigned int length = 0;
    int x = 0, y = 0, w = 0, h = 0;
    int buffer_length = 0;
    void *buffer = NULL;
    ErlNifBinary result_bin;
    ERL_NIF_TERM result;

    if(argc != 6)
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

    if (!enif_get_int(env, argv[3], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &h))
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[5], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    fmt = (char *) malloc(sizeof(char) * length + 1);
    if(!fmt) {
        return mk_error(env, "no_memory");
    }

    (void)memset(fmt, '\0', length + 1);

    if (enif_get_string(env, argv[5], fmt, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(fmt) free(fmt);
        return enif_make_badarg(env);
    }

    if(res->canvas) {
        buffer = caca_export_canvas_to_memory(res->canvas, fmt, (size_t *) &buffer_length);
        if(fmt) free(fmt);

        if (buffer==NULL) {
            return mk_error(env, "function_error");
        }

        /* 
         * Prepare to return a binary term
         */
        if(!enif_alloc_binary(buffer_length, &result_bin)) {
            return mk_error(env, "malloc_error");
        }

        memcpy(result_bin.data, buffer, buffer_length);
        result = enif_make_binary(env, &result_bin);

        return result;
    }

    if(fmt) free(fmt);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
create_display_0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    caca_display_t *display = NULL;
    ERL_NIF_TERM ret;
    CACA* res;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }

    display = caca_create_display(NULL);
    if(!display) return mk_error(env, "error");

    res = enif_alloc_resource(RES_TYPE, sizeof(CACA));
    if(res == NULL) return mk_error(env, "alloc_error");
    res->canvas = NULL;
    res->display = NULL;
    res->font = NULL;
    res->event = NULL;
    res->dither = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->display = display;

    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
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
    res->font = NULL;
    res->event = NULL;
    res->dither = NULL;

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

    if(length < 1) {
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
    res->font = NULL;
    res->event = NULL;
    res->dither = NULL;

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

    if(res->display == NULL) {
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
get_display_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    const char *driver = NULL;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        driver = caca_get_display_driver (res->display);
        ret = enif_make_string(env, driver, ERL_NIF_LATIN1);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_display_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    char *buffer = NULL;
    unsigned int length = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
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

    if(res->display) {
        result = caca_set_display_driver (res->display, buffer);
        if(buffer) free(buffer);

        if (result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    if(buffer) free(buffer);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_canvas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    caca_canvas_t *canvas;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        canvas = caca_get_canvas (res->display);
        if(!canvas) return mk_error(env, "function_error");

        res = enif_alloc_resource(RES_TYPE, sizeof(CACA));
        if(res == NULL) return mk_error(env, "alloc_error");
        res->canvas = NULL;
        res->display = NULL;
        res->font = NULL;
        res->event = NULL;
        res->dither = NULL;

        ret = enif_make_resource(env, res);
        enif_release_resource(res);

        res->canvas = canvas;

        return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
    }

    return mk_error(env, "error");
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

    if(res->display) {
        caca_refresh_display (res->display);
        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_display_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        // The render time in microseconds
        result = caca_get_display_time (res->display);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_display_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int usec = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &usec))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        result = caca_set_display_time (res->display, usec);
        if(result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_display_width(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        result = caca_get_display_width (res->display);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_display_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        result = caca_get_display_height (res->display);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_display_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    char *buffer = NULL;
    unsigned int length = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
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

    if(res->display) {
        result = caca_set_display_title (res->display, buffer);
        if(buffer) free(buffer);

        if(result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    if(buffer) free(buffer);
    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_mouse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int flag = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &flag))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        result = caca_set_mouse (res->display, flag);
        if(result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
set_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int flag = 0;
    int result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &flag))
    {
        return enif_make_badarg(env);
    }

    if(res->display) {
        result = caca_set_cursor (res->display, flag);
        if(result < 0) {
            return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
create_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    caca_event_t *event = NULL;
    ERL_NIF_TERM ret;
    CACA* res;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }

    event = (caca_event_t *) malloc(sizeof(caca_event_t));
    if(!event) {
        return mk_error(env, "no_memory");
    }

    (void)memset(event, 0x00, sizeof(caca_event_t));

    res = enif_alloc_resource(RES_TYPE, sizeof(CACA));
    if(res == NULL) {
        return mk_error(env, "alloc_error");
    }
    res->canvas = NULL;
    res->display = NULL;
    res->font = NULL;
    res->event = NULL;
    res->dither = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->event = event;
    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
free_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    if(res->event == NULL) {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->event) {
        free(res->event);
        res->event = NULL;
        assert(res->event == NULL);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
get_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *dis;
    CACA *evt;
    int mask = 0;
    int timeout = 0;

    if(argc != 4)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &dis))
    {
        return enif_make_badarg(env);
    }

    if(dis->display == NULL) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &mask))
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[2], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event == NULL) {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &timeout))
    {
        return enif_make_badarg(env);
    }

    if(dis->display && evt->event) {
        caca_get_event(dis->display, mask, evt->event, timeout);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
get_event_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = (int) caca_get_event_type(evt->event);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_key_ch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = caca_get_event_key_ch(evt->event);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_key_utf32(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    uint32_t result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = caca_get_event_key_utf32(evt->event);
        ret = enif_make_uint(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_key_utf8(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    char utf8[7];
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        caca_get_event_key_utf8(evt->event, utf8);
        ret = enif_make_string(env, utf8, ERL_NIF_LATIN1);

        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_mouse_button(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = caca_get_event_mouse_button(evt->event);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_mouse_x(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = caca_get_event_mouse_x(evt->event);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_mouse_y(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = caca_get_event_mouse_y(evt->event);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_resize_width(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = caca_get_event_resize_width(evt->event);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_event_resize_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *evt;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &evt))
    {
        return enif_make_badarg(env);
    }

    if(evt->event) {
        result = caca_get_event_resize_height(evt->event);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
create_dither(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    caca_dither_t *dither = NULL;
    int bpp = 0, w = 0, h = 0, pitch = 0;
    uint32_t rmask = 0, gmask = 0, bmask = 0, amask = 0;
    CACA* res;
    ERL_NIF_TERM ret;

    if(argc != 8)
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[0], &bpp))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[1], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &w))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &pitch))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[4], &rmask))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[5], &gmask))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[6], &bmask))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[7], &amask))
    {
        return enif_make_badarg(env);
    }

    dither = caca_create_dither(bpp, w, h, pitch, rmask, gmask, bmask, amask);
    if(!dither) {
        return mk_error(env, "function_error");
    }

    res = enif_alloc_resource(RES_TYPE, sizeof(CACA));
    if(res == NULL) {
        return mk_error(env, "alloc_error");
    }
    res->canvas = NULL;
    res->display = NULL;
    res->font = NULL;
    res->event = NULL;
    res->dither = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->dither = dither;
    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
free_dither(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    if(res->dither == NULL) {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->dither) {
        caca_free_dither(res->dither);
        res->dither = NULL;
        assert(res->font == NULL);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
load_font(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    caca_font_t *font = NULL;
    char *buffer = NULL;
    unsigned int length = 0;
    ERL_NIF_TERM ret;
    CACA* res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    // String in Erlang is a list, so try to get list length
    if(!enif_get_list_length(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }

    if(length < 1) {
        return enif_make_badarg(env);
    }

    buffer = (char *) malloc(sizeof(char) * length + 1);
    if(!buffer) {
        return mk_error(env, "no_memory");
    }

    (void)memset(buffer, '\0', length + 1);

    if (enif_get_string(env, argv[0], buffer, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(buffer) free(buffer);
        return enif_make_badarg(env);
    }

    font = caca_load_font((void const *) buffer, 0);
    if(!font) {
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
    res->font = NULL;
    res->event = NULL;
    res->dither = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->font = font;

    if(buffer) free(buffer);
    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
get_font_width(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->font) {
        result = caca_get_font_width(res->font);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
get_font_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *res;
    int result = 0;
    ERL_NIF_TERM ret;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->font) {
        result = caca_get_font_height(res->font);
        ret = enif_make_int(env, result);
        return ret;
    }

    return mk_error(env, "error");
}

static ERL_NIF_TERM
free_font(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    if(res->font == NULL) {
        return enif_make_badarg(env);
    }

    // If it is not NULL, then free it.
    if(res->font) {
        caca_free_font(res->font);
        res->font = NULL;
        assert(res->font == NULL);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
render_canvas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CACA *can;
    CACA *font;
    ErlNifBinary buf = {0};
    int width = 0, height = 0, pitch = 0;
    int result = 0;

    if(argc != 6)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &can))
    {
        return enif_make_badarg(env);
    }

    if(can->canvas == NULL) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], RES_TYPE, (void **) &font))
    {
        return enif_make_badarg(env);
    }

    if(font->font == NULL) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[2], &buf))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &width))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[4], &height))
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[5], &pitch))
    {
        return enif_make_badarg(env);
    }

    if(can->canvas && font->font) {
        result = caca_render_canvas(can->canvas, font->font, buf.data, width, height, pitch);
        if(result < 0) {
             return mk_error(env, "function_error");
        }

        return mk_atom(env, "ok");
    }

    return mk_error(env, "error");
}

static ErlNifFunc nif_funcs[] = {
    {"caca_version", 0, caca_version},
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
    {"put_str", 4, put_str},
    {"get_attr", 3, get_attr},
    {"set_attr", 2, set_attr},
    {"unset_attr", 2, unset_attr},
    {"toggle_attr", 2, toggle_attr},
    {"put_attr", 4, put_attr},
    {"set_color_ansi", 3, set_color_ansi},
    {"set_color_argb", 3, set_color_argb},
    {"clear_canvas", 1, clear_canvas},
    {"set_canvas_handle", 3, set_canvas_handle},
    {"get_canvas_handle_x", 1, get_canvas_handle_x},
    {"get_canvas_handle_y", 1, get_canvas_handle_y},
    {"blit", 4, blit_4},
    {"blit", 5, blit_5},
    {"set_canvas_boundaries", 5, set_canvas_boundaries},
    {"invert", 1, invert},
    {"flip", 1, flip},
    {"flop", 1, flop},
    {"rotate_180", 1, rotate_180},
    {"rotate_left", 1, rotate_left},
    {"rotate_right", 1, rotate_right},
    {"stretch_left", 1, stretch_left},
    {"stretch_right", 1, stretch_right},
    {"draw_line", 6, draw_line},
    {"draw_thin_line", 5, draw_thin_line},
    {"draw_polyline", 4, draw_polyline},
    {"draw_thin_polyline", 3, draw_thin_polyline},
    {"draw_circle", 5, draw_circle},
    {"draw_ellipse", 6, draw_ellipse},
    {"draw_thin_ellipse", 5, draw_thin_ellipse},
    {"fill_ellipse", 6, fill_ellipse},
    {"draw_box", 6, draw_box},
    {"draw_thin_box", 5, draw_thin_box},
    {"draw_cp437_box", 5, draw_cp437_box},
    {"fill_box", 6, fill_box},
    {"draw_triangle", 8, draw_triangle},
    {"draw_thin_triangle", 7, draw_thin_triangle},
    {"fill_triangle", 8, fill_triangle},
    {"get_frame_count", 1, get_frame_count},
    {"set_frame", 2, set_frame},
    {"get_frame_name", 1, get_frame_name},
    {"set_frame_name", 2, set_frame_name},
    {"create_frame", 2, create_frame},
    {"free_frame", 2, free_frame},
    {"import_canvas_from_memory", 3, import_canvas_from_memory},
    {"import_canvas_from_file", 3, import_canvas_from_file},
    {"import_area_from_memory", 5, import_area_from_memory},
    {"import_area_from_file", 5, import_area_from_file},
    {"export_canvas_to_memory", 2, export_canvas_to_memory},
    {"export_area_to_memory", 6, export_area_to_memory},
    {"create_display", 0, create_display_0},
    {"create_display", 1, create_display},
    {"create_display_with_driver", 2, create_display_with_driver},
    {"free_display", 1, free_display},
    {"get_display_driver", 1, get_display_driver},
    {"set_display_driver", 2, set_display_driver},
    {"get_canvas", 1, get_canvas},
    {"refresh_display", 1, refresh_display},
    {"get_display_time", 1, get_display_time},
    {"set_display_time", 2, set_display_time},
    {"get_display_width", 1, get_display_width},
    {"get_display_height", 1, get_display_height},
    {"set_display_title", 2, set_display_title},
    {"set_mouse", 2, set_mouse},
    {"set_cursor", 2, set_cursor},
    {"create_event", 0, create_event},
    {"free_event", 1, free_event},
    {"get_event", 4, get_event, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"get_event_type", 1, get_event_type},
    {"get_event_key_ch", 1, get_event_key_ch},
    {"get_event_key_utf32", 1, get_event_key_utf32},
    {"get_event_key_utf8", 1, get_event_key_utf8},
    {"get_event_mouse_button", 1, get_event_mouse_button},
    {"get_event_mouse_x", 1, get_event_mouse_x},
    {"get_event_mouse_y", 1, get_event_mouse_y},
    {"get_event_resize_width", 1, get_event_resize_width},
    {"get_event_resize_height", 1, get_event_resize_height},
    {"create_dither", 8, create_dither},
    {"free_dither", 1, free_dither},
    {"load_font", 1, load_font},
    {"get_font_width", 1, get_font_width},
    {"get_font_height", 1, get_font_height},
    {"free_font", 1, free_font},
    {"render_canvas", 6, render_canvas},
};

ERL_NIF_INIT(caca, nif_funcs, &load, &reload, NULL, NULL)
