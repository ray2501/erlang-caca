#include "erl_nif.h"
#include <assert.h>
#include <caca.h>

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);

static ErlNifResourceType* RES_TYPE;

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

static ErlNifFunc nif_funcs[] = {
    {"get_display_driver_list", 0, get_display_driver_list},
    {"get_export_list", 0, get_export_list},
    {"get_import_list", 0, get_import_list},
    {"get_font_list", 0, get_font_list},
};

ERL_NIF_INIT(caca, nif_funcs, &load, &reload, NULL, NULL)
