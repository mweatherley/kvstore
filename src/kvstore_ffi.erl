-module(kvstore_ffi).
-export([kvstore_new/1, kvstore_get/2, kvstore_contains/2, kvstore_insert/3, kvstore_insert_new/3, kvstore_delete/2, kvstore_drop/1]).

kvstore_new(Access) ->
    ets:new(kvstore_table, [set, Access]).

kvstore_get(Tid, Key) ->
    try
        case ets:lookup(Tid, Key) of
            [] -> {ok, none};
            [{_Key, Value}] ->
                {ok, {some, Value}}
        end
    catch error:badarg -> {error, nil}
    end.

kvstore_contains(Tid, Key) ->
    try
        IsInTable = ets:member(Tid, Key),
        {ok, IsInTable}
    catch error:badarg -> {error, nil}
    end.
        
kvstore_insert(Tid, Key, Value) ->
    try
        ets:insert(Tid, {Key, Value}),
        {ok, nil}
    catch error:badarg -> {error, nil}
    end.

kvstore_insert_new(Tid, Key, Value) ->
    try
        Inserted = ets:insert_new(Tid, {Key, Value}),
        {ok, Inserted}
    catch error:badarg -> {error, nil}
    end.

kvstore_delete(Tid, Key) ->
    try
        ets:delete(Tid, Key),
        {ok, nil}
    catch error:badarg -> {error, nil}
    end.

kvstore_drop(Tid) ->
    try
        ets:delete(Tid),
        {ok, nil}
    catch error:badarg -> {error, nil}
    end.
