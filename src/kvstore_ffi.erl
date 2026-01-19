-module(kvstore_ffi).
-export([kvstore_new/1, kvstore_get/2, kvstore_contains/2, kvstore_insert/3, kvstore_insert_new/3, kvstore_delete/2, kvstore_clear/1, kvstore_drop/1]).

kvstore_new(Options) ->
    % Name is unused here; this will never be a named table.
    ets:new(kvstore_table, Options).

kvstore_get(Table, Key) ->
    try
        case ets:lookup(Table, Key) of
            [] -> {ok, none};
            [{_Key, Value}] ->
                {ok, {some, Value}}
        end
    catch error:badarg -> {error, nil}
    end.

kvstore_contains(Table, Key) ->
    try
        IsInTable = ets:member(Table, Key),
        {ok, IsInTable}
    catch error:badarg -> {error, nil}
    end.
        
kvstore_insert(Table, Key, Value) ->
    try
        ets:insert(Table, {Key, Value}),
        {ok, nil}
    catch error:badarg -> {error, nil}
    end.

kvstore_insert_new(Table, Key, Value) ->
    try
        Inserted = ets:insert_new(Table, {Key, Value}),
        {ok, Inserted}
    catch error:badarg -> {error, nil}
    end.

kvstore_delete(Table, Key) ->
    try
        ets:delete(Table, Key),
        {ok, nil}
    catch error:badarg -> {error, nil}
    end.

kvstore_clear(Table) ->
    try
        ets:delete_all_objects(Table),
        {ok, nil}
    catch error:badarg -> {error, nil}
    end.

kvstore_drop(Table) ->
    try
        ets:delete(Table),
        {ok, nil}
    catch error:badarg -> {error, nil}
    end.
