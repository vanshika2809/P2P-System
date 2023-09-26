-module(storage).
-export([add/3,create/0,lookup/2,split/3,merge/2]).

create() ->
    [].

add(Key,Value,Storage) ->
    %io:fwrite("~p~n",[Storage]),
    [{Key,Value}|Storage].

lookup(Key,Storage) ->
    lists:keyfind(Key, 1, Storage).

split(LocalKey, Pkey, Storage) ->
    {Keep, Give} = lists:foldl(fun({Key,Value},{AccSplit1,AccSplit2}) ->
                                           case between(Key, Pkey, LocalKey) of
                                               true ->
                                                   %% keep for local node
                                                   {[{Key,Value}|AccSplit1],AccSplit2};
                                               false ->
                                                   {AccSplit1,[{Key,Value}|AccSplit2]}
                                           end
                                           end, {[],[]}, Storage).

merge(Storage1,Storage2) ->
    Storage1 ++ Storage2.

between(Key,From,To) ->
    if
        From<To ->
            (Key>From) and (Key=<To);
        From>To ->
            (Key>From) or (Key=<To);
        true ->
            true
    end.




