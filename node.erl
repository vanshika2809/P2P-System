-module(node).
-define(Stabilize,100).
-define(Timeout,1000).
-export([start/1,start/2]).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor,storage:create()).

connect(Id, nil) ->
    {ok, {Id,self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
               {ok,{Skey,Peer}}
    after ?Timeout ->
            io:format("Time out: no response~n")
    end.

node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->
            {Pred,NewStore} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, NewStore);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
        
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        
        {lookup, Hops, HopsPid, Key, Qref, Client} ->
            lookup(Key, Hops, HopsPid, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);
        
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
        StrangeMessage ->
            io:format("strange message : ~s~n",[StrangeMessage])
end.

add(Key, Value, Qref, Client, Id, {Pkey,Ppid}, {Skey,Spid}, Store) ->
    case between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Hops, HopsPid, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            %io:fwrite("Key : ~p, result : ~p, client : ~p~n",[Key, Result, Client]),
            %io:fwrite("Hops taken : ~p and hops pid is ~p~n",[Hops, HopsPid]),
            HopsPid ! {addHop, Hops},
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Hops+1, HopsPid, Key, Qref, Client}
    end.
    

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} -> 
            Successor;
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            case between(Xkey, Id, Skey) of
                true -> 
                    Xpid ! {request, self()},
                    Pred;
                false ->
                    Spid ! {notify,{Id,self()}},
                    Successor
            end
     end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Store, Nkey, Npid, Id),
            {{Nkey, Npid},Keep};
        {Pkey,  _} ->
            case between(Nkey, Pkey, Id) of
                true -> 
                    Keep = handover(Store, Nkey, Npid, Id),
                    {{Nkey, Npid},Keep};
                false -> 
                    {Predecessor,Store}
            end
    end.

handover(Store, Nkey, Npid, Id) ->
    {Keep, Leave} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

create_probe(Id,{Skey,Spid}) ->
    Spid ! {probe,Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
    Duration = timer:now_diff(erlang:now(),T),
    Printer = fun(E) -> io:format("~p ",[E]) end, 
    lists:foreach(Printer,Nodes),
    io:format("~n Time = ~p",[Duration]).

forward_probe(Ref, T, Nodes, Id, {Skey,Spid}) ->
    Spid ! {probe,Ref,Nodes ++ [Id],T}.

between(Key,From,To) ->
    if
        From<To ->
            (Key>From) and (Key=<To);
        From>To ->
            (Key>From) or (Key=<To);
        true ->
            true
    end.
