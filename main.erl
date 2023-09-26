-module(main).
-import(lists,[nth/2]).
-export([run/2, trackHops/1]).

run(NumNodes, NumRequests) ->
    io:fwrite("Received request for creating - ~nNodes: ~p and ~nRequests: ~p. ~nCreating ring...~n",[NumNodes, NumRequests]),
    Node1 = node:start(generate()),
    Arr = createNodes([Node1], NumNodes-1, Node1),
    timer:sleep(1000),
    HopsPid = spawn(main, trackHops, [0]),
    io:fwrite("Running Chord.... ~nAdding and performing lookup on all keys...~n"),
    addData(Arr, NumRequests, HopsPid).

generate() ->
    rand:uniform(1000000000).

addData([], NumRequests, HopsPid) -> 
    ok;

addData([H|T], NumRequests, HopsPid) ->
    spawn(fun() -> function(H,NumRequests, HopsPid) end),
    addData(T, NumRequests, HopsPid).

createNodes(Arr, 0, Node) ->
    io:fwrite("~p~n",[Arr]),
    Arr;

createNodes(Arr, N, Node) ->
    NodeNew = node:start(generate(),Node),
    createNodes([NodeNew|Arr], N-1, NodeNew).

function(Node, N, HopsPid) -> 
    add_then_lookup(Node, N, HopsPid).

add_then_lookup(Node,Nb, HopsPid) ->
    Tuples = [{generate(),N} || N <- lists:seq(1, Nb)],
    lists:foreach(fun({Key,Value}) ->
                          Node ! {add, Key, Value, Key, self()}
                          end,Tuples),
    T1 = erlang:timestamp(),
        lists:foreach(fun({Key,Value}) ->
                          Node ! {lookup, 0, HopsPid, Key, Key, self()}
                          end, Tuples),
     T2 = erlang:timestamp(),
     Duration = timer:now_diff(T2, T1)/1000000,
     io:format("Duration : ~p~n",[Duration]).

trackHops(Avg) ->
    receive
        {addHop, N} ->
            NewAvvg = (Avg+N)/2,
            %io:fwrite("Average number of Hops is : ~p~n",[NewAvvg]),
            trackHops(NewAvvg)
        after 3000 ->
            io:fwrite("Average number of Hops is ~.2f~n",[Avg])
    end.