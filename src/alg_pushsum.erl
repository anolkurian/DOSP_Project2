-module(alg_pushsum).
-author("aadithya/anol").

-export([spawn_actors/3, neighbours_message_passing / 0, index_of/2]).

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

spawn_actors(S, W, Counter)->
    receive {"Push_Sum", Index, ListOfNeighbours, ListOfActors, Parent, Received_S, Received_W} ->
        if
           Counter > 3 ->
               io:format("Convergence Achieved for ~p ~n", [self()]),
               convergence ! {"Actor_Convergence_Reached", self()},
               neighbours_message_passing ! {"Convergence_Reached_Cant_Acknowledge", "Dead", Parent, ListOfActors, ListOfNeighbours, self(), Received_S, Received_W},
               exit(normal);
            true ->
                Initial_Ratio = S / W,
                io:format("Initial Ratio ~p ~n", [Initial_Ratio]),
                New_S = S + Received_S,
                New_W = W + Received_W,
                New_Ratio = New_S / New_W,
                io:format("Updated Ratio ~p ~n", [New_Ratio]),
                Diff = abs(New_Ratio - Initial_Ratio),
                Pow = math:pow(10, -2),
                if
                    Diff < Pow ->
                        NewCounter = Counter + 1;
                    true -> NewCounter = 0
                end,

                neighbours_message_passing ! {Index, ListOfNeighbours, ListOfActors, self(), New_S / 2, New_W / 2},
                spawn_actors(New_S / 2, New_W / 2, NewCounter)
        end

    end.


neighbours_message_passing() ->
    receive
        {Index, ListOfNeighbours, ListOfActors, PID, S, W} ->
            Neighbours = lists:nth(Index, ListOfNeighbours),
            Len = length(Neighbours),
            Random = rand:uniform(Len),
            Neighbour_Pid = lists:nth(Random, Neighbours),
            Is_Alive = is_process_alive(Neighbour_Pid),
            if
                Is_Alive == false ->
                    self() ! {Index, ListOfNeighbours, ListOfActors, PID, S, W},
                    neighbours_message_passing();
                true ->
                    io:format("Message from ~p to ~p ~n", [PID, Neighbour_Pid]),
                    Neighbour_Pid ! {"Push_Sum", index_of(Neighbour_Pid, ListOfActors), ListOfNeighbours, ListOfActors, PID, S, W},
                    self() ! {Index, ListOfNeighbours, ListOfActors, PID, S, W},
                    neighbours_message_passing()
            end;
        {"Convergence_Reached_Cant_Acknowledge", Condition , PID, ListOfActors, ListOfNeighbours, Neighbour_Pid, S, W} ->
            Condition, Neighbour_Pid,
            self() ! {index_of(PID, ListOfActors), ListOfNeighbours, ListOfActors, PID, S, W},
            neighbours_message_passing()

    end.

