-module(alg_gossip).
-author("aadithya/anol").

%% API
-export([spawn_actors/1, neighbours_message_passing / 0, index_of/2]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).



spawn_actors(Counter)->
  receive {"Goss_Mess", Index, ListOfNeighbours, ListOfActors, Parent} ->
    Parent,
    if
      Counter == 1 ->
        io:format("Convergence achieved for ~p ~n", [self()]),
        convergence ! {"Actor_Convergence_Reached", self()},
        neighbours_message_passing ! {"Convergence_Reached_Cant_Acknowledge", "Dead", Parent, ListOfActors, ListOfNeighbours, self()},
        exit(normal);
      true ->
        neighbours_message_passing ! {Index, ListOfNeighbours, ListOfActors, self()},
        spawn_actors(Counter - 1)
    end
  end.

neighbours_message_passing() ->
  receive
    {Index, ListOfNeighbours, ListOfActors, PID} ->
      Neighbours = lists:nth(Index, ListOfNeighbours),
      Len = length(Neighbours),
      Random = rand:uniform(Len),
      Neighbour_Pid = lists:nth(Random, Neighbours),
      Is_Alive = is_process_alive(Neighbour_Pid),
      if
        Is_Alive == false ->
          self() ! {Index, ListOfNeighbours, ListOfActors, PID},
          neighbours_message_passing();
        true ->
          io:format("Message sent from ~p to ~p ~n", [Neighbour_Pid,PID]),
          Neighbour_Pid ! {"Goss_Mess", index_of(Neighbour_Pid, ListOfActors), ListOfNeighbours, ListOfActors, PID},
          self() ! {Index, ListOfNeighbours, ListOfActors, PID},
          neighbours_message_passing()
      end;

    {"Convergence_Reached_Cant_Acknowledge", Condition , PID, ListOfActors, ListOfNeighbours, Neighbour_Pid} ->
      Condition, Neighbour_Pid,
      self() ! {index_of(PID, ListOfActors), ListOfNeighbours, ListOfActors, PID},
      neighbours_message_passing()
  end.

