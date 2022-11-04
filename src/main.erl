-module(main).
-author("aadithya/anol").

%% API
-export([start/0]).
-export([create_topology/3,  grid_view/2, grid_view/4, imperfect_view/2, imperfect_view/5 ,check_convergence/2]).

start() ->
  {_, [Actor]} = io:fread("Enter the number of Nodes: ", "~d"),
  {_, [Topology]} = io:fread("Select the type of topology (line / 2d / imperfect3d / full): ", "~s"),
  {_, [Algorithm]} = io:fread("Select the algorithm to be used (gossip / pushsum): ", "~s"),
  io:format("No. of Nodes: ~w, Topology Selected: ~p, Algorithm Selected: ~p ~n", [Actor, Topology, Algorithm]),

  Convergence_Pid = spawn(?MODULE, check_convergence, [Actor, erlang:system_time(millisecond)]),
  register(convergence, Convergence_Pid),

  if
    Algorithm  == "gossip"->
      io:fwrite("Starting Gossip ~n"),
      ListOfActors = [spawn(alg_gossip, spawn_actors, [10]) || _ <- lists:seq(1, Actor)];

    Algorithm == "pushsum" ->
      io:fwrite("Starting PushSum ~n"),
      ListOfActors = [spawn(alg_pushsum, spawn_actors, [S, 1, 0]) || S <- lists:seq(1, Actor)];

    true -> io:format("Invalid Selection"), erlang:halt(0), ListOfActors =[]
  end,

  ListOfNeighbours = create_topology(Actor, Topology, ListOfActors),

  Index = rand:uniform(Actor),

  if
    Algorithm  == "gossip"->
      Initial_Gossip_Pid = lists:nth(Index, ListOfActors),
      Pass_to_Neighbours_Pid = spawn(alg_gossip, neighbours_message_passing, []),
      register(neighbours_message_passing, Pass_to_Neighbours_Pid),
      Initial_Gossip_Pid ! {"Goss_Mess", Index, ListOfNeighbours, ListOfActors, self()};

    Algorithm == "pushsum" ->
      Initial_Path_Sum_Pid = lists:nth(Index, ListOfActors),
      Pass_to_Neighbours_Pid = spawn(alg_pushsum, neighbours_message_passing, []),
      register(neighbours_message_passing, Pass_to_Neighbours_Pid),
      Initial_Path_Sum_Pid ! {"Push_Sum", Index, ListOfNeighbours, ListOfActors, self(), 0, 0};

    true -> io:format("Invalid Selection"), erlang:halt(0)
  end.

check_convergence(0, Start_Time) ->
  End_Time = erlang:system_time(millisecond),
  io:format("Server is alive ~p and PID ~p ~n", [is_process_alive(self()), self()]),
  io:format("Start Time is ~w ~n", [Start_Time]),
  io:format("End Time is ~w ~n", [End_Time]),
  io:format("Convergence time is ~w milliseconds  ~n", [End_Time - Start_Time]),
  erlang:halt(0);

check_convergence(Actor, Start_Time) ->
  receive {"Actor_Convergence_Reached", Pid} ->
    Pid,
    check_convergence(Actor - 1, Start_Time)
  end.

create_topology(Actors, Topology, ListOfActors) ->
  Neighbour_List = generate_neighbours(ListOfActors, Topology, Actors),
  io:format("Actor list => ~p ~n Neighbor list for actors => ~p ~n", [ListOfActors, Neighbour_List]),
  Neighbour_List.

generate_neighbours(ListOfActors, Topology, Actors) ->
  io:format("Create neighbours for Actors ~p for Topology ~p ~n", [Actors, Topology]),
  if
    Topology == "full" ->
      Neighbours = full_network(Actors, ListOfActors);

    Topology == "line" ->
      Neighbours = line(Actors, ListOfActors);

    Topology == "2d" ->
      Neighbours = grid_view(Actors, ListOfActors);

    Topology == "imperfect3d" ->
      Neighbours = imperfect_view(Actors, ListOfActors);
    true -> invalid_topology, Neighbours = []
  end,
  Neighbours.


%%%% Construct Neighbours for all topologies

%%% Grid Representation
grid_generation(Idx, Rows, Actors, ListOfActors, RowElement, Matrix) ->

  if
    Idx > Actors -> Matrix;
    true ->
      Element = lists:nth(Idx, ListOfActors),
      Temporary = lists:append(RowElement, [Element]),
      if
        Idx rem Rows == 0 ->
          grid_generation(Idx + 1, Rows, Actors, ListOfActors, [], lists:append(Matrix, [Temporary]));
        true ->
          grid_generation(Idx + 1, Rows, Actors, ListOfActors, Temporary, Matrix)
      end
  end.

%%% Grid View

grid_view(Actors , ListOfActors) ->
  Rows =  round(math:sqrt(Actors)),
  Grid_Matrix =  grid_generation(1, Rows, Actors, ListOfActors, [], []),
  grid_view(Actors, Grid_Matrix, Rows, []).


grid_view(0, Grid_Matrix, Rows, Neighbours) ->
  Grid_Matrix, Rows,
  lists:reverse(Neighbours);

grid_view(Index, Grid_Matrix, Rows, Neighbours) ->

  Grid_Matrix, Neighbours,
  if
    Index rem Rows == 0 ->
      Element_col = Rows,
      Element_rows  = round(Index / Rows);
    true ->
      Element_col = Index rem Rows,
      Element_rows = trunc(math:floor((Index / Rows))) + 1
  end,
  if
    Element_rows == 1 ->
      if
        Element_col == 1 ->
          Neigh1= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh2= lists:nth(Element_col, lists:nth(Element_rows + 1, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2]]));
        Element_col == Rows ->
          Neigh1= lists:nth(Element_col, lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col - 1, lists:nth(Element_rows , Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2]]));
        true ->
          Neigh1 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh2= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col, lists:nth(Element_rows + 1, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3]]))
      end;
    Element_rows == Rows ->
      if
        Element_col == 1 ->
          Neigh1= lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2]]));
        Element_col == Rows ->
          Neigh1= lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2]]));
        true ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3]]))
      end;
    true ->
      if
        Element_col == 1 ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col , lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3]]));
        Element_col == Rows ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col , lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3]]));
        true ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col , lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh4 = lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3, Neigh4]]))
      end
  end.


%%% imperfect3D

imperfect_view(Actors , ListOfActors) ->
  Rows =  round(math:sqrt(Actors)),
  Grid_Matrix =  grid_generation(1, Rows, Actors, ListOfActors, [], []),
  imperfect_view(Actors, Grid_Matrix, Rows, [], ListOfActors).


imperfect_view(0, Grid_Matrix, Rows, Neighbours, ListOfActors) ->
  Grid_Matrix, Rows, ListOfActors,
  lists:reverse(Neighbours);

imperfect_view(Index, Grid_Matrix, Rows, Neighbours, ListOfActors) ->

  Grid_Matrix, Neighbours,
  if
    Index rem Rows == 0 ->
      Element_col = Rows,
      Element_rows  = round(Index / Rows);
    true ->
      Element_col = Index rem Rows,
      Element_rows = trunc(math:floor((Index / Rows))) + 1
  end,
  Element =  lists:nth(Element_col, lists:nth(Element_rows, Grid_Matrix)),
  if
    Element_rows == 1 ->
      if
        Element_col == 1 ->
          Neigh1= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh2= lists:nth(Element_col, lists:nth(Element_rows + 1, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2,lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ListOfActors);
        Element_col == Rows ->
          Neigh1= lists:nth(Element_col, lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col - 1, lists:nth(Element_rows , Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ListOfActors);
        true ->
          Neigh1 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh2= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col, lists:nth(Element_rows + 1, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Neigh3, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ListOfActors)
      end;
    Element_rows == Rows ->
      if
        Element_col == 1 ->
          Neigh1= lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ListOfActors);
        Element_col == Rows ->
          Neigh1= lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ListOfActors);
        true ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Neigh3, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ListOfActors)
      end;
    true ->
      if
        Element_col == 1 ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col , lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Neigh3, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3,  lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ListOfActors);
        Element_col == Rows ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col , lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Neigh3, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3,  lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ListOfActors);
        true ->
          Neigh1 = lists:nth(Element_col, lists:nth(Element_rows - 1, Grid_Matrix)),
          Neigh2= lists:nth(Element_col , lists:nth(Element_rows + 1, Grid_Matrix)),
          Neigh3 = lists:nth(Element_col - 1, lists:nth(Element_rows, Grid_Matrix)),
          Neigh4 = lists:nth(Element_col + 1, lists:nth(Element_rows, Grid_Matrix)),
          Rem_List = ListOfActors -- [Neigh1, Neigh2, Neigh3, Neigh4, Element],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[Neigh1, Neigh2, Neigh3, Neigh4,  lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ListOfActors)
      end
  end.

%%% Full

full_network(Actors, ListOfActors) ->
  full_network(Actors, ListOfActors, []).

full_network(0, ListOfActors, Neighbours) ->
  ListOfActors,
  lists:reverse(Neighbours);

full_network(Index, ListOfActors, Neighbours) ->
  ActorNeighbours = ListOfActors -- [lists:nth(Index, ListOfActors)],
  full_network(Index - 1, ListOfActors, lists:append(Neighbours, [ActorNeighbours])).

%%% Line Neighbours

line(Actors, ListOfActors) ->
  line(Actors, Actors, ListOfActors, []).

line(0, Actors, ListOfActors, Neighbours) ->
  ListOfActors, Actors,
  lists:reverse(Neighbours);

line(Index, Actors, ListOfActors, Neighbours) ->
  if
    Index == 1 ->
      Next_Ele = lists:nth(Index + 1, ListOfActors),
      line(Index - 1, Actors, ListOfActors, lists:append(Neighbours, [[Next_Ele]]));

    Index == Actors ->
      io:format("Index ~p, Actors ~p, Length ~p Neighbours ~p ~n", [Index, Actors, length(ListOfActors), Neighbours]),
      Prev_Ele = lists:nth(Index - 1, ListOfActors),
      line(Index - 1, Actors, ListOfActors, lists:append(Neighbours, [[Prev_Ele]]));
    true ->
      Next_Ele = lists:nth(Index + 1, ListOfActors),
      Prev_Ele = lists:nth(Index - 1, ListOfActors),
      line(Index - 1, Actors, ListOfActors, lists:append(Neighbours, [[Prev_Ele, Next_Ele]]))
  end.






