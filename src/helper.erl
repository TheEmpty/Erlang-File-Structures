%%%-------------------------------------------------------------------
%%% @author Mohammad El-Abid
%%% Created : 29. Oct 2013
%%%-------------------------------------------------------------------
-module(helper).
-author("Mohammad El-Abid").
-compile(export_all).

for(Max, Max, Fun)   -> [Fun(Max)];
for(Count, Max, Fun) -> [Fun(Count) | for(Count + 1, Max, Fun)].

%% @doc Write a string right padded with spaces. Each
%% character is saved as 8bits.
write_string(_, _, 0) -> {complete}; % No more Length
write_string(File, [], Length) ->
  file:write(File, << ($\s):8/integer >>),
  write_string(File, [], Length - 1);
write_string(File, [Head | Tail], Length) ->
  file:write(File, << (Head):8/integer >>),
  write_string(File, Tail, Length - 1).

%% @doc Reads a string assuming each character is 8 bits.
load_string(_, 0) -> []; % Base case, no Length left
load_string(File, Length) ->
  {ok, << Int:8/integer >>} = file:read(File, 1),
  [Int] ++ load_string(File, Length - 1).