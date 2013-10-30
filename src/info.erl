%%-------------------------------------------------------------------
%%% @author Mohammad El-Abid
%%% Created : 29. Oct 2013
%%%-------------------------------------------------------------------
-module(info).
-author("Mohammad El-Abid").
-include("../incl/records.hrl").
-define(STRING_LENGTH, 15).
-define(VERSION_NUMBER, 1).
-export([write/2, test/0, read/1, sizeTest/0]).

% Prev, Next, Account ID, Balance, First Name, Last Name
%% @doc Writes a #info to a file (wherever it is currently seek'd).
write(File, Record) ->
  file:write(File, << ?VERSION_NUMBER:8/integer >>),
  file:write(File, << (Record#info.prev):64/integer >>),
  file:write(File, << (Record#info.next):64/integer >>),
  file:write(File, << (Record#info.account):64/integer >>),
  file:write(File, << (Record#info.balance):64/float >>),
  helper:write_string(File, Record#info.first_name, ?STRING_LENGTH),
  helper:write_string(File, Record#info.last_name,  ?STRING_LENGTH),
  {ok}.

%% @doc Loads a #info that was created with the write method above,
%% note that the File passed should be seek'd to the start of the record.
read(File) ->
  {ok, << ?VERSION_NUMBER:8/integer >>} = file:read(File, 1),
  {ok, << Prev:64/integer >>} = file:read(File, 8),
  {ok, << Next:64/integer >>} = file:read(File, 8),
  {ok, << Account:64/integer >>} = file:read(File, 8),
  {ok, << Balance:64/float >>} = file:read(File, 8),
  FirstName = helper:load_string(File, ?STRING_LENGTH),
  LastName  = helper:load_string(File, ?STRING_LENGTH),

  #info{
   account = Account,
   balance = Balance,
   first_name = string:strip(FirstName),
   last_name  = string:strip(LastName),
   prev = Prev,
   next = Next
  }.

















sizeTest()->
  {ok, FileOne} = file:open("1.dat", [read, write, binary]),
  Rec = #info{
    account = 0, balance = 0,
    first_name = "", last_name = "",
    prev = 0, next = 0
  },
  info:write(FileOne, Rec),
  file:close(FileOne),

  {ok, FileFive} = file:open("5.dat", [write, binary]),
  helper:for(1, 5, fun(_) -> info:write(FileFive, Rec) end),
  file:close(FileFive),

  {ok, FileTen} = file:open("10.dat", [write, binary]),
  helper:for(1, 10, fun(_) -> info:write(FileTen, Rec) end),
  file:close(FileTen).

test() ->
  {ok, File} = file:open("test.dat", [write, binary]),
  Rec = #info{
    first_name = "Mohammad",
    last_name  = "El-Abid",
    account    = 1337,
    balance    = 5000,
    prev = 0,
    next = 0
  },
  write(File, Rec),
  io:fwrite("Wrote: ~p~n", [Rec]),
  file:close(File),

  {ok, File2} = file:open("test.dat", [read, binary]),
  Rec2 = read(File2),
  io:fwrite("Read:  ~p~n", [Rec2]).