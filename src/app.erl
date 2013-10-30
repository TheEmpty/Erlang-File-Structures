%%%-------------------------------------------------------------------
%%% @author Mohammad El-Abid
%%% Created : 29. Oct 2013
%%%-------------------------------------------------------------------
-module(app).
-author("Mohammad El-Abid").
-compile(export_all).
-include("../incl/records.hrl").
-define(FILE_NAME, "app_list.data").

about() ->
  io:fwrite(
    "Developed by Mohammad El-Abid (mohammad.el-abid.com).~n"
    "Developed for CS201 - File Structures.~n"
  ), ok.

options() ->
  io:fwrite(
    "Welcome to (A) (FI)l(E) (ST)ructures (A)lgorthim (A FIESTA)~n"
    "~n"
    "You can write the following commands, (period intended)~n"
    "  * app:about().~n"
    "  * app:create_file().~n"
    "  * app:data_dump().~n"
    "  * app:data().~n"
    "  * app:create().~n"
    "  * app:search().~n"
    "  * app:edit().~n"
    "  * app:delete().~n"
    "  * app:options(). % this menu~n"
    "  * app:quit().~n"
  ), ok.

help() -> options().
exit() -> quit().
quit() -> halt().
data_dump() -> record_list:debugRead(?FILE_NAME).

create_file() ->
  {ok, [Size]} = io:fread("Number of Records: ", "~d"),
  io:fwrite("Create file `~p` with ~p records~n", [?FILE_NAME, Size]),
  [{file, File} | _] =  record_list:create(?FILE_NAME, Size),
  file:close(File), ok.

display_record(_, 0) -> ok;
display_record(File, Offset) ->
  file:position(File, Offset),
  Info = info:read(File),
  io:fwrite("At ~p, ~p~n", [Offset, Info]),
  display_record(File, Info#info.prev). % prev because it's a stack

data() ->
  [{file, File}, {free_pointer, _}, {data_pointer, DataPointer}] = record_list:open(?FILE_NAME),
  display_record(File, DataPointer),
  file:close(File), ok.

create() ->
  {ok, [Account]}   = io:fread("Account Number: ", "~d"),
  {ok, [Balance]}   = io:fread("Account Balance: ", "~f"),
  {ok, [FirstName]} = io:fread("First Name: ", "~s"),
  {ok, [LastName]}  = io:fread("Last Name: ", "~s"),

  Options = record_list:open(?FILE_NAME),
  Result  = record_list:addRecord(Options, #info{
    account = Account, balance = Balance,
    first_name = FirstName, last_name = LastName
  }),
  [{file, File} | _] = Options,
  file:close(File),
  Result.

search() ->
  {ok, [Account]} = io:fread("Account Number: ", "~d"),
  Options = record_list:open(?FILE_NAME),
  Results = record_list:search(Options, Account),
  [{file, File} | _] = Options,
  file:close(File),
  Results.

delete() ->
  case search() of
    {not_found, _} -> io:fwrite("Record not found.~n");
    {found, Offset} ->
      Options = record_list:open(?FILE_NAME),
      record_list:deleteRecord(Options, Offset),
      [{file, File} | _] = Options,
      file:close(File)
  end, ok.

edit() ->
  case search() of
    {not_found, _} -> io:fwrite("Record not found.~n");
    {found, Offset} ->
      [{file, File} | _] = record_list:open(?FILE_NAME),
      file:position(File, Offset),
      Original = info:read(File),

      io:fwrite("Account Number(~p): ", [Original#info.account]),
      {ok, [Account]} = io:fread("", "~d"),
      io:fwrite("Account Balance(~p): ", [Original#info.balance]),
      {ok, [Balance]} = io:fread("", "~f"),
      io:fwrite("First Name(~p): ", [Original#info.first_name]),
      {ok, [FirstName]} = io:fread("", "~s"),
      io:fwrite("Last Name(~p): ", [Original#info.last_name]),
      {ok, [LastName]} = io:fread("", "~s"),

      Info = #info{
        prev = Original#info.prev, next = Original#info.next,
        account = Account, balance = Balance,
        first_name = FirstName, last_name = LastName
      },

      file:position(File, Offset),
      info:write(File, Info),
      file:close(File)
  end, ok.