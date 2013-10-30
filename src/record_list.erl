%%-------------------------------------------------------------------
%%% @author Mohammad El-Abid
%%% Created : 29. Oct 2013
%%%-------------------------------------------------------------------
-module(record_list).
-author("Mohammad El-Abid").
-include("../incl/records.hrl").
-export([create/2, open/1, debugRead/1, addRecord/2, search/3, search/2, deleteRecord/2]).

open(FileName) ->
  {ok, File} = file:open(FileName, [write, read, binary]),
  {ok, << FreePointer:64/integer >>} = file:read(File, 8),
  {ok, << DataPointer:64/integer >>} = file:read(File, 8),
  [{file, File}, {free_pointer, FreePointer}, {data_pointer, DataPointer}].

readRecs(File) ->
  Res = (catch info:read(File)),
  case is_record(Res, info) of
    true -> [Res] ++ readRecs(File);
    false -> []
  end.

debugRead(FileName) ->
  %fp, dp
  {ok, File} = file:open(FileName, [read, binary]),
  {ok, << FreePointer:64/integer >>} = file:read(File, 8),
  {ok, << DataPointer:64/integer >>} = file:read(File, 8),

  % return truple
  Recs = readRecs(File),
  file:close(File),
  [{free_pointer, FreePointer}, {data_pointer, DataPointer}, {blocks, Recs}].

search([{file, File}, {free_pointer, _}, {data_pointer, DataPointer}], AccountID) ->
  search(File, DataPointer, AccountID).

search(_, 0, _) -> {not_found, 0};
search(File, Offset, AccountID) ->
  file:position(File, Offset),
  Rec = info:read(File),
  case Rec#info.account == AccountID of
    true  -> {found, Offset};
    false -> search(File, Rec#info.prev, AccountID)
  end.

create(FileName, Size) ->
  % free pointer
  % data pointer
  {ok, File} = file:open(FileName, [read, write, binary]),

  FreePointer = 16,
  DataPointer = 0,

  file:write(File, << FreePointer:64/integer >>),
  file:write(File, << DataPointer:64/integer >>),

  helper:for(1,Size, fun(I) -> info:write(File, #info{
      account = 0,
      balance = 0,
      first_name = "",
      last_name = "",
      prev = case I of
               1 -> 0;
               _ -> (I-2) * (?INFO_SIZE + 1) + 16
             end,
      next = case I of
               Size -> 0;
               _ -> (I)*(?INFO_SIZE+1) + 16
             end
  }) end),
  [{file, File}, {free_pointer, FreePointer}, {data_pointer, DataPointer}].

deleteRecord([{file, File}, {free_pointer, FreePointer} | _], Offset) ->
  file:position(File, Offset),
  Info = info:read(File),

  % prev needs to point to next,
  case Info#info.prev of
    0 -> ok;
    _ ->
      file:position(File, Info#info.prev),
      PInfo = info:read(File),
      NPInfo = #info {
        prev = PInfo#info.prev, next = Info#info.next,
        account = PInfo#info.account, balance = PInfo#info.balance,
        first_name = PInfo#info.first_name, last_name = PInfo#info.last_name
      },
      file:position(File, Info#info.prev),
      info:write(File, NPInfo)
  end,

  % next needs to points to prev,
  case Info#info.next of
    0 -> ok;
    _ ->
      file:position(File, Info#info.next),
      NextInfo = info:read(File),
      NNInfo = #info {
        prev = Info#info.prev, next = NextInfo#info.next,
        account = NextInfo#info.account, balance = NextInfo#info.balance,
        first_name = NextInfo#info.first_name, last_name = NextInfo#info.last_name
      },
      file:position(File, Info#info.next),
      info:write(File, NNInfo)
  end,

  % Offset's next points to FP
  file:position(File, Offset),
  NInfo = #info{
    prev = 0, next = FreePointer,
    balance = 0,  account = 0,
    first_name = "", last_name = ""
  },
  file:position(File, Offset),
  info:write(File, NInfo),
  % FP needs to point to Offset
  file:position(File, 0),
  file:write(File, << Offset:64/integer >>).

addRecord([{file, File}, {free_pointer, FreePointer}, {data_pointer, DataPointer}], Record) ->
  case FreePointer of
    0 -> {error, no_location};
    _ ->
      file:position(File, FreePointer),
      Read = info:read(File),
      Rec = #info{
        account = Record#info.account,
        balance = Record#info.balance,
        first_name = Record#info.first_name,
        last_name = Record#info.last_name,
        prev = DataPointer,
        next = 0
      },
      % write Rec at FreePointer,
      file:position(File, FreePointer),
      info:write(File, Rec),

      % notify the new free stack top that there is no prev
      file:position(File, Read#info.next),
      NewTop = info:read(File),
      file:position(File, Read#info.next),
      info:write(File, #info{
        prev = 0, next = NewTop#info.next,
        account = NewTop#info.account, balance = NewTop#info.balance,
        first_name = NewTop#info.first_name, last_name = NewTop#info.last_name
      }),

      % goto Rec#info.prev and change it's next to FreePointer
      case DataPointer /= 0 of
        true ->
          file:position(File, DataPointer),
          Original = info:read(File),
          Mutated = #info{
            account = Original#info.account,
            balance = Original#info.balance,
            first_name = Original#info.first_name,
            last_name = Original#info.last_name,
            prev = Original#info.prev,
            next = FreePointer
          },
          file:position(File, DataPointer),
          info:write(File, Mutated);
        false -> ok
      end,
      % return an updated Options with FreePointer corrected
      file:position(File, 0),
      file:write(File, << (Read#info.next):64/integer >>),
      file:write(File, << FreePointer:64/integer >>),
      {ok, [{file, File}, {data_pointer, FreePointer}, {free_pointer, (Read#info.next)}]}
  end.