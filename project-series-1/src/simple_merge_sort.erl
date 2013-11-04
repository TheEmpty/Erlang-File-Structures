-module(simple_merge_sort).
-compile(export_all).
-include("../incl/records.hrl").
-define(FILE_NAME, "merge.data").
-define(HALF_SIZE, 25).

generate_record() ->
  #info{
    account = random:uniform(999),
    balance = random:uniform(9999),
    first_name = "Generated",
    last_name  = "Record"
  }.

generate_records(_, 0) -> ok;
generate_records(File, Left) ->
  record_list:add_record(File, generate_record()),
  generate_records(File, Left - 1).

demo() ->
  Main = record_list:create(?FILE_NAME, ?HALF_SIZE * 2),
  [{file, File} | _] = Main,
  generate_records(File, ?HALF_SIZE * 2),
  file:close(File),
  merge(?FILE_NAME, ?HALF_SIZE*2),
  record_list:debug_read(?FILE_NAME).

split(FileName, Size) ->
  Main = record_list:open(FileName),
  [{file, File} | _] = Main,
  {ok, 16} = file:position(File, 16),

  A = record_list:create(FileName ++ ".a", round(Size/2)),
  [{file, FileA} | _] = A,
  helper:for(1, round(Size/2), fun(_) ->
        Read = info:read(File),
        {ok, _} = record_list:add_record( FileA, Read )
    end),
  file:close(FileA),

  B = record_list:create(FileName ++ ".b", round(Size/2)),
  [{file, FileB} | _] = B,
  helper:for(round(Size/2) + 1, Size, fun(_) ->
        Read = info:read(File),
        {ok, _} = record_list:add_record( FileB, Read )
    end),
  file:close(FileB),
  file:close(File), ok.

merge(MainName, Size) ->
  split(MainName, Size),
  Main = record_list:create(MainName, Size),
  [{file, FileM} | _] = Main,
  A = record_list:open(MainName ++ ".a"),
  B = record_list:open(MainName ++ ".b"),
  [{file, FileA} | _] = A,
  [{file, FileB} | _] = B,
  {ok, _} = file:position(FileA, 16),
  {ok, _} = file:position(FileB, 16),
  merge(FileM, FileA, FileB, round(Size/2), round(Size/2)),
  file:close(FileA),
  file:close(FileB),
  file:close(FileM),
  file:delete(MainName ++ ".a"),
  file:delete(MainName ++ ".b"),
  ok.

merge(_, _, _, 0, 0) -> [];
% All A gone
merge(FileM, _, FileB, 0, BLeft) ->
  % safe to assume all are written, but there is a chance get bad record
  {ok, _} = record_list:add_record(FileM, info:read(FileB)),
  merge(FileM, none, FileB, 0, BLeft - 1);

% All B gone
merge(FileM, FileA, _, ALeft, 0) ->
  {ok, _} = record_list:add_record(FileM, info:read(FileA)),
  merge(FileM, FileA, none, ALeft - 1, 0);

% A and B have more
merge(FileM, FileA, FileB, ALeft, BLeft) ->
  {ok, PrevA} = file:position(FileA, cur),
  {ok, PrevB} = file:position(FileB, cur),
  RecordA = info:read(FileA),
  RecordB = info:read(FileB),
  
  case RecordA#info.account > RecordB#info.account of
    true ->
      {ok, _} = record_list:add_record(FileM, RecordB),
      {ok, _} = file:position(FileA, PrevA),
      merge(FileM, FileA, FileB, ALeft, BLeft - 1);
    false ->
      {ok, _} = record_list:add_record(FileM, RecordA),
      {ok, _} =  file:position(FileB, PrevB),
      merge(FileM, FileA, FileB, ALeft - 1, BLeft)
  end.
