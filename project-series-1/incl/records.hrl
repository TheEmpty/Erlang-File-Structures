%%%-------------------------------------------------------------------
%%% @author Mohammad El-Abid
%%% Created : 29. Oct 2013
%%%-------------------------------------------------------------------
-define(INFO_SIZE, 62).
-record(info, {
  prev,          %  64 bit
  next,          %  64 bit
  account,       %  64 bit
  balance,       %  64 bit
  first_name,    % 120 bit
  last_name      % 120 bit
                 % -------
                 % 496 bit -> 62 bytes
}).