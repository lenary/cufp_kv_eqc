%% This module contains a QuickCheck state machine specification for kv.erl

-module(kv_eqc).

-include_lib("eqc/include/eqc.hrl").
-compile({parse_transform,eqc_cover}).

-include_lib("eqc/include/eqc_statem.hrl").


-compile(export_all).

%% -- State ------------------------------------------------------------------
%% We model the Key-Value store with a list of tuples
initial_state() ->
  [].

%% -- Generators -------------------------------------------------------------
key() ->
  nat().

val() ->
  nat().

%% -- Operations -------------------------------------------------------------

%% --- Operation: insert ---
insert_args(_S) ->
  [key(), val()].

insert(Key, Val) ->
  kv:insert(Key, Val).

insert_next(S, _Value, [Key, Val]) ->
  lists:keystore(Key, 1, S, {Key, Val}).

%% --- Operation: lookup ---
lookup_args(_S) ->
  [key()].

lookup(Key) ->
  kv:lookup(Key).

lookup_post(S, [Key], Res) ->
  eq(Res, lists:keyfind(Key, 1, S)).

%% --- Operation: delete ---
%% delete_args(_S) ->
%%   [key()].

%% delete(Key) ->
%%   kv:delete(Key).

%% delete_next(S, _Value, [Key]) ->
%%   lists:keydelete(Key, 1, S).

%% -- Property ---------------------------------------------------------------
weight(_S, lookup) -> 2;
weight(_S, _Cmd)   -> 1.

prop_kv() ->
  numtests(1000,
  ?FORALL(Cmds, commands(?MODULE),
  begin
    kv:start(),
    {H, S, Res} = run_commands(?MODULE, Cmds),
    pretty_commands(?MODULE, Cmds, {H, S, Res},
      measure(length, length(Cmds),
        aggregate(command_names(Cmds),
                  Res == ok)))
  end)).
