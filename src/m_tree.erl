-module(m_tree).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-compile({no_auto_import,[size/1]}).

-export([example/0]).
-export([new/1, size/1]).
-export([lookup/2, insert/3, update/3]).

-type key()    :: term(). 
-type m_tree() :: #{Path::[key()] => {Value::term(), [key()]}}.


% Tree example ------------------------------------------------------
example() -> 
    #{
        []      => {'√',  [a]  },
        [a]     => {'*',  [b,c]},
        [b,a]   => {'i1', []   },
        [c,a]   => {'*',  [d,e]},
        [d,c,a] => {'i1', []   },
        [e,c,a] => {'i1', []   }
    }.

%%--------------------------------------------------------------------
%% @doc Returns a new tree with the indicated value at the roor '[]'.
%% @end
%%--------------------------------------------------------------------
-spec new(Val) -> m_tree() when
    Val :: term().
new(Val) -> 
    #{[] => {Val, []}}.

new_test() -> 
    ?assertEqual(#{[]=>{val_a,[]}}, new(val_a)).


%%--------------------------------------------------------------------
%% @doc Returns the number of nodes in the tree as an integer.
%%      Returns 0 (zero) if the tree is empty.
%% @end
%%--------------------------------------------------------------------
-spec size(Tree) -> non_neg_integer() when
      Tree :: m_tree().
size(Tree) ->
    map_size(Tree).

size_test() -> 
    ?assertEqual(1, size(new(val_1))).


%%--------------------------------------------------------------------
%% @doc Looks up the path in tree. Returns {value, V}, or `none' if 
%%      the path does not exist.
%% @end
%%--------------------------------------------------------------------
-spec lookup(Path, Tree) -> 'none' | {'value', Val} when
      Path :: [key()],
      Val  :: term(),
      Tree :: m_tree().
lookup(Path, Tree) ->
    case maps:get(Path, Tree, 'none') of 
        {Val, _} -> {value, Val};
        'none' -> 'none'
    end.

lookup_test() -> 
    ?assertEqual({value, 'val_1'}, lookup([], new('val_1'))),
    ?assertEqual('none', lookup([bad_path], new('val_1'))).


%%--------------------------------------------------------------------
%% @doc Retreives the value stored in the path. Assumes that the path
%%      is present in the tree.
%% @end
%%--------------------------------------------------------------------
-spec get(Path, Tree) -> Val when
      Path :: [key()],
      Val  :: term(),
      Tree :: m_tree().
get(Path, Tree) ->
    {Val, _} = maps:get(Path, Tree),
    Val.

get_test() -> 
    ?assertEqual('val_1', get([], new('val_1'))),
    ?assertException(error, {badkey,[bad_path]}, get([bad_path], new('val_1'))).


%%--------------------------------------------------------------------
%% @doc Inserts a new value under a path. Returns the assogned key and
%%      the new tree. Assumes that the path is present in the tree.
%% @end
%%--------------------------------------------------------------------
-spec insert(Path, Val, Tree1) -> {Key, Tree2} when
      Path  :: [key()],
      Val   :: term(),
      Tree1 :: m_tree(),
      Key   :: key(),
      Tree2 :: m_tree().
insert(Path, Val, Tree) ->
    {Key, Tree2} = add_key(Path, Tree), % Adds a new branch to the tree
    {Key, Tree2#{[Key|Path] => {Val, []}}}. % Inserts the node on the branch

insert_test() -> 
    WriteAndReadTest = fun(Val) -> 
        {K,Tree} = insert([], Val, new(a)), 
        get([K], Tree)
    end,
    ?assertEqual(b, WriteAndReadTest(b)).


%%--------------------------------------------------------------------
%% @doc Updates value at path in the tree and returns the new tree. 
%%      Assumes that the key is present in the tree.
%% @end
%%--------------------------------------------------------------------
-spec update(Path, Val, Tree1) -> Tree2 when
      Path  :: [key()],
      Val   :: term(),
      Tree1 :: m_tree(),
      Tree2 :: m_tree().
update(Path, Val, Tree) ->
    Updt_val = fun({_, Keys}) -> {Val, Keys} end, 
    maps:update_with(Path, Updt_val, Tree).

update_test() -> 
    ?assertEqual(new(b), update([], b, new(a))),
    ?assertException(error, {badkey,[bad_path]}, update([bad_path], b, new(a))).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Adds a new key to the node connections ----------------------------
-define(id, erlang:unique_integer([monotonic, positive])).
add_key(Path, Tree) ->
    Id = ?id,
    Add_key = fun({Val, Keys}) -> {Val, [Id|Keys]} end, 
    {Id, maps:update_with(Path, Add_key, Tree)}.

