-module(m_tree).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-compile({no_auto_import,[size/1]}).

-export([example/0]).
-export([empty/0, is_empty/1, size/1]).
-export([lookup/2]).

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
%% @doc Returns an empty tree.
%% @end
%%--------------------------------------------------------------------
-spec empty() -> m_tree().
empty() -> 
    #{}.

empty_test() -> 
    ?_assertEqual(#{}, empty()).


%%--------------------------------------------------------------------
%% @doc Returns true if the tree is empty, and false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_empty(Tree) -> boolean() when
      Tree::m_tree().
is_empty(Tree) ->
    0 == size(Tree).

is_empty_test() -> 
    ?_assert(is_empty(empty())).


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
    ?_assertEqual(0, size(empty())).


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
    maps:get(Path, Tree, 'none').

lookupe_test() -> 
    ?_assertEqual('none', lookup([], empty())).

