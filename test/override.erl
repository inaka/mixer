-module(override).

-include("mixer.hrl").

-mixin([{foo, except, module}]).

-export([doit/1]).
-export(['or'/2]).

doit(A) ->
    [A, A].

'or'(A, B) ->
    ['or_2', A, B].
