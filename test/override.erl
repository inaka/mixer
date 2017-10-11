-module(override).

-include("mixer.hrl").

-mixin([{foo, except, module}]).

-export([doit/1]).

doit(A) ->
    [A, A].
