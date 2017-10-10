-module(override).

-include("mixer.hrl").

-mixin([{foo, except, ?MODULE}]).

-export([doit/1]).

doit(A) ->
    [A, A].
