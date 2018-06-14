-module(override).

-include("mixer.hrl").

-mixin([{foo, except, module}]).

-export([doit/1]).
-export([
  'or'/2, 'a-function'/1,
  'CamelCaseFunction WithSpaces'/0, 'CamelCaseFunction WithSpaces'/1
]).

doit(A) ->
    [A, A].

'or'(A, B) ->
    ['or_override', A, B].

'a-function'(A) ->
  [a_function_override, A].

'CamelCaseFunction WithSpaces'() ->
  [camel_override].

'CamelCaseFunction WithSpaces'(A) ->
  [camel_override, A].
