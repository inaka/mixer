-module(foo).

-export([doit/0, doit/1, doit/2]).
-export([
  'or'/2, 'a-function'/1,
  'CamelCaseFunction WithSpaces'/0, 'CamelCaseFunction WithSpaces'/1
]).
-export(['\''/0, '\''/1, ''/0, ''/1]).
-export(['🎱'/0, '🎱'/1]).

doit() ->
    doit.

doit(A) ->
    [doit, A].

doit(A, B) ->
    [doit, A, B].

'or'(A, B) ->
    ['or', A, B].

'a-function'(A) ->
    [a_function, A].

'CamelCaseFunction WithSpaces'() ->
    [camel].

'CamelCaseFunction WithSpaces'(A) ->
    [camel, A].

'\''() ->
    "why do you do this, kiddo?".

'\''(This) ->
    {This, is, devilish}.

''() ->
    ''(wat).

''(Var) ->
    {'🤯', Var}.

'🎱'() ->
    '🎱'('🙄').

'🎱'(X) ->
    {'not', X}.
