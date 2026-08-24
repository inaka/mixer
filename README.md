# Mixer

[![Build Status](https://github.com/inaka/mixer/workflows/build/badge.svg)](https://github.com/inaka/mixer)

Mix in functions from other modules.

## Concept
The original motivation for this parse transform was to permit reuse of functions implementing common logic for tasks such as signature verification and authorization across multiple webmachine resources.
It allows you to provide shared implementations of behavior callbacks without copy&pasting.

## Examples

`foo.erl`:

```erlang
    -module(foo).

    -export([doit/0, doit/1, doit/2]).

    doit() ->
        doit.

    doit(A) ->
        [doit, A].

    doit(A, B) ->
        [doit, A, B].
```

Module `bar.erl` which 'mixes in' all functions from `foo`:

```erlang
    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([foo]).
```

or all except specific functions from `foo`:

```erlang
    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([{foo, except, [doit/0, doit/2]}]).
```

or only specific functions from `foo`:

```erlang
    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([{foo, [doit/0, doit/2]}]).
```

Another version of `bar.erl` which mixes in all functions from `foo` and some functions from `baz`:

```erlang
    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([foo, {baz, [doit/0, doit/1]}]).
```

One more version of `bar.erl` which mixes in `foo:doit/0` and renames it to `do_it_now/0`:

```erlang
    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([{foo, [{doit/0, do_it_now}]}]).
```

Yet another version of `bar.erl` which mixes in all of `foo`'s public functions not implemented by `bar`.
In this case the functions `foo:doit/0` and `foo:doit/1` will be injected into `bar`.

```erlang
    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin([{foo, except, module}]).
    -export([doit/2]).

    doit(A, B) ->
        [bar_did_it, A, B].
```

Last version of `bar.erl`, this time including specs for functions.

```erlang
    -module(bar).
    -include_lib("mixer/include/mixer.hrl").
    -mixin_specs(all).
    -mixin([foo]).
```

(At this time the only valid value for `mixin_specs` is `all`).
