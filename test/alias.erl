-module(alias).

-include("mixin.hrl").

-mixin([{foo, [{doit/0, blah}]}]).
-mixin({bar, canhas/0, can_has}).
