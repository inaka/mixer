-module(import_test).

-include_lib("eunit/include/eunit.hrl").

single_test_() ->
    [{<<"All functions on 'single' stubbed properly">>,
     [?_assert(lists:member({doit, 0}, exports(single))),
      ?_assert(lists:member({doit, 1}, exports(single))),
      ?_assert(lists:member({doit, 2}, exports(single)))]},
     {<<"All functions on 'single' work correctly">>,
      [?_assertMatch(doit, single:doit()),
       ?_assertMatch([doit, 1], single:doit(1)),
       ?_assertMatch([doit, 1, 2], single:doit(1, 2))]}].

multiple_test_() ->
    [{<<"All functions stubbed">>,
      [?_assert(lists:member({doit, 0}, exports(multiple))),
       ?_assert(lists:member({doit, 1}, exports(multiple))),
       ?_assert(lists:member({doit, 2}, exports(multiple))),
       ?_assert(lists:member({canhas, 0}, exports(multiple))),
       ?_assert(lists:member({canhas, 1}, exports(multiple)))]},
     {<<"All stubbed functions work">>,
      [?_assertMatch(doit, multiple:doit()),
       ?_assertMatch({doit, one}, multiple:doit(one)),
       ?_assertMatch([doit, one, two], multiple:doit(one, two)),
       ?_assert(multiple:canhas()),
       ?_assertMatch(cheezburger, multiple:canhas(cheezburger))]}].

alias_test_() ->
    [{<<"Function stubbed with alias">>,
      [?_assert(lists:member({blah, 0}, exports(alias))),
       ?_assert(lists:member({can_has, 0}, exports(alias)))]},
     {<<"All stubbed functions work">>,
      [?_assertMatch(doit, alias:blah()),
       ?_assertMatch(true, alias:can_has())]}].

override_test_() ->
    [{<<"All non-overridden functions are stubbed">>,
      [?_assert(lists:member({doit, 0}, exports(override))),
       ?_assert(lists:member({doit, 1}, exports(override))),
       ?_assert(lists:member({doit, 2}, exports(override)))]},
     {<<"All functions work as expected">>,
      [?_assertMatch(doit, override:doit()),
       ?_assertMatch([5,5], override:doit(5)),
       ?_assertMatch([doit, 5, 10], override:doit(5, 10))]}].

specs_test_() ->
    [{<<"Specs are not generated if not requested">>,
      [?_assertNot(lists:member({doit, 0}, specs(override))),
       ?_assertNot(lists:member({doit, 1}, specs(override))),
       ?_assertNot(lists:member({doit, 2}, specs(override)))]},
     {<<"Specs are generated if requested">>,
      [?_assert(lists:member({doit, 0}, specs(specs))),
       ?_assert(lists:member({doit, 1}, specs(specs))),
       ?_assert(lists:member({doit, 2}, specs(specs)))]},
     {<<"All functions are stubbed properly">>,
     [?_assert(lists:member({doit, 0}, exports(specs))),
      ?_assert(lists:member({doit, 1}, exports(specs))),
      ?_assert(lists:member({doit, 2}, exports(specs)))]},
     {<<"All functions on 'specs' work correctly">>,
      [?_assertMatch(doit, specs:doit()),
       ?_assertMatch([doit, 1], specs:doit(1)),
       ?_assertMatch([doit, 1, 2], specs:doit(1, 2))]}].

exports(Mod) -> Mod:module_info(exports).

specs(Mod) ->
    Path = code:which(Mod),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Path, [abstract_code]),
    [{Fun, Arity} || {attribute, 1, spec, {{Fun, Arity}, _}} <- AC].
