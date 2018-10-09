-module(failure_test).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EXPORTS(Mod), Mod:module_info(exports)).
-define(EUNIT_TEMP_PATH, "_build/test/test_gen").

duplicate_test_() ->
    [{<<"Duplicate mixins detected">>,
      fun() ->
              Mkdir = file:make_dir(?EUNIT_TEMP_PATH),
              ?assertEqual(ok, check_make_dir(Mkdir)),
              {ok, Path, Error} = compile_bad_test_file("duplicates"),
              ?assertMatch({error,[{Path,
                                    [{none,compile,
                                      {parse_transform,mixer,{error,duplicate_mixins}}}]}],
                            []}, Error) end}].

conflicting_mixins_test_() ->
    [{<<"Conflicting mixins detected">>,
      fun()->
              Mkdir = file:make_dir(?EUNIT_TEMP_PATH),
              ?assertEqual(ok, check_make_dir(Mkdir)),
              {ok, Path, Error} = compile_bad_test_file("conflicts"),
              ?assertMatch({error,[{Path,
                                    [{none,compile,
                                      {parse_transform,mixer,{error,duplicate_mixins}}}]}],
                            []}, Error) end}].

%% Internal functions
compile_bad_test_file(Module) ->
    From = filename:join(["test", Module ++ ".erl.bad"]),
    To = filename:join([?EUNIT_TEMP_PATH, Module ++ ".erl"]),
    {ok, BytesCopied} = file:copy(From, To),
    {ok, Info} = file:read_file_info(From),
    ?assertEqual(Info#file_info.size, BytesCopied),
    {ok, To, compile:file(To, [{i, "include"}, return_errors])}.

check_make_dir(ok) -> ok;
check_make_dir({error, eexist}) -> ok;
check_make_dir({error, _} = Error) -> Error.
