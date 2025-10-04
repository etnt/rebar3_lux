-module(rebar3_lux_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(DESCRIPTION, "A rebar3 plugin to run Lux tests.").
-define(PROVIDER, lux).
-define(DEPS, []).
-define(OPTS, []).
-define(INCLUDE_FILE_PATTERNS, [
  "\\A.+\\.lux\\z"
]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name, ?PROVIDER},            % The 'user friendly' name of the task
    {module, ?MODULE},            % The module implementation of the task
    {bare, true},                 % The task can be run by the user, always true
    {deps, ?DEPS},                % The list of dependencies
    {example, "rebar3 lux"},      % How to use the plugin
    {opts, ?OPTS},                % list of options understood by the plugin
    {short_desc, ?DESCRIPTION},
    {desc, ?DESCRIPTION},
    {profiles, [test]}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Determine which provider is being called based on the command
    case rebar_state:command(State) of
        lux -> do_lux(State);
        Cmd -> {error, Cmd}
    end.

do_lux(State) ->

    %% Parse command line options
    {Args, _} = rebar_state:command_parsed_args(State),
    Suite = proplists:get_value(suite, Args, "."),
    Verbose = proplists:get_bool(verbose, Args),

    %% Change to test/lux directory
    OrigDir = file:get_cwd(),
    LuxDir = "test/lux",

    case file:set_cwd(LuxDir) of
        ok ->
            %% Build lux command
            VerboseFlag =
                case Verbose of
                    true -> " --verbose";
                    false -> ""
                end,
            Cmd = "lux" ++ VerboseFlag ++ " " ++ Suite,

            rebar_api:info("Running: ~s (in ~s)", [Cmd, LuxDir]),

            %% Execute lux
            case run_cmd(Cmd) of
                0 ->
                    file:set_cwd(OrigDir),
                    {ok, State};
                ExitCode ->
                    file:set_cwd(OrigDir),
                    rebar_api:abort("Lux tests failed with exit code ~p", [
                        ExitCode
                    ])
            end;
        {error, Reason} ->
            rebar_api:abort("Failed to change to ~s directory: ~p", [
                LuxDir, Reason
            ])
    end.

run_cmd(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status, {line, 256}]),
    receive_port_output(Port).

receive_port_output(Port) ->
    receive
        {Port, {data, {eol, Line}}} ->
            io:format("~s~n", [Line]),
            receive_port_output(Port);
        {Port, {data, {noeol, Line}}} ->
            io:format("~s", [Line]),
            receive_port_output(Port);
        {Port, {exit_status, Status}} ->
            Status;
        Other ->
            rebar_api:debug("Unexpected message: ~p", [Other]),
            receive_port_output(Port)
    end.
