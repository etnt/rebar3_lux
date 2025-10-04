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
        % The 'user friendly' name of the task
        {name, ?PROVIDER},
        % The module implementation of the task
        {module, ?MODULE},
        % The task can be run by the user, always true
        {bare, true},
        % The list of dependencies
        {deps, ?DEPS},
        % How to use the plugin
        {example, "rebar3 lux"},
        % list of options understood by the plugin
        {opts, ?OPTS},
        {short_desc, ?DESCRIPTION},
        {desc, ?DESCRIPTION},
        {profiles, [test]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Parse command line options
    {Args, _} = rebar_state:command_parsed_args(State),
    Suite = proplists:get_value(suite, Args, "."),
    Verbose = proplists:get_bool(verbose, Args),

    %% Find the lux binary path
    LuxBin = find_lux_binary(),

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
            Cmd = LuxBin ++ VerboseFlag ++ " " ++ Suite,

            case LuxBin of
                "lux" ->
                    rebar_api:info("Using lux binary from PATH", []);
                _ ->
                    rebar_api:info("Using lux binary: ~s", [LuxBin])
            end,
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

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Find the lux binary in the dependency structure
find_lux_binary() ->
    %% When rebar3_lux is used as a plugin, it's located at:
    %% _build/default/plugins/rebar3_lux
    %% And lux dependency (also a plugin dependency) should be at:
    %% _build/default/plugins/lux

    LuxPath =
        case code:lib_dir(rebar3_lux) of
            {error, bad_name} ->
                %% Plugin is not loaded via code path, determine from working directory
                {ok, Cwd} = file:get_cwd(),
                find_lux_from_build_dir(Cwd);
            LibDir ->
                %% Found rebar3_lux directory, determine if it's in lib or plugins
                case string:str(LibDir, "/plugins/") of
                    0 ->
                        %% It's in lib directory, lux should be parallel
                        LibsDir = filename:dirname(LibDir),
                        filename:join([LibsDir, "lux", "bin", "lux"]);
                    _ ->
                        %% It's in plugins directory, lux is also in plugins directory
                        PluginsDir = filename:dirname(LibDir),
                        filename:join([PluginsDir, "lux", "bin", "lux"])
                end
        end,

    %% Check if the lux binary exists at the expected location
    case filelib:is_regular(LuxPath) of
        true ->
            LuxPath;
        false ->
            %% Fallback to using 'lux' from PATH
            "lux"
    end.

%% Helper function to find lux binary from current working directory
find_lux_from_build_dir(Path) ->
    Parts = filename:split(Path),
    case find_build_index(Parts) of
        not_found ->
            %% Not in _build directory, assume we're at project root
            %% Return path to plugins directory (most common case)
            "_build/default/plugins/lux/bin/lux";
        Index ->
            %% Found _build in path, construct path to lux binary in plugins

            % Include _build/default
            BuildParts = lists:sublist(Parts, Index + 2),
            BuildPath = filename:join(BuildParts),
            filename:join([BuildPath, "plugins", "lux", "bin", "lux"])
    end.

%% Find the index of "_build" in path parts
find_build_index(Parts) ->
    find_build_index(Parts, 1).

find_build_index([], _) ->
    not_found;
find_build_index(["_build" | _], Index) ->
    Index;
find_build_index([_ | Rest], Index) ->
    find_build_index(Rest, Index + 1).
