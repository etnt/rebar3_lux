# rebar3_lux

A rebar3 plugin for running [Lux](https://github.com/hawk/lux) tests as part of your Erlang/OTP build process.

## Overview

This plugin integrates Lux test execution with rebar3, allowing you to run command-line interface tests directly from your rebar3 workflow. It automatically ensures that compiled beam files are available in the expected locations before running tests.

## Usage

Add to your `rebar.config`:

```erlang
{plugins, [{rebar3_lux, {path, "_plugins/rebar3_lux"}}]}.
```

Then run:

```bash
rebar3 lux                    # Run all lux tests
rebar3 lux -s test_name.lux   # Run specific test
```

## Requirements

- [Lux](https://github.com/hawk/lux) must be installed and available in PATH
- Compiled beam files in `ebin/` directory (handled automatically)

## Development Status

This plugin is currently in development. The core functionality works, but plugin loading has some known issues. For reliable lux test execution, consider using the post-hooks approach in your main project's rebar.config instead.

## License

MPL-2.0
