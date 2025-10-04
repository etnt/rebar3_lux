# rebar3_lux

**NOTE: in development, not working atm...**

A rebar3 plugin for running [Lux](https://github.com/hawk/lux) tests as
part of your Erlang/OTP build process.

## Overview

This plugin integrates Lux test execution with rebar3, allowing you to
run command-line interface tests directly from your rebar3 workflow.
It automatically ensures that compiled beam files are available in
the expected locations before running tests.

## Usage

Add to your `rebar.config`:

```erlang
{plugins, [
  {rebar3_lux, ".*", {git, "https://github.com/etnt/rebar3_lux.git", {branch, "main"}}}]}.
```

Then run:

```bash
rebar3 lux                    # Run all lux tests
rebar3 lux -s test_name.lux   # Run specific test
```

## Requirements

- [Lux](https://github.com/hawk/lux) will be installed as a dependency.

## Development Status

This plugin is currently in development.

## License

MPL-2.0
