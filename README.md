# rebar3_lux

A rebar3 plugin for running [Lux](https://github.com/hawk/lux) tests as
part of your Erlang/OTP build process.

## Overview

This plugin integrates Lux test execution with rebar3, allowing you to
run command-line interface tests directly from your rebar3 workflow.

### Features

- **Smart binary resolution**: Automatically uses the Lux binary from plugin dependencies (`_build/default/plugins/lux/bin/lux`)
- **PATH fallback**: Falls back to system-installed `lux` if dependency binary is not found
- **Selective test execution**: Run individual test modules or entire test suites
- **Verbose output support**: Enable detailed Lux output for debugging
- **Automatic file extension handling**: Works with both `foo` and `foo.lux` module names

## Usage

Add the plugin to your `rebar.config`:

```erlang
{plugins, [
  {rebar3_lux, ".*", {git, "https://github.com/etnt/rebar3_lux.git", {branch, "main"}}}
]}.
```

The plugin will automatically download and build the [Lux](https://github.com/hawk/lux) dependency when first used.

Then run:

```bash
rebar3 lux                       # Run all lux tests
rebar3 lux --module=foo          # Run only foo.lux test
rebar3 lux -m bar                # Run only bar.lux test (short form)
rebar3 lux --verbose             # Run all tests with verbose output
rebar3 lux -m foo -v             # Run foo.lux with verbose output
```

## Command Line Options

- `--module=<name>` or `-m <name>` - Run a specific .lux test module
- `--verbose` or `-v` - Enable verbose output from Lux

The plugin automatically adds the `.lux` extension if not provided, so both `--module=foo` and `--module=foo.lux` will run the same test.

## Requirements

- [Lux](https://github.com/hawk/lux) will be installed as a dependency.

## Test Directory Structure

The plugin expects Lux tests to be located in the `test/lux/` directory of your project. When executed, it will change to this directory and run the specified tests from there.

## License

MPL-2.0
