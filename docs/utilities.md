# Utilities

Application-level utilities for logging, timing, command-line argument parsing, and process execution.

## Logging

The logging library is in `lib/log.casa`. Import it with:

```casa
import "path/to/lib/log.casa"
```

It provides leveled logging to stderr. Messages above the current log level (more verbose) are suppressed. The default level is `LogLevel::Warning`.

### `LogLevel`

An enum with four variants, ordered from least to most verbose:

| Variant | Ordinal | Description |
|---------|---------|-------------|
| `LogLevel::Error` | 0 | Critical errors |
| `LogLevel::Warning` | 1 | Warnings (default level) |
| `LogLevel::Info` | 2 | Informational messages |
| `LogLevel::Debug` | 3 | Debug details |

### `log_set_level`

Sets the global log level. Messages at or below this level are printed.

**Signature:** `log_set_level level:LogLevel`

**Stack effect:** `LogLevel -> None`

```casa
LogLevel::Info log_set_level
```

### `log_error`

Logs a message at ERROR level.

**Signature:** `log_error msg:str`

**Stack effect:** `str -> None`

```casa
"something went wrong" log_error
# stderr: [ERROR] something went wrong
```

### `log_warning`

Logs a message at WARNING level.

**Signature:** `log_warning msg:str`

**Stack effect:** `str -> None`

```casa
"deprecated feature used" log_warning
# stderr: [WARNING] deprecated feature used
```

### `log_info`

Logs a message at INFO level.

**Signature:** `log_info msg:str`

**Stack effect:** `str -> None`

```casa
"processing file" log_info
# stderr: [INFO] processing file
```

### `log_debug`

Logs a message at DEBUG level.

**Signature:** `log_debug msg:str`

**Stack effect:** `str -> None`

```casa
f"token count: {count .to_str}" log_debug
# stderr: [DEBUG] token count: 42
```

### Complete Example

```casa
import "path/to/lib/log.casa"

# Default level is Warning — only Error and Warning are shown
"this is hidden" log_info
"this is visible" log_warning

# Raise the level to see Info messages
LogLevel::Info log_set_level
"now this is visible" log_info
"still visible" log_warning
```

## Timer

The timer library is in `lib/timer.casa`. Import it with:

```casa
import "path/to/lib/timer.casa"
```

It provides high-resolution timing using `clock_gettime` with `CLOCK_MONOTONIC`. Timer implements the `Display` trait, formatting elapsed time as fractional seconds (e.g., `"1.042s"`).

### `Timer::new`

Creates a new Timer and starts it immediately.

**Signature:** `Timer::new -> Timer`

**Stack effect:** `-> Timer`

```casa
Timer::new = timer
```

### `Timer::elapsed_ns`

Returns elapsed nanoseconds since the Timer was created.

**Signature:** `elapsed_ns self:Timer -> int`

**Stack effect:** `Timer -> int`

```casa
timer .elapsed_ns print   # e.g. 42000000
```

### `Timer::elapsed_ms`

Returns elapsed milliseconds since the Timer was created.

**Signature:** `elapsed_ms self:Timer -> int`

**Stack effect:** `Timer -> int`

```casa
timer .elapsed_ms print   # e.g. 42
```

### `Timer::to_str`

Returns the elapsed time formatted as fractional seconds. Implements the `Display` trait, so Timer can be used directly in format strings.

**Signature:** `to_str self:Timer -> str`

**Stack effect:** `Timer -> str`

```casa
f"Elapsed: {timer}\n" print   # e.g. Elapsed: 1.042s
```

### Global Convenience Functions

A global timer can be used without managing a Timer struct directly.

#### `timer_start`

Starts the global timer.

**Signature:** `timer_start`

**Stack effect:** `-> None`

#### `timer_elapsed_ms`

Returns elapsed milliseconds from the global timer. Exits with an error if `timer_start` has not been called.

**Signature:** `timer_elapsed_ms -> int`

**Stack effect:** `-> int`

#### `timer_elapsed_ns`

Returns elapsed nanoseconds from the global timer. Exits with an error if `timer_start` has not been called.

**Signature:** `timer_elapsed_ns -> int`

**Stack effect:** `-> int`

### Complete Example

```casa
import "path/to/lib/timer.casa"

Timer::new = timer

# Do some work
0 = count
while 100000 count > do
    1 += count
done

f"Elapsed: {timer}\n" print
f"Elapsed ms: {timer .elapsed_ms}\n" print
```

## Command-Line Arguments

### `argc`

Pushes the number of command-line arguments onto the stack.

**Signature:** `argc -> int`

**Stack effect:** `-> int`

```casa
argc print    # prints the argument count
```

### `argv`

Pushes a pointer to the argument array onto the stack.

**Signature:** `argv -> ptr`

**Stack effect:** `-> ptr`

### `get_arg`

Returns the nth command-line argument as a string (zero-indexed). Prints an error to stderr and exits if the index is out of bounds.

**Signature:** `get_arg n:int -> str`

**Stack effect:** `int -> str`

```casa
0 get_arg print    # prints the program name
1 get_arg print    # prints the first argument
```

## Argument Parser

The argument parser library is in `lib/argparse.casa`. Import it with:

```casa
import "path/to/lib/argparse.casa"
```

It provides a declarative API for defining and parsing CLI arguments, with auto-generated help output similar to Python's `argparse`.

### `ArgParser::new`

Creates a new argument parser. The program name used in help/error output is derived from `basename(argv[0])`.

**Signature:** `ArgParser::new -> ArgParser`

**Stack effect:** `-> ArgParser`

```casa
ArgParser::new = parser
```

### `ArgParser::add_positional`

Adds a required positional argument.

**Signature:** `ArgParser::add_positional self:ArgParser name:str help_text:str`

**Stack effect:** `ArgParser str str -> None`

```casa
"input file" "input" parser .add_positional
```

### `ArgParser::add_flag`

Adds a boolean flag (store-true). Pass `""` for `short_flag` or `long_flag` if only one form is needed.

**Signature:** `ArgParser::add_flag self:ArgParser name:str short_flag:str long_flag:str help_text:str`

**Stack effect:** `ArgParser str str str str -> None`

```casa
"verbose output" "--verbose" "-v" "verbose" parser .add_flag
```

### `ArgParser::add_option`

Adds an option that takes a string value.

**Signature:** `ArgParser::add_option self:ArgParser name:str short_flag:str long_flag:str help_text:str`

**Stack effect:** `ArgParser str str str str -> None`

```casa
"output binary name" "--output" "-o" "output" parser .add_option
```

### `ArgParser::add_multi_option`

Adds an option that takes a string value and may appear multiple times. Each occurrence appends its value to a list retrieved with `ParsedArgs::get_multi`.

**Signature:** `ArgParser::add_multi_option self:ArgParser name:str short_flag:str long_flag:str help_text:str`

**Stack effect:** `ArgParser str str str str -> None`

```casa
"library search path" "--library-path" "-L" "library_path" parser .add_multi_option
```

### `ArgParser::parse_args`

Parses `argc`/`argv` and returns the results. Automatically handles `-h`/`--help` (prints help and exits). Prints a usage error and exits on unrecognized arguments or missing positionals.

**Signature:** `ArgParser::parse_args self:ArgParser -> ParsedArgs`

**Stack effect:** `ArgParser -> ParsedArgs`

```casa
parser .parse_args = args
```

### `ParsedArgs::get`

Returns an `Option[str]` for the named argument. Returns `Option::Some` with the value if provided, `Option::None` if not.

**Signature:** `ParsedArgs::get self:ParsedArgs name:str -> Option[str]`

**Stack effect:** `ParsedArgs str -> Option[str]`

```casa
"input" args .get .unwrap = input_file
"output" args .get = output_opt    # Option::None if -o not given
```

### `ParsedArgs::get_flag`

Returns `true` if the flag was set, `false` otherwise.

**Signature:** `ParsedArgs::get_flag self:ParsedArgs name:str -> bool`

**Stack effect:** `ParsedArgs str -> bool`

```casa
"verbose" args .get_flag = is_verbose
```

### `ParsedArgs::get_multi`

Returns `Option::Some` wrapping the collected `List[str]` for a registered multi-option (the list is empty if the flag was never given). Returns `Option::None` if the name was never registered as a multi-option, mirroring `ParsedArgs::get`.

**Signature:** `ParsedArgs::get_multi self:ParsedArgs name:str -> Option[List[str]]`

**Stack effect:** `ParsedArgs str -> Option[List[str]]`

```casa
"library_path" args .get_multi .unwrap = library_paths
```

### Complete Example

```casa
import "path/to/lib/argparse.casa"

ArgParser::new = parser
"input file" "input" parser .add_positional
"output file" "--output" "-o" "output" parser .add_option
"verbose output" "--verbose" "-v" "verbose" parser .add_flag
parser .parse_args = args

"input" args .get .unwrap = input_file
"output" args .get = output_opt
"verbose" args .get_flag = is_verbose
```

Assuming the compiled binary is named `myapp`, running `./myapp --help` produces:

```
usage: myapp [-h] [-o OUTPUT] [-v] input

positional arguments:
  input               input file

options:
  -h, --help          show this help message and exit
  -o, --output OUTPUT output file
  -v, --verbose       verbose output
```

## Process Execution

### `run_command`

Executes an external command using fork/execve/wait4. Takes a `List[str]` where the first element is the executable path and the remaining elements are arguments. Returns the child process exit code.

**Signature:** `run_command args:List[str] -> int`

**Stack effect:** `List[str] -> int`

```casa
List::new (List[str]) = args
"/bin/echo" args.push
"hello" args.push
args run_command = exit_code
```

Prints an error to stderr and exits if fork fails. If execve fails (command not found), the child process exits with code 1.

### `chars_to_str`

Converts a `List[char]` to a `str`. Used internally by `StringBuilder::build`.

**Signature:** `chars_to_str chars:List[char] -> str`

**Stack effect:** `List[char] -> str`

## See Also

- [Standard Library](standard-library.md) -- arrays, Option, Result, and the `import` directive
- [Collections](collections.md) -- List, Map, Set, and StringBuilder
- [Strings and IO](strings-and-io.md) -- string methods and file I/O
