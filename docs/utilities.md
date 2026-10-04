# Specialist Libraries

Compile module-style imports with a library path such as `casac -L lib`.

| Module | Purpose | Runnable example |
|---|---|---|
| `argparse` | Command-line definitions and help | [examples/argparse.casa](../examples/argparse.casa) |
| `json` | JSON values, parsing, and serialization | See [JSON](#json) |
| `log` | Leveled messages to standard error | [examples/log.casa](../examples/log.casa) |
| `os` | Files, directories, environment, paths, and processes | [OS reference](os.md) |
| `parser` | Cursor-based text parsers | [examples/parser.casa](../examples/parser.casa) |
| `timer` | Monotonic elapsed time | [examples/timer.casa](../examples/timer.casa) |

Reference tables abbreviate library type names and list inputs in consumption
order. Source examples use qualified names. See [reference notation](notation.md)
for signatures, fragments, and commands for running complete examples.

## Argument parsing

```casa
import "argparse"

argparse::ArgParser::new = parser
"input file" "input" parser.add_positional
"verbose output" "--verbose" "-v" "verbose" parser.add_flag
parser.parse_args = arguments
```

`parse_args` handles `-h` and `--help`. Invalid arguments print usage and
terminate with exit code `2`.

Use `""` when an option has no short or long spelling.

| Method | Signature | Behavior |
|---|---|---|
| [add_flag](#argparseradd_flag) | `fn add_flag self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Boolean flag |
| [add_multi_option](#argparseradd_multi_option) | `fn add_multi_option self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Repeatable string option |
| [add_option](#argparseradd_option) | `fn add_option self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Option with one string value |
| [add_positional](#argparseradd_positional) | `fn add_positional self:mut$ArgParser name:str help_text:str` | Required positional value |
| [add_terminal_flag](#argparseradd_terminal_flag) | `fn add_terminal_flag self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Flag that permits missing positional values |
| [get](#parsedargsget) | `fn get self:$ParsedArgs name:$str -> std::Option[std::String]` | Positional or option value |
| [get_flag](#parsedargsget_flag) | `fn get_flag self:$ParsedArgs name:$str -> bool` | Flag state |
| [get_multi](#parsedargsget_multi) | `fn get_multi self:$ParsedArgs name:$str -> std::Option[std::List[std::String]]` | Repeatable values |
| [new](#argparsernew) | `fn new -> ArgParser` | Parser named from argument `0` |
| [parse_args](#argparserparse_args) | `fn parse_args self:$ArgParser -> ParsedArgs` | Parse process arguments without changing definitions |

### ArgParser::add_flag

```text
fn add_flag self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str
```

Adds a Boolean flag.

### ArgParser::add_multi_option

```text
fn add_multi_option self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str
```

Adds a repeatable string option.

### ArgParser::add_option

```text
fn add_option self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str
```

Adds an option that accepts one string value.

### ArgParser::add_positional

```text
fn add_positional self:mut$ArgParser name:str help_text:str
```

Adds a required positional value.

### ArgParser::add_terminal_flag

```text
fn add_terminal_flag self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str
```

Adds a flag that permits missing positional values.

### ParsedArgs::get

```text
fn get self:$ParsedArgs name:$str -> std::Option[std::String]
```

Returns an independent cloned positional or option value, if present.

### ParsedArgs::get_flag

```text
fn get_flag self:$ParsedArgs name:$str -> bool
```

Returns the state of a defined flag. An unknown name terminates the program.

### ParsedArgs::get_multi

```text
fn get_multi self:$ParsedArgs name:$str -> std::Option[std::List[std::String]]
```

Returns an independent cloned list of repeatable values, if present.

### ArgParser::new

```text
fn new -> ArgParser
```

Creates a parser named from process argument `0`.

### ArgParser::parse_args

```text
fn parse_args self:$ArgParser -> ParsedArgs
```

Parses process arguments without changing the parser definitions. `-h` and `--help` print help. Invalid arguments print usage and terminate with exit code `2`.

## JSON

```casa
import "std"
import "json"
import "parser"

"{\"name\":\"Ada\"}" parser::Cursor::new = cursor
cursor json::json_parse.unwrap = value
"name" value json::json_get_str.unwrap print
```

`JsonValue` variants are `JsonNull`, `JsonBool`, `JsonInt`, `JsonString`,
`JsonArray`, and `JsonObject`.

| Function | Result |
|---|---|
| `json_escape_string text:$str -> String` | Escape string contents |
| `json_get_array value:$JsonValue key:$str -> Option[List[JsonValue]]` | Array member |
| `json_get_bool value:$JsonValue key:$str -> Option[bool]` | Boolean member |
| `json_get_int value:$JsonValue key:$str -> Option[i64]` | Integer member |
| `json_get_object value:$JsonValue key:$str -> Option[$JsonValue]` | Object member |
| `json_get_str value:$JsonValue key:$str -> Option[String]` | Cloned string member |
| `json_get_value value:$JsonValue key:$str -> Option[$JsonValue]` | Object member |
| `json_object -> Map[String JsonValue]` | Empty object map |
| `json_parse cursor:mut$Cursor -> Result[JsonValue ParseError]` | Parse one value |
| `json_serialize value:$JsonValue -> String` | Serialize a value |
| `json_set value:JsonValue key:String map:Map[String JsonValue] -> Map[String JsonValue]` | Add an object member |

`json_get_value` and `json_get_object` borrow from the input value.
The [returned borrow](ownership.md#return-a-borrow) keeps that value loaned until its last use.
`json_get_str` and `json_get_array` return independent cloned values.

JSON numbers are integers. Unicode `\uXXXX` escapes currently decode as `?`.

## Logging

```casa
import "log"

log::Logger::new = logger
log::LogLevel::Info logger.configure
logger "server started" log::log_info
```

The program owns a `Logger` and passes a [shared borrow](ownership.md#borrow-for-a-call) to each log operation.
A selected level includes less verbose levels.

`LogLevel` provides `Error`, `Warning`, `Info`, and `Debug`.

| Method | Signature | Behavior |
|---|---|---|
| [configure](#loggerconfigure) | `fn configure self:mut$Logger level:LogLevel` | Change the owned level |
| [new](#loggernew) | `fn new -> Logger` | Create logger state at `Warning` |

### Logger::configure

```text
fn configure self:mut$Logger level:LogLevel
```

Changes the logger level through an exclusive borrow.

### Logger::new

```text
fn new -> Logger
```

Creates logger state with level `Warning`.

### Logging functions

| Function | Action |
|---|---|
| `log_debug message:str logger:$Logger` | Log debugging detail |
| `log_error message:str logger:$Logger` | Log an error |
| `log_info message:str logger:$Logger` | Log information |
| `log_warning message:str logger:$Logger` | Log a warning |

## Parser building blocks

Import `parser` for a mutable `Cursor`, `ParseError`, and parsers for integers,
identifiers, strings, characters, and escapes. See the compact
[Parser Library](parser.md) reference.

## Processes

Process arguments and `run_command` are documented with the other
[operating-system APIs](os.md#arguments-and-processes).

## Timing

```casa
import "timer"

timer::Timer::new = timer
f"elapsed: {timer}\n" print
```

| Method | Signature | Behavior |
|---|---|---|
| [elapsed_ms](#timerelapsed_ms) | `fn elapsed_ms self:$Timer -> i64` | Elapsed milliseconds |
| [elapsed_ns](#timerelapsed_ns) | `fn elapsed_ns self:$Timer -> i64` | Elapsed nanoseconds |
| [new](#timernew) | `fn new -> Timer` | Start a timer |
| [to_str](#timerto_str) | `fn to_str self:$Timer -> std::String` | Fractional seconds, such as `1.042s` |

### Timer::elapsed_ms

```text
fn elapsed_ms self:$Timer -> i64
```

Returns elapsed milliseconds without consuming the timer.

### Timer::elapsed_ns

```text
fn elapsed_ns self:$Timer -> i64
```

Returns elapsed nanoseconds without consuming the timer.

### Timer::new

```text
fn new -> Timer
```

Creates a timer using the monotonic clock.

### Timer::to_str

```text
fn to_str self:$Timer -> std::String
```

Returns elapsed seconds as owned text, such as `1.042s`.

### Timer reuse

Create a `Timer` value and keep it for later elapsed-time queries. The timer
module has no global convenience state.
