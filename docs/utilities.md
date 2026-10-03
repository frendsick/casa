# Specialist Libraries

Compile module-style imports with a library path such as `casac -L lib`.

| Module | Purpose | Runnable example |
|---|---|---|
| `log` | Leveled messages to standard error | [`examples/log.casa`](../examples/log.casa) |
| `timer` | Monotonic elapsed time | [`examples/timer.casa`](../examples/timer.casa) |
| `argparse` | Command-line definitions and help | [`examples/argparse.casa`](../examples/argparse.casa) |
| `parser` | Cursor-based text parsers | [`examples/parser.casa`](../examples/parser.casa) |
| `json` | JSON values, parsing, and serialization | See [JSON](#json) |
| `os` | Files, directories, environment, paths, and processes | [OS reference](os.md) |

Reference tables abbreviate library type names and list inputs in consumption
order. Source examples use qualified names. See [reference notation](notation.md)
for signatures, fragments, and commands for running complete examples.

## Logging

```casa
import "log"

log::Logger::new = logger
log::LogLevel::Info logger.configure
logger "server started" log::log_info
```

The program owns a `Logger` and passes a shared borrow to each log operation.
A selected level includes less verbose levels.

`LogLevel` provides `Error`, `Warning`, `Info`, and `Debug`.

| Method | Signature | Behavior |
|---|---|---|
| [Logger::new](#loggernew) | `fn new -> Logger` | Create logger state at `Warning` |
| [Logger::configure](#loggerconfigure) | `fn configure self:mut$Logger level:LogLevel` | Change the owned level |

### Logger::new

Creates logger state with level `Warning`.

### Logger::configure

Changes the logger level through an exclusive borrow.

| Function | Action |
|---|---|
| `log_error message:str logger:$Logger` | Log an error |
| `log_warning message:str logger:$Logger` | Log a warning |
| `log_info message:str logger:$Logger` | Log information |
| `log_debug message:str logger:$Logger` | Log debugging detail |

## Timing

```casa
import "timer"

timer::Timer::new = timer
f"elapsed: {timer}\n" print
```

| Method | Signature | Behavior |
|---|---|---|
| [Timer::new](#timernew) | `fn new -> Timer` | Start a timer |
| [Timer::elapsed_ns](#timerelapsed_ns) | `fn elapsed_ns self:$Timer -> i64` | Elapsed nanoseconds |
| [Timer::elapsed_ms](#timerelapsed_ms) | `fn elapsed_ms self:$Timer -> i64` | Elapsed milliseconds |
| [Timer::to_str](#timerto_str) | `fn to_str self:$Timer -> std::String` | Fractional seconds, such as `1.042s` |

### Timer::new

Creates a timer using the monotonic clock.

### Timer::elapsed_ns

Returns elapsed nanoseconds without consuming the timer.

### Timer::elapsed_ms

Returns elapsed milliseconds without consuming the timer.

### Timer::to_str

Returns elapsed seconds as owned text, such as `1.042s`.

Create a `Timer` value and keep it for later elapsed-time queries. The timer
module has no global convenience state.

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

| Method | Signature | Behavior |
|---|---|---|
| [ArgParser::new](#argparsernew) | `fn new -> ArgParser` | Parser named from argument `0` |
| [ArgParser::add_positional](#argparseradd_positional) | `fn add_positional self:mut$ArgParser name:str help_text:str` | Required positional value |
| [ArgParser::add_flag](#argparseradd_flag) | `fn add_flag self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Boolean flag |
| [ArgParser::add_terminal_flag](#argparseradd_terminal_flag) | `fn add_terminal_flag self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Flag that permits missing positional values |
| [ArgParser::add_option](#argparseradd_option) | `fn add_option self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Option with one string value |
| [ArgParser::add_multi_option](#argparseradd_multi_option) | `fn add_multi_option self:mut$ArgParser name:str short_flag:str long_flag:str help_text:str` | Repeatable string option |
| [ArgParser::parse_args](#argparserparse_args) | `fn parse_args self:$ArgParser -> ParsedArgs` | Parse process arguments without changing definitions |
| [ParsedArgs::get](#parsedargsget) | `fn get self:$ParsedArgs name:$str -> std::Option[std::String]` | Positional or option value |
| [ParsedArgs::get_flag](#parsedargsget_flag) | `fn get_flag self:$ParsedArgs name:$str -> bool` | Flag state |
| [ParsedArgs::get_multi](#parsedargsget_multi) | `fn get_multi self:$ParsedArgs name:$str -> std::Option[std::List[std::String]]` | Repeatable values |

### ArgParser::new

Creates a parser named from process argument `0`.

### ArgParser::add_positional

Adds a required positional value.

### ArgParser::add_flag

Adds a Boolean flag.

### ArgParser::add_terminal_flag

Adds a flag that permits missing positional values.

### ArgParser::add_option

Adds an option that accepts one string value.

### ArgParser::add_multi_option

Adds a repeatable string option.

### ArgParser::parse_args

Parses process arguments without changing the parser definitions. `-h` and `--help` print help. Invalid arguments print usage and terminate with exit code `2`.

### ParsedArgs::get

Returns an independent cloned positional or option value, if present.

### ParsedArgs::get_flag

Returns the state of a defined flag. An unknown name terminates the program.

### ParsedArgs::get_multi

Returns an independent cloned list of repeatable values, if present.

Use `""` when an option has no short or long spelling.

## Parser building blocks

Import `parser` for a mutable `Cursor`, `ParseError`, and parsers for integers,
identifiers, strings, characters, and escapes. See the compact
[Parser Library](parser.md) reference.

## JSON

```casa
import "std"
import "json"
import "parser"

"{\"name\":\"Ada\"}" parser::Cursor::new = cursor
cursor json::json_parse .unwrap = value
"name" value json::json_get_str .unwrap print
```

`JsonValue` variants are `JsonNull`, `JsonBool`, `JsonInt`, `JsonString`,
`JsonArray`, and `JsonObject`.

| Function | Result |
|---|---|
| `json_parse cursor:mut$Cursor -> Result[JsonValue ParseError]` | Parse one value |
| `json_serialize value:$JsonValue -> String` | Serialize a value |
| `json_escape_string text:$str -> String` | Escape string contents |
| `json_get_value value:$JsonValue key:$str -> Option[$JsonValue]` | Object member |
| `json_get_str value:$JsonValue key:$str -> Option[String]` | Cloned string member |
| `json_get_int value:$JsonValue key:$str -> Option[i64]` | Integer member |
| `json_get_bool value:$JsonValue key:$str -> Option[bool]` | Boolean member |
| `json_get_object value:$JsonValue key:$str -> Option[$JsonValue]` | Object member |
| `json_get_array value:$JsonValue key:$str -> Option[List[JsonValue]]` | Array member |
| `json_object -> Map[String JsonValue]` | Empty object map |
| `json_set value:JsonValue key:String map:Map[String JsonValue] -> Map[String JsonValue]` | Add an object member |

`json_get_value` and `json_get_object` borrow from the input value.
The returned borrow keeps that value loaned until its last use.
`json_get_str` and `json_get_array` return independent cloned values.

JSON numbers are integers. Unicode `\uXXXX` escapes currently decode as `?`.

## Processes

Process arguments and `run_command` are documented with the other
[operating-system APIs](os.md#arguments-and-processes).
