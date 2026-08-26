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

## Logging

```casa
import "log"

LogLevel::Info log_set_level
"server started" log_info
```

The default level is `Warning`. A selected level includes less verbose levels.

| API | Action |
|---|---|
| `LogLevel::Error`, `Warning`, `Info`, `Debug` | Available levels |
| `log_set_level level:LogLevel` | Set the active level |
| `log_error message:str` | Log an error |
| `log_warning message:str` | Log a warning |
| `log_info message:str` | Log information |
| `log_debug message:str` | Log debugging detail |

## Timing

```casa
import "timer"

Timer::new = timer
f"elapsed: {timer}\n" print
```

| API | Result or action |
|---|---|
| `Timer::new -> Timer` | Start a timer |
| `elapsed_ns self:Timer -> i64` | Elapsed nanoseconds |
| `elapsed_ms self:Timer -> i64` | Elapsed milliseconds |
| `to_str self:Timer -> str` | Fractional seconds, such as `1.042s` |
| `timer_start` | Start the global timer |
| `timer_elapsed_ns -> i64` | Global elapsed nanoseconds |
| `timer_elapsed_ms -> i64` | Global elapsed milliseconds |

The global elapsed functions terminate if `timer_start` was not called.

## Argument parsing

```casa
import "argparse"

ArgParser::new = parser
"input file" "input" parser.add_positional
"verbose output" "--verbose" "-v" "verbose" parser.add_flag
parser.parse_args = arguments
```

`parse_args` handles `-h` and `--help`. Invalid arguments print usage and
terminate with exit code `2`.

| API | Result or action |
|---|---|
| `ArgParser::new -> ArgParser` | Parser named from argument `0` |
| `add_positional self name help_text` | Required positional value |
| `add_flag self name short long help_text` | Boolean flag |
| `add_terminal_flag self name short long help_text` | Flag that permits missing positional values |
| `add_option self name short long help_text` | Option with one string value |
| `add_multi_option self name short long help_text` | Repeatable string option |
| `parse_args self:$ArgParser -> ParsedArgs` | Parse process arguments without changing definitions |
| `get self:ParsedArgs name:str -> Option[str]` | Positional or option value |
| `get_flag self:ParsedArgs name:str -> bool` | Flag state |
| `get_multi self:ParsedArgs name:str -> Option[List[str]]` | Repeatable values |

Use `""` when an option has no short or long spelling.

## Parser building blocks

Import `parser` for a mutable `Cursor`, `ParseError`, and parsers for integers,
identifiers, strings, characters, and escapes. See the compact
[Parser Library](parser.md) reference.

## JSON

```casa
import "json"

"{\"name\":\"Ada\"}" Cursor::new json_parse .unwrap = value
"name" value json_get_str .unwrap print
```

`JsonValue` variants are `JsonNull`, `JsonBool`, `JsonInt`, `JsonString`,
`JsonArray`, and `JsonObject`.

| API | Result |
|---|---|
| `json_parse cursor:Cursor -> Result[JsonValue ParseError]` | Parse one value |
| `json_serialize value:JsonValue -> str` | Serialize a value |
| `json_escape_string text:str -> str` | Escape string contents |
| `json_get_value value key -> Option[JsonValue]` | Object member |
| `json_get_str value key -> Option[str]` | String member |
| `json_get_int value key -> Option[i64]` | Integer member |
| `json_get_bool value key -> Option[bool]` | Boolean member |
| `json_get_object value key -> Option[JsonValue]` | Object member |
| `json_get_array value key -> Option[List[JsonValue]]` | Array member |
| `json_object -> Map[str JsonValue]` | Empty object map |
| `json_set value key map -> Map[str JsonValue]` | Add an object member |

JSON numbers are integers. Unicode `\uXXXX` escapes currently decode as `?`.

## Processes

Process arguments and `run_command` are documented with the other
[operating-system APIs](os.md#arguments-and-processes).
