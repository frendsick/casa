# JSON

Import `json` to convert between JSON text and Casa values. Implement
`json::Serialize` and `json::Deserialize` for your own structs and enums.
Each implementation chooses its JSON representation, field names, and validation.

```casa
import "std"
import "json"

struct User {
    name: std::String
}

impl User: json::Serialize + json::Deserialize {
    fn to_json self:$User -> std::Result[json::JsonValue json::Error] {
        json::json_object = fields
        self.name.to_json ? "name" fields.set_str
        fields json::JsonValue::JsonObject std::Result::Ok
    }

    fn from_json value:$json::JsonValue -> std::Result[User json::Error] {
        "name" value.require_field ? std::String::from_json ? User std::Result::Ok
    }
}

User { name: "Ada".to_str } = original
original json::serialize.unwrap = encoded
encoded.as_str &json::deserialize[User] exec = result
result.unwrap = restored
restored.name print
```

`serialize` infers the type from its argument. `deserialize` needs an explicit
result type, supplied through a specialized function reference such as
`&json::deserialize[User]`. The [runnable example](../examples/json.casa) also
implements an enum as a JSON string and rejects unknown variant names.

## Conversion contracts

| Operation | Contract |
|---|---|
| `Serialize.to_json self:$self -> Result[JsonValue Error]` | Borrow a value and build an owned JSON tree. |
| `Deserialize.from_json value:$JsonValue -> Result[self Error]` | Validate a borrowed tree and return an owned value. |
| `serialize [T:Serialize] value:$T -> Result[String Error]` | Convert a value and emit compact JSON text. |
| `deserialize [T:Deserialize] text:$str -> Result[T Error]` | Parse one complete document and convert its tree. |
| `JsonValue.require_field self:$JsonValue key:$str -> Result[$JsonValue Error]` | Borrow a required object member or report its absence. |

The signatures above use `std::Result` and `std::String`. Implement either trait
or both. Neither conversion consumes its input. Deserialized strings and
collections own their storage and remain valid after the input is destroyed.
Serialization builds an intermediate `JsonValue` tree before writing text.

Use `?` to propagate conversion errors. Keep object builders in local bindings
so an early return does not leave intermediate values on the stack. For enum
and generic collection constructors, use a function reference:

```casa
import "std"
import "json"

fn read_scores value:$json::JsonValue -> std::Result[std::List[i64] json::Error] {
    "scores" value.require_field ? &std::List[i64]::from_json exec
}
```

Custom implementations decide whether fields are required, whether unknown
fields are accepted, and how enums store their tags and payloads. The example
requires both fields and ignores additional fields. Missing fields do not
implicitly become `Option::None`.

## Supported types

| Casa type | JSON representation |
|---|---|
| `bool` | Boolean. |
| `char` | String containing exactly one Unicode scalar. |
| `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64` | Integer within both the Casa type's range and the JSON library's `i64` range. |
| `JsonValue` | Independent copy of the JSON tree. |
| `List[T]` | Array, when `T` implements the corresponding trait. |
| `Map[String T]` | Object, when `T` implements the corresponding trait. |
| `Option[T]` | `null` for `None`, otherwise the representation of `T`. |
| `str` | String. Serialization only. |
| `String` | String. Both conversions are supported. |

Nested lists, maps, and optional values use the element type's conversion.
`Option[T]` cannot distinguish `None` from a `Some` value whose own representation
is `null`. Use a custom tagged representation when that distinction matters.
Object member order follows map iteration and is not a stable output contract.
Duplicate input keys use the last value.

Numbers remain limited to `i64`. Fractional and exponent forms, integer overflow,
and `u64` serialization above `9223372036854775807` return errors. There are no
conversions for `f32` or `f64`. Revisit the numeric representation in `JsonValue`
when adding floating-point or full-range unsigned JSON numbers.

## Errors

Both typed entry points return `Result` with `json::Error`:

| Variant | Meaning |
|---|---|
| `InvalidValue(str)` | The value has invalid content, such as an unknown enum tag or a multi-character string for `char`. |
| `MissingField(String)` | A required object member is absent. The payload names the field. |
| `OutOfRange(str)` | An integer does not fit the destination range. The payload names the target type. |
| `Parse(parser::ParseError)` | Invalid JSON text, unsupported number, or trailing content. The payload contains a message and a byte offset. |
| `TypeMismatch(str)` | The JSON value has the wrong kind. The payload names the expected kind. |

Errors from nested conversions propagate unchanged. They do not include the
complete field path or array index. Use `InvalidValue` for application validation.

Strings preserve UTF-8 text and decode all JSON escapes, including UTF-16
surrogate pairs. Invalid escapes, lone surrogates, and unescaped control
characters are rejected. Serialization escapes every control character below
U+0020. Leading and trailing JSON whitespace is accepted.

## Working with a JSON tree

`JsonValue` has the variants `JsonNull`, `JsonBool(bool)`, `JsonInt(i64)`,
`JsonString(String)`, `JsonArray(List[JsonValue])`, and
`JsonObject(Map[String JsonValue])`.

```casa
import "std"
import "json"

"{\"name\":\"Ada\"}" &json::deserialize[json::JsonValue] exec = parsed
parsed.unwrap = value
"name" value json::json_get_str.unwrap print
value json::json_serialize print
```

The existing tree API remains available:

| Function | Result |
|---|---|
| `json_escape_string text:$str -> String` | Escape string contents without surrounding quotes. |
| `json_get_array value:$JsonValue key:$str -> Option[List[JsonValue]]` | Clone an array member. |
| `json_get_bool value:$JsonValue key:$str -> Option[bool]` | Read a boolean member. |
| `json_get_int value:$JsonValue key:$str -> Option[i64]` | Read an integer member. |
| `json_get_object value:$JsonValue key:$str -> Option[$JsonValue]` | Borrow an object member. |
| `json_get_str value:$JsonValue key:$str -> Option[String]` | Clone a string member. |
| `json_get_value value:$JsonValue key:$str -> Option[$JsonValue]` | Borrow any object member. |
| `json_object -> Map[String JsonValue]` | Construct an empty object map. |
| `json_parse cursor:mut$parser::Cursor -> Result[JsonValue parser::ParseError]` | Parse one value at the current cursor position. |
| `json_serialize value:$JsonValue -> String` | Serialize a tree. |
| `json_set value:JsonValue key:String map:Map[String JsonValue] -> Map[String JsonValue]` | Add or replace a member and return the map. |

The `json_get_*` helpers return `None` for a missing member or the wrong value
kind. Borrowed results keep the input tree loaned until their last use.
`json_parse` leaves trailing content for the caller. Use `deserialize` when
reading a complete JSON document.
