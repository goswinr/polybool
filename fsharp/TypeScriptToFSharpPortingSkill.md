# Skill: TypeScript to F# Direct Translation


## File Order and declaration order

Try to keep the file order in F# close to the original TypeScript module order, but within each file, ensure that types and functions are declared before they are used. This may require reordering some declarations or splitting code into multiple files to avoid circular dependencies.
In F# files, declaration order matters. Types and functions must be declared before they are used. This is different from TypeScript, where imports/exports and declaration hoisting can hide ordering issues. When porting TypeScript to F#, we need to ensure that the order of types and functions in the F# files allows for successful compilation without forward references.


## Translation Rules


### Preserve TypeScript Shape
- Keep classes, interfaces, type aliases, enums, and modules as their closest F# equivalents.
- Keep mutable fields mutable. Use `let mutable`, mutable record fields, or mutable properties.
- Keep imperative control flow (`while`, `for`, `if/else`, early return via mutable + condition`).
- Do NOT convert to idiomatic F# (`|>` pipelines, `Option` everywhere, point-free style, etc.) unless required for correctness.


### Modules and Exports
- Preserve module boundaries where practical. A TypeScript file should usually become one F# module or namespace.
- Keep exported names recognizable so downstream code remains easy to map back to the source.
- `export default` should become a normal named F# binding or type with a stable, explicit name.
- Flatten re-export patterns only when F# module ordering requires it.

### Numeric and JavaScript Parity
- TypeScript `number` is a floating-point type. Default to F# `float` unless the source clearly requires integral semantics.
- Preserve explicit numeric conversions; F# does not auto-widen.
- Preserve rounding and truncation behavior exactly. Only swap JavaScript/TypeScript helpers for .NET helpers when the semantics actually match.
- Keep `NaN`, `Infinity`, and signed-zero behavior in mind when translating numeric edge cases.
- TypeScript `bigint` should stay `bigint` in F#.

### Null and Undefined Handling
- TypeScript `null`, `undefined`, and omitted optional values are not always interchangeable. Preserve whichever distinction the source relies on.
- Reference types that must allow `null` should use `[<AllowNullLiteral>]` on their type declaration.
- Use `isNull x` or `not (isNull x)` instead of direct null comparison.
- Use `Unchecked.defaultof<_>` for direct null/default-style behavior when needed.
- Use `option<'T>` only when the TypeScript code is modeling a real optional value, not just because F# offers it.
- Optional object properties may need either explicit presence tracking or `option`, depending on how the source checks them.

### Properties and Methods
- TypeScript `get`/`set` accessors become `member this.Prop with get() = ... and set(v) = ...`.
- TypeScript `static` methods become `static member`.
- TypeScript `private` members should stay restricted in F#.
- TypeScript `protected` members may need `member internal` when equivalent visibility is required.
- TypeScript `override` stays `override` in F#.

### Interfaces, Object Shapes, and Union Types
- TypeScript interfaces that describe callable or object contracts should stay as close as possible to F# interfaces or records.
- Plain object shapes used as data containers often translate well to records.
- String-literal unions and tagged object unions can become discriminated unions when that is the closest direct representation.
- Avoid redesigning structural types into more idiomatic F# shapes unless the original shape cannot be represented directly.

### Collections and Types
- Do not use F# lists by default.
- TypeScript `Array<T>` should usually become `T[]` for array semantics, or `ResizeArray<T>` when the source relies on list-like mutation methods (push, pop, shift, unshift, splice, etc.).
- TypeScript `ReadonlyArray<T>` should remain non-mutated in the F# translation.
- TypeScript `Map<K, V>` and `Set<T>` should map to .NET collection types with similar mutation and lookup behavior.
- TypeScript `Record<string, T>` and index-signature objects often need `Dictionary<string, T>` or another explicit key/value representation.

### Functions and `this`
- Arrow functions capture lexical `this`; preserve that behavior when translating closures.
- If a TypeScript function depends on dynamic `this`, translate that dependency explicitly instead of assuming normal F# function behavior is equivalent.
- Keep overload-like APIs recognizable, even if F# requires separate members, overloads, or optional parameters.

### Async and Promises
- TypeScript `Promise<T>` should become either `Async<T>` or `Task<T>` depending on the surrounding F# codebase. Stay consistent within the port.
- Preserve the sequencing semantics of `await`, especially inside loops and error handling.
- Do not silently parallelize work that was originally awaited in order.

### Loops and Control Flow
- `for (let i = 0; i < n; i++)` = `for i = 0 to n - 1 do`.
- `for (let i = n; i >= 0; i--)` = `for i = n downto 0 do`.
- `while` loops stay as `while ... do`.
- TypeScript `break` / `continue` / early `return`: use a mutable flag pattern since F# lacks these. Example:
  ```fsharp
  let mutable continueLoop = true
  let mutable i = 0
  while i < n && continueLoop do
      if condition then
          continueLoop <- false
      else
          i <- i + 1
  ```
- TypeScript `switch`: use `match ... with` or `if/elif/else`.
- TypeScript ternary `a ? b : c` = `if a then b else c`.

### Operators
- TypeScript `&&` = F# `&&`; TypeScript `||` = F# `||`.
- TypeScript `&` (bitwise) = F# `&&&`; TypeScript `|` (bitwise) = F# `|||`.
- TypeScript `<<` = F# `<<<`; TypeScript `>>` = F# `>>>`.
- TypeScript `~` (bitwise not) = F# `~~~`.
- Be careful with equality. TypeScript `===` / `!==` do not always map directly to F# structural equality.
- If the source depends on object identity, preserve that explicitly instead of using normal F# `=`.

### Enums
- TypeScript numeric enums with explicit values should remain explicit in F# enums.
- String enums are often better represented as discriminated unions or named constants than as F# enums.
- F# enum and union case names should start with an uppercase letter.

### Comments
- Preserve all comments as they are, including doc comments.

### Strings and Formatting
- Prefer `$"..."` string interpolation in F#, especially when porting TypeScript template literals.
- Keep formatting behavior explicit when the source relies on padding, radix conversion, or locale-sensitive formatting.

### Exception Handling
- TypeScript `try { ... } catch (e) { ... }` = F# `try ... with | ex -> ...`.
- Preserve the original exception flow; do not replace it with option/result types unless the source already models failure that way.


### Optional and Rest Parameters
- TypeScript optional parameters and default values should stay explicit in F# through optional parameters, overloads, or default-handling logic.
- Preserve `undefined`-driven behavior when it affects control flow.
- TypeScript rest parameters should become array parameters or `[<ParamArray>]` when appropriate.

### Generics
- Preserve generic parameters and constraints explicitly.
- TypeScript `extends` constraints should become the closest F# generic constraints that preserve the original intent.

## Return Types
- Always add the return types explicitly to all functions and members, even if they could be inferred. This improves readability and keeps the F# code close to the TypeScript source in terms of visible shape.