---
title: Patterns
---

Patterns are used in `match`.

## Literal patterns

A literal pattern matches the same literal value.

```lt
match name {
  "world" => "Hello world!",
  n => f"Hello, {n}!"
}
```

In this example, `"world"` is a literal pattern.

## Binding patterns

A binding pattern matches any value and binds it to a name.

```lt
match name {
  "world" => "Hello world!",
  n => f"Hello, {n}!"
}
```

In this example, `n` matches every remaining value.

## Wildcard patterns

The wildcard pattern `_` matches any value and does not bind it.

```lt
match a {
  .cat | .hamster => false,
  _ => true,
}
```

## Enum variant patterns

An enum variant pattern uses the same variant spelling as enum construction:

```lt
.done
.pending(x)
.dog(.collie(name))
```

Examples:

```lt
match status {
  .done => 0,
  .pending(x) => x,
  .cancelled => 0,
}
```

```lt
match animal {
  .cat(name) => f"Hello {name}",
  .dog(.generic) => "Who's a good boy?",
  .dog(.collie(name)) => f"Come here {name}",
}
```

## Alternative patterns

Use `|` inside a pattern to match any of several alternatives.

```lt
match a {
  .cat | .hamster => false,
  _ => true,
}
```

Alternatives can also bind the same name through different shapes:

```lt
match a {
  .cat(name) | .dog(.collie(name)) => name,
  _ => "<unnamed>",
}
```

## See also

- [Expressions](expressions.md)
- [Types](types.md)
