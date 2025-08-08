---
title: Relation to PRQL
weight: 2
---

PRQL is a pipelined relational query language, in some ways similar to Lutra. It was the
language I was working on before starting Lutra.

There is a few key differences:
- Lutra requires database schema definitions and does not ever infer tables or columns.
  It allows fully typed queries, where each expression can be proven to have exactly one type.
- Lutra generates SQL that is not meant to be readable.
  While PRQL can be used as a "tool to write SQL with", Lutra cannot.
  Lutra does not put emphasis on SQL being human-readable; it uses sequential numbers
  to name columns & relations, it heavily uses subqueries and sometimes uses unconventional
  representations of certain data types.
- There are many syntactical differences,
  for example function calls require parenthesis and commas in Lutra.

---

When I was working on the PRQL compiler, I was trying to formalize the language with as few
syntactic and semantic rules as possible. That has lead to some interesting rules and even
more interesting chunks of the compiler.

A good example of this was what I called *implicit closure*, which was my explanation for
the fact that within `select {...}`, one can refer to columns of the input relation.
There is no syntactical indication that these column reside within the input relation.
My (and the compiler's) rationale was that the example is expanded to:

```
select (func this -> {...})
```

... with a special rule that name `this` is the *default scope*. So this query:

```
select {x}
```

... would be resolved as:

```
select (func this -> {this.x})
```

After a few more such formalizations, the compiler got quite complex and confusing. That slowed
down the development and prompted me to *temporarily* break some features so I could fit-in new ones.
That does not really work if people already use the language, which caused me a lot of frustration.

---

In the end, I've decided to start anew and embraced all the breaking changes. I've changed syntax, removed
rules that I deemed to complicated and named the result Lutra. That opened doors for fully-typed queries,
generic type parameters, Hindley–Milner-like type inference and many other things.

Looking back, I find that we approached the language design from the wrong angle.
We have started with query examples and then tried to find language rules that would fit them.
Instead we should have started with simple rules, seeing what queries do they allow and then
coming up with ideas of how to change the rules to push the language further.

Or maybe, the problem was that I wanted PRQL to be too *general*, for the lack of better word.
I wanted to define what a PRQL expression is and then support using that in all possible locations.
For example, `1 + 2` is a valid SQL expression, but `SELECT ... FROM 1 + 2` is not valid SQL, even syntactically.
In general purpose languages, you can pass any expression to any function call (given the types match),
and I wanted that for PRQL.

In any case, PRQL remains very convenient domain-specific-language for relational data, while Lutra
is trying to push into general-purpose language teritorry, sometimes at the cost of ease of use when
dealing with relations.
