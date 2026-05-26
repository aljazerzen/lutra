# Lutra website documentation

This directory contains the website source for Lutra.

## Structure

The website has two content entry points:

- `content/_index.md` — the Zola landing page for `https://lutra-lang.org/`
- `docs/` — the Zensical documentation site for `https://lutra-lang.org/docs/`

Use them for different purposes:

- Put homepage and marketing-style content into `content/_index.md`.
- Put user-facing documentation into `docs/`.

## Generated reference pages

Pages under `docs/reference/language/std/` are generated from
`lutra-compiler/src/std.lt`.

## Documentation structure

The docs are organized by **user intent**:

- `usage/` — task-oriented guides for using Lutra from each entry point
- `learn/` — learning-oriented guides for writing Lutra language
- `reference/` — information-oriented reference for the language and the project internals
- `about/` — project meta information: motivation, design, status, comparisons

Use this structure consistently.

### `usage/`

Use `usage/` for **workflow guides**.
These pages should help a user accomplish a concrete task.

Examples:
- using the CLI to inspect or run data
- using Lutra from Python on tabular data
- using Lutra from Rust for typed serialization or execution

A usage page should answer questions like:
- What am I trying to do?
- What commands or code do I run?
- What files do I create?
- What result should I expect?

Usage pages may introduce a small amount of language syntax when needed, but they must not become full language tutorials.

### `learn/`

Use `learn/` for **learning how to write Lutra**.
These pages should be sequential, example-heavy, and optimized for understanding.

Examples:
- first program
- types and values
- functions and pipelines
- control flow and pattern matching
- collections and tabular data
- modules and projects

A learn page should answer questions like:
- How does this language feature work?
- When should I use it?
- What are the common patterns?
- How does this fit into the rest of the language?

Learn pages should prefer the local interpreter unless a remote backend is essential to the topic.
Avoid requiring PostgreSQL setup early in the learning path.

### `reference/`

Use `reference/` for **authoritative lookup documentation**.
This includes both:
- the **language reference**, and
- the **project reference** for runners, binary format, and related internals.

Reference pages should be precise, stable, and easy to scan.
They are not tutorials.

Examples:
- syntax and grammar-oriented pages
- literals, types, expressions, patterns, operators
- standard library reference
- runner model
- binary format and representation rules
- execution/data flow details

A reference page should answer questions like:
- What is the exact syntax?
- What values are allowed?
- What does this function/operator/type do?
- How is data represented across runner boundaries?

### `about/`

Use `about/` for **project narrative and meta information**.

Examples:
- motivation
- design principles
- status
- comparisons with related tools

These pages explain why Lutra exists and what tradeoffs it makes.
They are neither tutorials nor normative reference.

## Writing rules

### Optimize for a single reader intent per page

Every page should primarily serve one intent:
- accomplish a task
- learn a concept
- look up exact information
- understand project context

If a page tries to do too many of these at once, split it.

### Prefer small, focused pages over large mixed pages

A page should have a clear scope.
If a page grows to cover multiple unrelated topics, split it.

Examples:
- split language learning from PostgreSQL setup
- split syntax reference from standard library reference
- split project architecture from host-language workflows

### Write clearly and directly

- Write for developers with varying English proficiency.
- Prefer clarity over cleverness.
- Use second person: write to the reader as "you".
- Prefer active voice.
- Use conversational but professional language.
- Avoid jargon, filler, and buzzwords.
- Avoid words like "simply", "just", and "easy".
- Write "for example" instead of "e.g." and "that is" instead of "i.e.".
- Prefer "earlier" and "later" over "above" and "below".

### Use consistent structure and formatting

- Use sentence case for headings.
- Do not skip heading levels.
- Use numbered lists for steps and bulleted lists for unordered items.
- Introduce lists with a complete sentence and a colon.
- Avoid single-item lists.
- Use parallel structure within lists.

### Write procedures as procedures

For task-oriented pages:
- use numbered steps
- start steps with imperative verbs
- state context or location before the action
- mark optional steps with `Optional:`
- document one clear procedure instead of many alternatives in one place

### Format code, commands, and links consistently

- Use backticks for commands, filenames, paths, flags, function names, keywords, variables, and literal values.
- Do not inflect code elements in prose.
- Use descriptive link text.
- Avoid link text like "click here" or "this page".

### Cross-link intentionally

Use links to connect:
- usage pages → relevant learn pages
- learn pages → relevant reference pages
- reference pages → related project/about pages when useful

Links should help a user go deeper without forcing them to leave the current flow too early.
