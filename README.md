
<div align="center">
  <a href="https://lutra-lang.org">
    <img src="./website/static/lutra.png" alt="Logo" width="80" height="80">
  </a>

  <h2 align="center">Lutra</h2>

  <p align="center">
    General-purpose query language
    <br />
    <a href="https://lutra-lang.org/">Home</a>
    &middot;
    <a href="https://lutra-lang.org/docs/">Docs</a>
    &middot;
    <a href="https://codeberg.org/lutra/lutra">Code</a>
    &middot;
    <a href="https://lutra.zulipchat.com/">Zulip chat</a>
  </p>
</div>

---

Lutra is a language for preserving type information between different software components.
It is a high-level, statically typed language, designed for querying data
and expressing data structures.

```lt
type Album: {id: int16, title: text}

func get_albums(): [Album] -> std::sql::from("albums")

func get_album_by_id(album_id: int16): Album -> (
  get_albums()
  | find(this -> this.id == album_id)
)
```

It is minimal and designed to be extended to new execution targets.
Currently, it can run on a reference-implementation interpreter and PostgreSQL.


## Project status

Lutra is a personal passion project, currently in a proof-of-concept stage.
It is not ready for production use.
It works, but is not feature complete and will change in inconvenient ways.

Feel free to try it out or come and chat with me at [Zulip](https://lutra.zulipchat.com/).

