<p align="center">
  <img src="./asset/logo-optimized.svg" alt="Ibidem logo" height="130px"><br><br><br>
  <a href="https://travis-ci.com/stefandevai/ibidem"><img src="https://img.shields.io/travis/stefandevai/ibidem?style=flat-square" alt="Build Status"></href>
  <a href="https://travis-ci.com/stefandevai/ibidem"><img src="https://img.shields.io/badge/License-MIT-blue.svg?style=flat-square" alt="License: MIT"></href>
</p>

CLI tool that makes it easier to write academic papers with citations. It uses a markdown to LaTeX conversion.

## What is it about?

The purpose of this utility is to automate the formatting of academic articles or essays:

- We write a file on a [superset](#superset-of-markdowns-syntax) of [markdown's syntax](https://daringfireball.net/projects/markdown/).
- *latex-builder* converts it to a basic *LaTeX* file.

## How to use?

Currently it has only been tested in Linux and OS X using SBCL.

### First option: *build an executable*

1. Make sure that you have [SBCL](http://www.sbcl.org/), [quicklisp](https://www.quicklisp.org/beta/) and [asdf](https://common-lisp.net/project/asdf/) installed.

2. Run `git clone https://github.com/stefandevai/latex-builder && cd latex-builder` on a terminal.

3. Run `make && cd bin/` to build the program.

4. Run `./latex-builder input-path -o output-path`, where `input-path` is the markdown file and `output-path` is the path and name of the output `.tex` file.

### Second option: *run from the REPL*

1. Make sure that you have [SBCL](http://www.sbcl.org/), [quicklisp](https://www.quicklisp.org/beta/) and [asdf](https://common-lisp.net/project/asdf/) installed.

2. Run `git clone https://github.com/stefandevai/latex-builder && cd latex-builder` on a terminal.

3. Run the SBCL's REPL and enter `(load "latex-builder.asd")`.

4. Run `(latex-builder:create input-path output-path)` where `input-path` is the markdown file and `output-path` is the path and name of the output `.tex` file.


## Superset of Markdown's syntax

We use a superset of Markdown's syntax in order to allow meta information about the text, citations and bibliography sources.

### Header block

If we want to have a latex header, we write a header at the beginning of the markdown file containing the following format and parameters:

~~~markdown
---
author: "Your Name"
date: "25/10/1917"
location: "A location"
---
~~~

### Bibliography block

If we want to have citations in our text, we can write a bibliography block. This can usually be placed at the end of the file:

```markdown
~~~bibliography
---
id: "[source-unique-id]"
type: "[citation-type]"
author: "Author's Full Name"
title: "Source's Title"
journal: "Source's Journal"
year: "Publication year"
volume-issue: "[volume]([issue])"
---

---
id: "jacoby"
type: "article"
author: "William G. Jacoby"
title: "Public attitudes toward government spending"
journal: "American Journal of Political Science"
year: "1994"
volume-issue: "38(2)"
---
~~~
```

Each block piece is described in isolation here:

```markdown
~~~bibliography
[This is a bibliography block. It expects citation source blocks.]
~~~

~~~bibliography
---
[This is a citation source block. It expects parameters about a single source.]
---

---
[You can have multiple citation source blocks.]
---
~~~
```

### Citation

Within the text, you can have citations in the following format:
`c[source-unique-id:pages]`

E.g.
`c[jacoby:334-336]`
