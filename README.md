# latex-builder

Focus on research and automate latex formatting.
The application is in it's early development stage.

## What is it about?

The purpose of this utility is to automate the formatting of academic articles or essays:

- We write a file on a [superset of markdown's syntax](#custom-markdown).
- *latex-builder* converts it to a basic *LaTeX* file.

## Progress

- [ ] Parse default markdown syntax
  - [x] Parse bold
  - [x] Parse italics
  - [x] Parse bold-italics
  - [x] Parse headers ("#" and "##")
  - [ ] Parse lists
  - [ ] Parse links
 
- [ ] Parse custom markdown syntax
  - [x] Parse header
  - [x] Parse bibliography
  - [~] Parse citations in text
  - [ ] Math functions

- [ ] Write full LaTeX syntax
  - [x] Main styles
  - [ ] Custom layout
  - [ ] Math functions

## How to use?

Currently it has only been tested in Linux and OS X using SBCL.

### First option: build an executable

1. Make sure that you have [SBCL](http://www.sbcl.org/), [quicklisp](https://www.quicklisp.org/beta/) and [asdf](https://common-lisp.net/project/asdf/) installed.

2. Run `git clone https://github.com/stefandevai/latex-builder && cd latex-builder` on a terminal.

3. Run `make && cd bin/` to build the program.

4. Run `./latex-builder input-path -o output-path`, where `input-path` is the markdown file and `output-path` is the path and name of the output `.tex` file.

### Second option: run from the REPL

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

~~~markdown
~--
--
id: "[source-unique-id]"
type: "[citation-type]"
author: "Author's Full Name"
title: "Source's Title"
journal: "Source's Journal"
year: "Publication year"
volume-issue: "[valume]([issue])"
--

--
id: "jacoby"
type: "article"
author: "William G. Jacoby"
title: "Public attitudes toward government spending"
journal: "American Journal of Political Science"
year: "1994"
volume-issue: "38(2)"
--
~--
~~~

Each block piece is described in isolation here:
~~~markdown
~--
[This is a bibliography block. It expects citation source blocks.]
~--

~--
--
[This is a citation source block. It expects parameters about a single source.]
--

--
[You can have multiple citation source blocks.]
--
~--
~~~
### Citation

Within the text, you can have citations in the following format:
`[source-unique-id:pages]`

E.g.
`[jacoby:334-336]`
