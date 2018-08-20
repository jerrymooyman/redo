# redo

This is an implementation of djb's redo in haskell

## Usage

redo *target*

i.e. redo install

## Generating documentation

```bash
$> cabal haddock --executable
```
Then open generated index.html in web browser.

### Haddock tips
* `adding options_haddock prune` only generates documentation for function that contain documentation.
* by correctly formatting type signatures, haddock will generate documentation for function arguments.
* to generate documentation for executables run haddock with `--executable` option
* haddock from commandline may have trouble finding deps, so use haddock via cabal (see example above)
* haddock will complain if main function is not exported from the module


## Background
This project was built by following Jekor's haskell from scratch tutorial.

* episode 1


* episode 2

* episode 3

* episode 4

* episode 5

* episode 6

* episode 7

* episode 8

* episode 9

ref: https://github.com/jekor/redo/tree/ep09

* episode 10
- recusion. rebuilding dependencies if required
- building an opensource project (redis) by using redo instead of make.

ref: https://github.com/jekor/redo/tree/ep10

* episode 11
- replacement of Make

ref:

https://www.youtube.com/watch?annotation_id=annotation_911185&feature=iv&src_vid=nHZ7CtFnI48&v=yH3xQt7XUrc


* episode 12
- cabal and hackage
- packaging up project and uploading to hackage
- resolving deps for distribution


* episode 13
- documentation and haddock





