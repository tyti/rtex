# rtex

rtex is a reimplementation of TeX82. It covers nearly all features of TeX82
(see limitations) and should in principal produce identical output files.

This is a personal project with the purpose of testing how well TeX can be
implemented in Rust as well as a learning opportunity for both of these
languages. While I am not currently looking for contributions to this project,
I am happy for any feedback and suggestions.

## Limitations

* As this project only implements TeX82, it lacks a number of features expected
  of modern TeX engines, e.g. pdfTeX or LuaTeX, and is not a drop-in
  replacement for those. Importantly, it lacks integration with Kpathsea and
  only looks for files in a few hard-coded directories.
* It uses some Unix-specific functions and has only been tested on Linux.
* Time and date facilities are not implemented and the timestamp is fixed to
  noon, July 4, 1776.
* The option to edit the current input file during interactive error recovery
  (option 'E') has been removed.

## Trying it out

If you would like to try it out, you need to have a Unix system with a recent
version of Rust (1.74 or later) installed. To run it, clone this git repository
locally, change the working directory to `rtex/` and run

```
cargo run
```

Note though that there is no format preloaded and no fonts are preinstalled. To
make a font available, put the corresponding TFM file into `rtex/fonts`.

To create a format file from e.g. `plain.tex`, put the necessary TFM files into
`rtex/fonts` and a copy of `plain.tex` and `hyphen.tex` into `rtex/`. You are
likely to find copies of these files in your local TeX installation or online.
If you then run

```
cargo run 'plain.tex \\dump'
```

the format file `plain.fmt` should be created. You can start with the format
preloaded by running

```
cargo run '&plain'
```

to run interactively or

```
cargo run '&plain file.tex'
```

with an input file `file.tex`.

