About
=====

This ez_toml library is a small library to read and write files in TOML
format. Compared to the toml library, it supports more features (mixed
arrays for example), fixes a few bugs in the printer, and keeps better
track of options order and comments in file.

TOML Format
-----------

For a description of TOML, see https://toml.io/en/

The toml-check tool
-------------------

The :code:`toml-check` tool is a very simple tool. It was initially
designed to test the parser/printers of :code:`ez_toml`, but finally
distributed as it could be useful on its own.

The :code:`toml-check` tool can be used to check the syntax of a TOML
file, to convert it to JSON, or to convert a JSON file to TOML
(typically, if you have a doubt on how to express something in TOML).
The tool uses :code:`ez_toml` to parse/print files, but can also be
asked to use the former :code:`toml` library for comparison.

For more information, use::

  toml-check --help


Comparison with Toml opam package
---------------------------------

Compared to the :code:`toml` opam package, :code:`ez_toml` contains
the following improvements:

* Simpler interface: keys are simple `string` types instead of abstract

* More conservative interface: integers, floats and dates are not
  parsed. Instead, they are returned as simple `string` types that
  have to be processed by the user.

* Better support for TOML:

  * Support for mixed arrays (the :code:`toml` library only allows
    monomorphic arrays)

  * Support for hexa, octal and binary integer notations

* Better pretty-printing:

  * Pretty-prints definitions in the same order the order as they were
    read (the :code:`toml` library pretty-pretty only prints definitions
    in alphabetical order)

  * Comments can be attached before and after definitions to be
    pretty-printed in the file. The parser will automatically attached
    such comments when definitions are read from a file.

Authors
-------

* Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>
