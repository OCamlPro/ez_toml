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

OCaml Interface
---------------

A typical application using :code:`ez_toml` will begin with::

  open Ez_toml.V1
  open TOML.Types

The first line means that the application is using version 1 of the
interface. Only the :code:`TOML` submodule is supposed to be used
afterwards.

The second line adds all the types manipulated in the :code:`TOML`
module to the environment. This is useful to directly access labels
and constructors. We advise not to open :code:`TOML` itself, and keep
using qualified names for all its functions.

The main type to be manipulated is :code:`node`. This type represents
a value in the TOML file, either the complete configuration file
itself, a section or a value assigned to a variable.

The :code:`TOML` module defines functions to create nodes, to get and
set values in tables, and to convert them to OCaml standard types.

A lower-level interface is also available through the use of the
:type:`value` type, that is stored in every :code:`node`. This
:code:`value` type is harder to use, as integers, floats and dates are
actually stored as strings inside, requiring conversions to use them.

Supported TOML Extensions
-------------------------

The :code:`ez_toml` library supports several extensions to the TOML
format. These extensions are not "standard" in any form. Using them
will, by default, trigger an error 21. Thus, applications wanting to
use them should add 21 to the :code:`silent_errors` set of the
:code:`config` parameter.

The following extensions are available:

* :code:`[]`: the empty section header is used to "re-open" the root
  section to add variables inside. Otherwise, it is not possible to
  extend the root section once another section/array has been opened.

* :code:`[!number]`: this header means that the error code
  :code:`number` is expected to be triggered by the next line. This
  annotation is used to test that the parser correctly rejects some
  sentences in the testsuite. Not all errors can be tested this way.
  An error will be raised if no error happened, or if another error
  was triggered. Since an error happened, the next line will not
  be visible in the result of the parsing.

* :code:`x == value`: Variable :code:`x` will only be set if it does
  not already exists. If it exists, no error will be raised and it
  will not be modified.

* :code:`x := value`: Variable :code:`x` will always be set, even if
  it already exists, without raising any error.

* :code:`x -= [ "y" ]`: The sub-variable :code:`y` will be removed (if
  it exsists) from the table :code:`x`.

* :code:`{} -= [ "y" ]`: The variable :code:`y` will be removed (if
  it exsists) from the root section.

Authors
-------

* Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>
