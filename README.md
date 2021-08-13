# simple-bytcode-vm

A simple bytecode compiler + VM for a Clojure - inspired lisp, written in Clojure.

Inspired by [this brilliant post](https://bernsteinbear.com/blog/bytecode-interpreters/).

## Usage

### Compile a file

    $ lein -compile examples/fact.lisp

### Run a compiled bytecode file

    $ lein -run examples/fact.lisp.edn 5

### REPL

    $ rlwrap lein -repl

## Future work

- JIT
- AOT bytecode -> native compiler

## License

Copyright © 2021 Divyansh Prakash

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
