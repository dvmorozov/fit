<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Tests for the `fitminimizers` package

The units under test live in a **separate repository**, checked out beside this
one as `../fitminimizers`. Their tests live here because this is the suite that
builds and runs them - the package has no suite of its own - and keeping them in
a directory named after the package is what makes that visible.

| test | subject |
|---|---|
| `testcase_simplex.pas` | `DownhillSimplexAlgorithm` - the optimiser the engine is built on |
| `testcase_simplex_server.pas` | `DownhillSimplexServer` - flattening several curves' parameters into one vector |
| `testcase_decisions.pas` | `Decisions` - the discrete-choice containers |
| `testcase_simpmath.pas` | `SimpMath` - the lineshapes and numeric helpers |
| `testcase_running_thread.pas` | `RunningThread` |

**The package is IN the coverage target**, unlike the other sibling: it is the
pure-math optimiser this engine rests on, and the figure counts it. `fitgrids` is
excluded instead - see `tools/coverage/vendor.txt` - on the grounds that a number
which moves when a separately published component grows is measuring the wrong
thing. That is why there is no `tests/fitgrids/`.

A test here is an ordinary member of the suite: it is registered into `unit` or
`integration` like any other, and `fit_tests.lpr` names it in the same uses
clause. Only the directory differs.
