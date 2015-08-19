---
title: Janus Circuit Compiler
---

This language compiled is based on a modified version of the Janus programming language. Below are some references:

* [A reversible programming language and its invertible self-interpreter](http://doi.acm.org/10.1145/1244381.1244404)
* [JANUS: A Time-Reversible Language](http://www.tetsuo.jp/ref/janus.pdf)

Not all arithmetic operations are currently supported but are planned.

Due to the compilation target (no-feedback circuit model), loops are only planned to be supported in the case where they have fixed bounds.
Similarly support for recursion is not planned due to the difficulty of proving max bounds.
