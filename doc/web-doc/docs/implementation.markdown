---
title: Implementation Details
---

##Gate Set

The gate set used by the compiler includes the NOT, CNOT, and Toffoli gates.

When choosing the implementation of various operations consideration is given to the expansion into the clifford+T gate set.
More sepcifically to the minizmization of T-gates.
For example shared controls on toffoli gates are desired as they result in T cancellation.

##Addition

Addition is done using the CDKM[@CDKM:2004] adder as shown below:

```
$add$
```

![](../images/add.svg)

The final carry bit is not computed in this adder.
The result is an adder which computes:

\\[ a + b \\mod 2^n \\]

This is very similar to the classic ripple adder.
The main improvement is the realization that information about the carry can be stored in one of the input bits of each column.
This allows it to overwrite one of it’s inputs with the resulting sum.

##Subtraction

Subtraction can be done simply by reversing the addition circuit.

##Multiplication

Multiplication is done with a simple shift and add circuit:

```
$mult$
```

![](../images/mult.svg)

##Conditional Statments

Conditional if-else branches can be evaluated by evaluating the conditional statement then swapping the bits to the correct circuit path controlled on the result.

```
$ifExample$
```

![](../images/ifExample.svg)
