---
title: Implementation Details
---

This document contains technical details about the implementation of jcc.

#Gate Set

The gate set used by the compiler includes the NOT, CNOT, and Toffoli gates.

When choosing the implementation of various operations consideration is given to the expansion into the Clifford+T gate set.
More specifically to the minimization of T-gates.
For example shared controls on Toffoli gates are desired as they result in T cancellation.

#Addition

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
This allows it to overwrite one of its inputs with the resulting sum.

#Subtraction

Subtraction can be done simply by reversing the addition circuit.

#Multiplication

Multiplication is done with a simple shift and add circuit.
An adder adding the variables $$a$$ and $$b$$ consists of a CDKM[@CDKM:2004] adder controlled on each bit of $$a$$.
Each adder adds $$b$$ to some ancilla shifted left by the position of the control in $$a$$.
Each adder is smaller then the last as the multiplication is preformed $$\mod 2^n$$.
Below is an example of the compiler output for a multiplication:

```
$mult$
```

![](../images/mult.svg)

#Division
Unsigned division can be done with a long division algorithm.
Long divsion where `N` is the numerator, `D` is the divisor, `Q` is the quotient, and `R` is the remainder. 
```
D != 0
Q,R := 0                     
for i = n-1..0 do
    R := R << 1
    R[0] := N[i]
    if R >= D then
        R := R - D
        Q[i] := 1
```

#Conditional Statements

Conditional if-else branches can be evaluated by swapping the bits to the correct circuit path controlled on the if conditional.
The other circuit path is evaluated on a set of ancilla bits is initialized to:
\\[H^{\\otimes n}\\left|0\\right\\rangle\\]
Since this is an eigenvector of every permutation matrix it will be unchanged and can be cleaned up by reversing the initialization.


```
$ifExample$
```

![](../images/ifExample.svg)

Note that the assertion is a statement about the state of the program which is true id and only if the `if` branch is taken rather then the `else` branch.
This is useful as it describes a property of our data that can be used to clean up the bit used to store the result of the conditional.
i.e. If we know that we took that `if` branch rather then the `else` branch we can XOR the conditional bit and be assured it is being reset to zero.

#Loops
In the circuit model we need to implement all loop operations up to some max bound.
This is especially important in the Quantum model as some states in a superposition may require more loop iterations then others to evaluate.

So to implement a loop we want to repeatedly preform some computation until a condition is met.
We then want to stop preforming that computation for the remainder of the loop.

Swapping out of the loop using as done in [conditional statements](#conditional-statements) will not work since some of the superposition states will reset the condition bit.

