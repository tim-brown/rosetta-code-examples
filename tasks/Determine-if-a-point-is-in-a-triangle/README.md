# Determine-if-a-point-is-in-a-triangle

This is the Racket implementation of the: [Determine-if-a-point-is-in-a-triangle](https://rosettacode.org/wiki/Determine-if-a-point-is-in-a-triangle) task.

Wikipedia reference: ())

Racket has exact numbers in its numerical tower... so I don't see much motivation to accomodate
rounding errors. This is why the implementation _fails_ the second imprecise test, whereas other
implementations pass it. That point is very close to the edge of the triange. If your edge is fat
enough (epsilon), it will fall inside. If it is infinitessimal (i.e. exact), it is on the outside.

I would probably use the dot-product version, if only because it requires less (no) division.
