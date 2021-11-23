# Stupid Consensus

## Definition of Conensus problem
(Taken from https://www.cs.uic.edu/~ajayk/Chapter14.pdf)

- Agreement: All non-faulty processes must agree on the same (single) value.
- Validity: If all the non-faulty processes have the same initial value, then the agreed upon
value by all the non-faulty processes must be that same value.
- Termination: Each non-faulty process must eventually decide on a value.


## Current algorithm

- listen to messages until `Timeout` and stores them
- at `Timeout` decide on the first value received
- if no message received then stutter

=> We need to order messages