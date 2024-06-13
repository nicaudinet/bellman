# Generation problem

I first tackled the generation problem. I set the probability of transitioning
from GU (Good Unstable) to BP (Bad Permanent) is 0.1.

Finding the optimal policy for n=4 gives the following policy:

```
Stay -> Stay -> Stay -> Stay
```

Here, "Stay" means that we stay at the same state in this generation.
Conversely, "Go" means that if we are in state GU, we go to state BT (Bad
Temporary).

This policy sequence has the following possible trajectories and associated
probabilities:

```
(GU -> Stay -> GU -> Stay -> GU -> Stay -> GU -> Stay -> GU : 0.6561000000000001)
(GU -> Stay -> BP -> Stay -> BP -> Stay -> BP -> Stay -> BP : 0.1)
(GU -> Stay -> GU -> Stay -> BP -> Stay -> BP -> Stay -> BP : 0.09000000000000001)
(GU -> Stay -> GU -> Stay -> GU -> Stay -> BP -> Stay -> BP : 0.08100000000000002)
(GU -> Stay -> GU -> Stay -> GU -> Stay -> GU -> Stay -> BP : 0.07290000000000002)
```

The optimal policy for n=5 however gives a different result:

```
Go -> Stay -> Stay -> Stay -> Stay
```

and a single trajectory with probability 1.0:

```
(GU -> Go -> BT -> Stay -> GS -> Stay -> GS -> Stay -> GS -> Stay -> GS : 1.0)
```

For n>4 there seems to be a change in pattern. Whereas before it was worth
risking staying in GU to avoid the temporary cost of going to BT, after n=4 this
risk is no longer worth it, and it is instead better to go to BT immediately.
Note that the risk is dependent on the probability of transitioning from GU to
BP: the lower the probability, the longer the risk of staying in GU is worth
taking.

# Room walk problem

The second SDP I tackled is a simplified version of the room walk problem
presented [in these notes](https://teaching.csse.uwa.edu.au/units/CITS4211/Lectures/wk6.pdf).

The room looks as follows, where cell (3,2) gives a positive reward, cell (3,1)
gives a negative reward and all other cells give no reward.

```
   ----------------
 2 |    |    | +1 |
   ----------------
 1 |    |    | -1 |
   ----------------
     1    2    3
```

In each cell, the agent can choose to move up, down, left or right. However, the
agent will only move in that direction with a probability of 0.8. The other 20%
of the time, the agent will move either to the direction to the left or right of
the chosen direction with equal probability. If the agent chooses to move into a
wall, they stay in the same cell instead.

For example, if the agent is in cell (2,1) and chooses to go up, they will go to
cell (2,2) with probability 0.8, to cell (1,1) with probability 0.1 and to cell
(3,1) with probability 0.1:

```
        0.8
         ^
       -----
 0.1 < | A | > 0.1
       -----
```

Computing the optimal policy for this problem is much more difficult, as for
each step there are 4^6=4096 possible policies that need to be evaluated
(compared to 2 for the generation problem). My program was only able to compute
the optimal policy for n=1, which gives the following policy:

```
-------------
| > | > | ^ |
-------------
| > | < | ^ |
-------------
```

A few things to note here:
- If the agent starts in cell (3,2), the positive reward cell, then going up
  will give the highest change of staying in (3,2) without accidentally moving
  to (3,1), the negative reward cell.
- Cell (2,1) also points directly away from the negative reward cell
- All other cells point towards the positive reward cell

Starting at cell (1,1) we get the following trajectories:

```
(C11 -> > -> C21 : 0.8)
(C11 -> > -> C12 : 0.1)
(C11 -> > -> C11 : 0.1)
```

If we imagine repeating this policy into a policy sequence, I suspect it is
almost optimal. Starting at (1,1) we would get a loop between (1,1) and (2,1).
However, this loop would be broken by the small probability of moving to (1,2)
or (2,2). These two cells bring the agent directly to the positive reward cell.
The negative reward cell would be impossible to get to from outside, and the
agent would be immediately pushed to the positive reward cell if they start in
(3,1). However, I think that a more optimal strategy would be to go up in cell
(1,1). This way, rather than relying on the small transition probability the
agent would be immediately be pushed towards the positive reward cell.
