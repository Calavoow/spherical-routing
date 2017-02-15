# spherical-routing
Routing on a spherical polygon.

## How to run
Install Scala Build Tool (sbt).
Instructions can be found here: http://www.scala-sbt.org/0.13/docs/Setup.html
Then run:

```
$ sbt
> compile
> test
> run ring.out sphere.out
```

The command `test` will run the currently active test suites,
including some that verify correctness of the algorithms and their implementations.
The `run [ring output] [sphere output]` will output simulation information to the files given as arguments.

The output can be interpreted as a csv with 2 columns followed by `x` simulation results,
where `x` is the number of iterations.
The output structured can be summarized as follows:

| Nodes | concurrent paths | results... |
| ----- | ---------------- | ---------- |
| Number of nodes in this simulation | Number of concurrent paths used in this simulation | A list of the layers in which a collision ocurred for this iteration. -1 Indicates no collision. |

## Processing of results
Results can be graphed using the scripts in the `doc/` directory.
Specifically, the `stackedbar.m` was used for the paper.
The data in `doc/sphere.csv` was used to generate the figure in the paper.

