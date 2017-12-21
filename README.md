# Quantum state engineering with QW - code

Code and data used for the paper: L. Innocenti, H. Majury, T. Giordani, N. Spagnolo, F. Sciarrino, M. Paternostro, A. Ferraro, *Quantum state engineering using one-dimensional discrete-time quantum walks* ([Phys. Rev. A 96, 062326 (2017)](https://journals.aps.org/pra/abstract/10.1103/PhysRevA.96.062326), [arXiv:1710.10518 [quant-ph]](https://arxiv.org/abs/1710.10518)).

## Requirements
Many functions used here rely on functionality provided by the Mathematica packages [`QM`](https://github.com/lucainnocenti/QM) and [`QuantumWalks`](https://github.com/lucainnocenti/QuantumWalks).
Both of these packages can be installed by simply `git clone`ing the repositories into the directoy given by Mathematica evaluating `FileNameJoin@{$UserBaseDirectory, "Applications"}`.
Furthermore, the [`MaTeX`](https://github.com/szhorvat/MaTeX) package is required for a few functionalities (but this should be automatically installed by `QM`).


## Package files:
- `reachabilitySolutions.m`: Defines a set of function to easily handle the analytical reachability conditions to obtain target superpositions after projection over the + state. Mainly used in `reachabilityConditions.nb`.
- `searchCoinParameters.m`: Defines the function `searchCoinParameters`, which is used to run the numerical optimization procedure to find sets of coin parameters generating a target superposition. A number of utilities to handle and process the output of `searchCoinParameters` are also defined here.
- `stabilityAnalysis.m`: Defines the function `plotFidelityVsParameters`, which can be used to plot the fidelity as a function of variations of single parameters.

## Notebooks:
- `reachabilityConditions.nb`: In this notebook we find and study the analytical solutions that give target superpositions after projection. For a few steps the equations can solved analytically, while for more steps (up to 5 here) they are solved numerically.
- `numericalMaximization.nb`: For more then a few steps the methods in `reachabilityConditions.nb` are not viable anymore. Here a new function, `searchCoinParameters`, is used to solve via optimization the equations for up to 20 steps (and possibly more). This is done writing the output fidelity between target state and obtained state as a function of the set of coin parameters of the walk. After this, `NMaximize` or `FindMaximum` can be used to find sets of solutions. To boost the efficiency the fidelity function is compiled in `C`.
