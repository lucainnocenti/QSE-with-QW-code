BeginPackage["reachabilitySolutions`", {"QM`", "QuantumWalks`"}];

Unprotect @@ Names["reachabilitySolutions`*"];
ClearAll @@ Names["reachabilitySolutions`*"];
ClearAll @@ Names["reachabilitySolutions`Private`*"];

(* Verify that a given state satisfies the reachability conditions *)
verifyReachabilityConditions;
makeConditionsForDs;
findDsGivingReachableState;
computeFullStateFromDsSolution;
computeFullStatesFromTarget;
computeProbabilitiesFromTarget;

(* Process obtained results *)
checkTargetFidelity;
recomputeWholeStates;
computeCoinParameters;

Begin["`Private`"];

generalTwoColumnsStateWithD[nRows_, u_: u, d_: d] := Join[
  {{u[1], 0}},
  Table[
     {u[rowIndex] - d[rowIndex], d[rowIndex]},
     {rowIndex, Range[2, nRows - 1]}
   ],
   {{0, u[nRows]}}
];


makeReachabilityConditions[nSteps_Integer, u_: u] := Table[
  Sum[
    u[k, 1] Conjugate @ u[nSteps + k - s, 1] +
    u[k + 1, 2] Conjugate @ u[nSteps + 1 + k - s, 2],
    {k, 1, s}
  ] == 0,
  {s, 1, nSteps - 1}
];


equalAllToZero[expressions_List, keepEqual_ : True] := If[
  TrueQ @ keepEqual,
  With[{expr = Chop @ #}, Defer[expr == 0]] & /@ expressions,
  Equal[#, 0] & /@ expressions
];

Options[verifyReachabilityConditions] = {"SeeConditions" -> True};
verifyReachabilityConditions[n_Integer] := verifyReachabilityConditions[
  Array[Subscript["u", ##]&, {n, 2}]
]

verifyReachabilityConditions[
  state : {{_, _}..}, OptionsPattern[]
] := ReplaceAll[
  makeReachabilityConditions[Length @ state - 1, u][[All, 1]],
  u[s__] :> state[[s]]
] // equalAllToZero[#, OptionValue @ "SeeConditions"]&;

verifyReachabilityConditions[stateKet_List, opts : OptionsPattern[]] := (
  verifyReachabilityConditions[Partition[#, 2]& @ stateKet, opts]
);


makeConditionsForDs[nSteps_Integer, us_List : None, d_ : d] /; (
  If[us =!= None, Length @ us == nSteps + 1, True]
) := Block[{target, u},
  If[us === None,
    target = u /@ Range[nSteps + 1],
    target = us
  ];

  Table[
    Sum[
      Plus[
        (target[[k]] - d[k]) * Conjugate[target[[nSteps + k - s]] - d[nSteps + k - s]],
        d[k + 1] * Conjugate @ d[1 + nSteps + k - s]
      ],
      {k, 1, s}
    ] == 0,
    {s, 1, nSteps - 1}
  ] /. {d[1] -> 0, d[nSteps + 1] -> Last @ target}
];


Options[findDsGivingReachableState] = Options[NSolve];
findDsGivingReachableState[target_List, d_ : d, opts : OptionsPattern[]] := With[{
    nSteps = Length @ target - 1,
    nTarget = Normalize @ target
  },
  NSolve[
    makeConditionsForDs[nSteps, nTarget, d],
    d /@ Range[2, nSteps],
    opts
  ]
];

findDsGivingReachableState[nSteps_Integer, d_ : d, opts : OptionsPattern[]] := (
  findDsGivingReachableState[
    First @ RandomUnitary[nSteps + 1], d, opts
  ]
);


computeFullStateFromDsSolution[targetKet_List, solutionForDs_List] := With[{
    fullState = generalTwoColumnsStateWithD[
      Length @ targetKet, targetKet[[#]] &
    ]
  },
  fullState /. solutionForDs
];


computeFullStatesFromTarget[targetKet_List] := With[{
    solutions = findDsGivingReachableState @ Normalize @ targetKet
  },
  Map[# / Norm @ Flatten @ # &][
    computeFullStateFromDsSolution[Normalize @ targetKet, #] & /@ solutions
  ]
];


computeProbabilitiesFromTarget[targetKet_List] := (
  QWProjectionProbability[#, {1, 1}]& /@
    computeFullStatesFromTarget @ Normalize @ targetKet
);


(* RESULTS PROCESSING
   ------------------ *)

(* Verify that projecting the whole states we obtain the target states *)
checkTargetFidelity[result_Association] := With[{
    wholeStates = result["WholeStates"],
    targetState = result["TargetState"]
  },
  QFidelity[
    QWProjectCoin[#, {1, 1}],
    targetState
  ] & /@ wholeStates
];


(* Recompute whole states from coin computed from them *)
recomputeWholeStates[result_Association] := recomputeWholeStates /@
  result["WholeStates"];
recomputeWholeStates[state : {{_, _}..}] := QWEvolve[{1, 0},
  Reverse @ QWComputeCoinParametersFromState @ state
];


(* Compute coin parameters for each (or a single) obtained solution *)
computeCoinParameters[result_Association] :=
  QWComputeCoinParametersFromState /@ result["WholeStates"];

computeCoinParameters[result_Association, n_Integer] :=
  QWComputeCoinParametersFromState @ result["WholeStates"][[n]];

End[];
Block[{$ContextPath}, EndPackage[]];
