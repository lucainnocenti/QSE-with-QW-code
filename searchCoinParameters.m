BeginPackage["searchCoinParameters`", {"QM`", "QuantumWalks`"}];

Unprotect @@ Names["searchCoinParameters`*"];
ClearAll @@ Names["searchCoinParameters`*"];
ClearAll @@ Names["searchCoinParameters`Private`*"];


searchCoinParameters;

searchResultGetProjection;
searchResultGetCoinParameters;
searchResultComputeProjectedOutput;
searchResultComputeFullOutput;
searchResultProjectionProbability;

searchResultTestFidelity;

Begin["`Private`"];


galpha = Global`\[Alpha];
gtheta = Global`\[Theta];
gxi = Global`\[Xi];
Protect @ Evaluate[galpha, gtheta, gxi];


conditionalPrint[message_String, flag_] := If[TrueQ @ flag, Print @ message];

globalizeSymbols[expr_] := expr /. (
    s_Symbol /; Context @ s === "searchCoinParameters`Private`"
  ) :> ToExpression @ SymbolName @ s;


(* The Old convention can give problem during the optimization, because alpha
   can go out of the range [-1, 1] thus resulting in non-physical projections.*)
projectionParametrization[alpha_, type_ : "New"] := Which[
  type == "Old", {alpha, Sqrt[1 - alpha ^ 2]},
  type == "New", {Cos @ alpha, Sin @ alpha}
]


(* Compute the general symbolic expression for the projection matrix *)
projectionMatrix[
  nSteps_, includeProjectionInMaximization_ : True, alpha_ : \[Alpha]
] := KroneckerProduct[
  IdentityMatrix[nSteps + 1],
  If[TrueQ @ includeProjectionInMaximization,
    KroneckerProduct[#, Conjugate @ #] & @ projectionParametrization @ alpha,
    ConstantArray[1 / 2, {2, 2}]
  ]
];


(* Build the expression computing the fidelity for a given target state using
   in a more procedural style, and then compile it down to C for better
   performance.
   Returns: the compiled function.*)
compileExpression[
  parametersForMaximization_, messyExpression_, projection_, targetState_
] := Hold @ Compile["compileInputs",
  Module[{projectedState, norm2OfProjectedState},
    projectedState = Dot[projection, messyExpression][[1 ;; "lengthProjection" ;; 2]];
    norm2OfProjectedState = Total @ Power[Abs @ projectedState, 2];
    Abs[Dot[
      Conjugate @ projectedState,
      targetState
    ]] ^ 2 / norm2OfProjectedState
  ],
  {{Normalize[_], _Complex, 1}},
  CompilationTarget -> "C",
  RuntimeOptions -> "Speed"
] /. {
  "compileInputs" :> RuleCondition @ {
    (* set of real inputs, one for each coin parameter *)
    Sequence @@ Map[{#, _Real} &, parametersForMaximization],
    (* 1d array of complex amplitudes, for the target state*)
    {targetAmplitudes, _Complex, 1}
  },
  "lengthProjection" :> RuleCondition @ Length @ projection
};


(* Take a list of parameters of the form {foo[1], foo[2], ...} and convert it
   into a "stringified" form, that is in this case, {foo1, foo2, ...} *)
stringifyParameters[parameters_] := Map[
  (* Only expressions of the form par[stuff] are stringified, while simple
     symbols are left untouched *)
  If[Head @ # =!= Symbol,
    ToExpression @ StringJoin[
      Context @ Evaluate @ Head @ #,
      ToString @ SymbolName @ Head @ #,
      StringJoin @@ (ToString /@ #)
    ]
  ]&,
  (* leave symbols untouched *)
  parameters
];


(* Take a list of parameters of the form {foo[1], foo[2], ...} and convert it
   into a pattern to be fed as inputs section of a function, that is:
   {foo1_, foo2_, ...}. *)
makeParametersIntoInputs[parameters_] := Map[
  Pattern[#, Blank[]] &,
  stringifyParameters @ parameters
];


(* If `searchCoinParameters` is used with caching disabled, then the fidelity
   is computed in the standard Mathematica way. `noncompiledExpression` is used
   to build this function.
   Return: a function taking a sequence of parameters as input and returning
           the corresponding fidelity. *)
noncompiledExpression[
  parametersForMaximization_, finalWalkerState_, projection_, targetState_
] := Module[
  {finalWalkerStateAfterProjection, overlapWithTarget, functionToMaximize},
  finalWalkerStateAfterProjection = Normalize[
    Dot[projection, finalWalkerState][[ ;; ;; 2]]
  ];
  overlapWithTarget = Dot[finalWalkerStateAfterProjection, targetState];
  ReleaseHold[
    Hold[
      functionToMaximize["coinParameters"] = Abs[overlapWithTarget] ^ 2
    ] /. {
      "coinParameters" -> Sequence @@ makeParametersIntoInputs @ parametersForMaximization
    }
  ];
  functionToMaximize
];


(*
  `searchCoinParameters` is the main function of this package.
  Given a number of steps and (optional) target state, it performs a numerical
  optimization looking for the coin parameters that generate the target state
  after suitable projection.

  Returns
  -------
  An association like the following:
  ```
  <|"FinalFidelity" -> 1., "CoinParameters" -> {
    \[Theta][1] -> -1.11382, \[Xi][1] -> 1.33178*10^-8,
    \[Theta][2] -> 1.45634, \[Xi][2] -> -1.04273*10^-7
  }, "TargetState" -> {0.107827, 0.969681, 0.219297}|>
  ```
*)
Options[searchCoinParameters] = {
  "MaximizationFunction" -> NMaximize,
  "PrintExecutionMessages" -> True,
  "IncludeProjectionInMaximization" -> True,
  "InitialCoinState" -> {1, 0},
  "CacheFinalExpression" -> True
};
Options[searchCoinParameters] = Normal @ Association @ Join[
  Options @ searchCoinParameters,
  Options @ FindMaximum, Options @ NMaximize
];

searchCoinParameters[
  nSteps_Integer, target_List: None, opts : OptionsPattern[]
] := Block[{
    \[Theta], \[Xi], \[Alpha],
    maximize,
    targetState,
    finalWalkerState,
    functionToMaximize, parametersForMaximization,
    resultOfMaximization
  },
  With[{
      (* `projMatrix` is the projection matrix.
       If the option "IncludeProjectionInMaximization" is True, then a general
       projection is used, and the parameters defining it included in the
       maximization procedure.
       Otherwise, the projection is fixed to be over |+\[RightAngleBracket] \[LeftAngleBracket]+|. *)
      projMatrix = projectionMatrix[nSteps,
        OptionValue @ "IncludeProjectionInMaximization"
      ],
      (* `parameters` contains (all and only) the coin parameters for every step *)
      parameters = Transpose @ {
        Array[\[Theta], nSteps], Array[\[Xi], nSteps],
        ConstantArray[0, nSteps]
      },
      initialCoinState = OptionValue @ "InitialCoinState"
    },
    (* `maximize` is only defined here to shorten the notation *)
    maximize = OptionValue @ "MaximizationFunction";
    (* Alert the user that we are starting to prepare the state, if the
      execution messages have been enabled *)
    conditionalPrint["Preparing state...", OptionValue @ "PrintExecutionMessages"];
    (* If the second positional argument `target` is not given, then a random
       target state is generated with ``QM`RandomUnitary`` *)
    If[target === None,
      targetState = First @ RandomUnitary[nSteps + 1],
      targetState = target
    ];
    (* Prepare list of parameters for the maximization *)
    parametersForMaximization = DeleteCases[0] @ Flatten @ parameters;
    If[TrueQ @ OptionValue @ "IncludeProjectionInMaximization",
      AppendTo[parametersForMaximization, \[Alpha]]
    ];

    (* ---- MAIN BLOCK ----
       Compute the expression for the final state of the walker, and build
       the function to be used for the maximization.
       If the "CacheFinalExpression" option has been given, this expression
       is only computed at the first call, after which the maximization
       function is compiled and cached for later use.
       If "CacheFinalExpression" is False, the expression is computed the
       same, but the expression obtained is not cached.
    *)
    If[
      Or[! TrueQ @ OptionValue @ "CacheFinalExpression",
        And[
          TrueQ @ OptionValue @ "CacheFinalExpression",
          ! ValueQ @ searchCoinParameters`Cache[nSteps]
        ]
      ],
      (* Evolve the walker for nSteps steps *)
      finalWalkerState = QWEvolve[initialCoinState, parameters];

      (* If caching is enabled, we compile and save for later the function
         for the maximization *)
      If[TrueQ @ OptionValue @ "CacheFinalExpression",
        conditionalPrint["Compiling function for caching...", OptionValue @ "PrintExecutionMessages"];
        (* Prepare arguments to compile the function to feed to `maximize` *)
        functionToMaximize = searchCoinParameters`Cache[nSteps] = ReleaseHold[
          compileExpression[parametersForMaximization, finalWalkerState, projMatrix, targetState]
        ],
        (* If caching is NOT enabled, we just define a regular function to
           be used later for the maximization *)
        functionToMaximize = noncompiledExpression[
          parametersForMaximization, finalWalkerState, projMatrix, targetState
        ]
      ],
      (* If CACHING IS ENABLED, and a cached value was found, then we just load
         the cached function (assuming it is a compiled function) *)
      functionToMaximize = searchCoinParameters`Cache[nSteps]
    ];
    (* Alert the user that we are starting the maximization,
    if the execution messages have been enabled *)
    conditionalPrint["Starting maximization...", OptionValue @ "PrintExecutionMessages"];
    (* Go with the maximization *)
    Quiet @ Module[{numericalFunction},
      If[Head @ functionToMaximize === CompiledFunction,
        numericalFunction[x__?NumericQ] := functionToMaximize[x, targetState],
        numericalFunction[x__?NumericQ] := functionToMaximize[x]
      ];
      (*Print @ Definition @ numericalFunction; Abort[];*)
      resultOfMaximization = maximize[
        numericalFunction @@ parametersForMaximization,
        parametersForMaximization,
        FilterRules[{opts}, Options @ maximize]
      ]
    ];
    (* Format and return results *)
    <|"FinalFidelity" -> resultOfMaximization[[1]],
     "CoinParameters" -> globalizeSymbols @ resultOfMaximization[[2]],
     "TargetState" -> targetState|>
  ]
];


(* Return True if the alpha projection parameter is explicitly present in the results*)
searchResultExplicitProjectionQ[searchResult_Association] := MemberQ[
  searchResult["CoinParameters"][[All, 1]], galpha
];


(* Return the projection stored in a result. If no explicit projection parameter
   is present, then it is assumed that the projection is over the balanced
   (1, 1) state. *)
searchResultGetProjection[searchResult_Association, projectionConvention_ : "New"] := With[{
    coinParameters = searchResult["CoinParameters"]
  },
  If[searchResultExplicitProjectionQ @ searchResult,
    Which[
      projectionConvention == "Old",
      {galpha, Sqrt[1 - galpha^2]} /. coinParameters,
      projectionConvention == "New",
      {Cos @ galpha, Sin @ galpha} /. coinParameters
    ],
    Normalize @ {1, 1}
  ]
];


(* Extract the values of the coin parameters from a result Association, and
   format them into a list of pairs. *)
searchResultGetCoinParameters[searchResult_Association] := With[{
    coinParameters = searchResult["CoinParameters"]
  },
  If[searchResultExplicitProjectionQ @ searchResult,
    (* If present, the parameter alpha is assumed to be the last in the list *)
    coinParameters[[;;-2]], coinParameters
  ][[All, 2]] // Partition[#, 2]&
];


Options @ searchResultComputeFullOutput = {
  "InitialCoinState" -> {1, 0}
};
searchResultComputeFullOutput[searchResult_Association, OptionsPattern[]] := With[{
    initialCoinState = OptionValue @ "InitialCoinState",
    coins = searchResultGetCoinParameters @ searchResult
  },
  QWEvolve[initialCoinState, coins]
];


Options @ searchResultComputeProjectedOutput = {
  "InitialCoinState" -> {1, 0},
  "ProjectionConvention" -> "New"
};
searchResultComputeProjectedOutput[searchResult_Association, OptionsPattern[]] := With[{
    initialCoinState = OptionValue @ "InitialCoinState",
    projection = searchResultGetProjection[searchResult, OptionValue @ "ProjectionConvention"]
  },
  searchResultComputeFullOutput[searchResult, "InitialCoinState" -> initialCoinState] //
    QWProjectCoin @ projection
];


(* Retest the fidelity using the full output computed by searchResultComputeFullOutput and
   the value of the "TargetState" field. *)
Options @ searchResultTestFidelity = {
  "InitialCoinState" -> {1, 0},
  "ProjectionConvention" -> "New"
};
searchResultTestFidelity[searchResult_Association, opts : OptionsPattern[]] := With[{
    target = searchResult["TargetState"]
  },
  searchResultComputeProjectedOutput[searchResult, opts] // QFidelity @ target
];


Options @ searchResultProjectionProbability = Options @ searchResultComputeProjectedOutput;
searchResultProjectionProbability[searchResult_Association, OptionsPattern[]] := With[{
    fullOutput = searchResultComputeFullOutput[searchResult, "InitialCoinState" -> OptionValue @ "InitialCoinState"],
    projection = searchResultGetProjection[searchResult, OptionValue @ "ProjectionConvention"]
  },
  fullOutput // QWProjectionProbability @ projection
];


End[];
EndPackage[];
