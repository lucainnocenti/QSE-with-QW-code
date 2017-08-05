BeginPackage["stabilityAnalysis`", {"QM`", "QuantumWalks`"}];

Unprotect @@ Names["stabilityAnalysis`*"];
ClearAll @@ Names["stabilityAnalysis`*"];

plotFidelityVsParameters;

Begin["`Private`"];

texStyle = Directive[
  GrayLevel @ 0,
  {FontFamily -> "CMU Serif", FontSize -> 20, FontWeight -> Plain, FontSlant -> Plain}
];


(* Simple utility converting {1, i} into \[Theta]_i, and so on. *)
parIndicesToLabel[{row_, col_}] := (
  Subscript[col /. {1 -> "\[Theta]", 2 -> "\[Xi]", 3 -> "\[Zeta]"}, row]
);


(* Expect as input a set of coin parameters, and a position in that list,
   and return the same set of parameters with the specified element(s) perturbed
   according to the value of `typeOfPerturbation`.
   The output will contain the symbolic parameter `eps`, controlling the
   perturbation.*)
injectEpsInPars[coinPars_, pos_, typeOfPerturbation_ : Times] := ReplacePart[
  coinPars,
  pos :> typeOfPerturbation[eps, coinPars[[Sequence @@ pos]]]
];


(* Starting from a given set of coin parameters, use `injectEpsInPairs` to
   apply a perturbation (parametrized by `eps`), to the coin parameter given
   with `changingPar`, then evolve a walker using these parameters,
   project at the end according to the value of `projection`, and finally
   compute the fidelity with the target state `targetState`. *)
Options @ fidelityWithEps = {"InitialCoinState" -> {1, 0}};
fidelityWithEps[
  coinParameters_, changingPar_, targetState_, op_, projection_, OptionsPattern[]
] := QFidelity[
  QWEvolve[OptionValue @ "InitialCoinState",
    injectEpsInPars[coinParameters, changingPar, op]
  ] // QWProjectCoin @ projection,
  targetState
];


Options @ makeLabelForLine = {"HighlightZeros" -> False};
makeLabelForLine[coinParameters_, changingPar_, OptionsPattern[]] := Labeled[
  #,
  Tooltip[
    Style[parIndicesToLabel @ changingPar,
      Bold, texStyle,
      If[
        And[
          TrueQ @ OptionValue @ "HighlightZeros",
          coinParameters[[Sequence @@ changingPar]] == 0
        ],
        Red, {}
      ]
    ],
    coinParameters[[Sequence @@ changingPar]]
  ]
]&;


Options[plotFidelityVsParameters] = {
  "TypeOfPerturbation" -> Times,
  "ParametersToPlot" -> {1, 3},
  "Projection" -> {1, 1}
};
Options[plotFidelityVsParameters] = Normal @ Association @ Join[
  Options @ Plot,
  Options @ plotFidelityVsParameters,
  Options @ makeLabelForLine
];
plotFidelityVsParameters[coinParameters_List,
  targetState_List, {epsMin_, epsMax_}, opts : OptionsPattern[]
] := With[{
    expressions = #[[;;]]& @ Table[
      fidelityWithEps[
        coinParameters, changingPar, targetState,
        OptionValue @ "TypeOfPerturbation", OptionValue @ "Projection"
      ] // makeLabelForLine[coinParameters, changingPar,
            "HighlightZeros" -> OptionValue @ "HighlightZeros"
          ],

      {
        changingPar,
        Tuples[
          {Range @ Length @ coinParameters, OptionValue @ "ParametersToPlot"}
        ]
      }
    ] ~ Monitor ~ parIndicesToLabel @ changingPar
  },

  Plot[expressions,
    {eps, epsMin, epsMax},
    PlotRange -> All,
    Evaluate @ FilterRules[{opts}, Options @ Plot],
    AxesStyle -> texStyle
  ]

];


(* `nMaximize*` functions work on the `Association` given in output by
   `searchCoinParameters`.
   These `nMaximizeResults` have the following structure:
   ```
   <| "FinalFidelity" -> 1., "CoinParameters" -> {...}, "TargetState" -> {...} |>
   ```
*)

nMaximizeResultsGetProjection[nMaximizeResults_Association] := With[{
    coinParameters = nMaximizeResults["CoinParameters"]
  },
  If[MemberQ[coinParameters[[All, 1]], Global`\[Alpha]],
    {Global`\[Alpha], Sqrt[1 - Global`\[Alpha]^2]} /. coinParameters,
    Normalize @ {1, 1}
  ]
];


nMaximizeResultsFormatCoinPars[nMaximizeResults_Association] := With[{
    coins = nMaximizeResults["CoinParameters"]
  },
  PadRight[#, 3]& /@
    Partition[#, 2]& @ DeleteCases[coins, \[Alpha] -> _][[All, 2]]
];


plotFidelityVsParameters[
  nMaximizeResults_Association,
  {epsMin_, epsMax_},
  opts : OptionsPattern[]
] /; (
  KeyExistsQ[nMaximizeResults, "CoinParameters"]
) := plotFidelityVsParameters[
  nMaximizeResultsFormatCoinPars @ nMaximizeResults,
  nMaximizeResults["TargetState"],
  {epsMin, epsMax},
  "Projection" -> nMaximizeResultsGetProjection @ nMaximizeResults,
  opts
];

plotFidelityVsParameters[
  {epsMin_, epsMax_}, opts : OptionsPattern[]
][result_Association] := plotFidelityVsParameters[
  result, {epsMin, epsMax}, opts
];


End[];
Block[{$ContextPath}, EndPackage[]];
