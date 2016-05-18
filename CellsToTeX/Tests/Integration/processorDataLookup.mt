(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`processorDataLookup`", {"MUnit`"}]


Get["CellsToTeX`"]

$ContextPath =
	Join[
		{
			"CellsToTeX`Configuration`",
			"CellsToTeX`Internal`",
			"CellsToTeX`Backports`"
		},
		$ContextPath
	]


(* ::Section:: *)
(*Tests*)


Module[{testProcessor, testVar, a, b},
	Test[
		processorDataLookup[
			testProcessor[testVar = "evalaution leaked"], {a -> b}, a
		]
		,
		b
		,
		TestID -> "existing keys"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "existing keys: evalaution leak"
	]
]


Block[{testProcessor, testVar, a, b, c},
	Test[
		Catch[
			processorDataLookup[
				testProcessor[testVar = "evalaution leaked"], {a -> b}, {a, c}
			];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missingProcArg,
					"MessageParameters" -> {
						HoldForm @
							testProcessor[testVar = "evalaution leaked"],
						HoldForm @ CellsToTeXException[
							"Missing", "Keys", "ProcessorArgument"
						],
						HoldForm @ "Keys",
						HoldForm @ {"c"},
						HoldForm @ {"a"}
					},
					"Type" -> {"Missing", "Keys", "ProcessorArgument"}
				]
			],
			CellsToTeXException["Missing", "Keys", "ProcessorArgument"]
		}
		,
		TestID -> "missing keys"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "missing keys: evalaution leak"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[processorDataLookup], Protected]
	,
	True
	,
	TestID -> "Protected attribute"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
