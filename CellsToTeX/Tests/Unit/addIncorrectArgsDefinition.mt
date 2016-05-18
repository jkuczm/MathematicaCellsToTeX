(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`addIncorrectArgsDefinition`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[
		{
			"CellsToTeX`Package`",
			"CellsToTeX`Internal`",
			"CellsToTeX`Backports`"
		},
		$ContextPath
	]


(* ::Section:: *)
(*Tests*)


Module[{testFunc, testResult, testArg1, testArg2, oldDownValues},
	testFunc[arg_] := testResult[arg];
	oldDownValues = DownValues[testFunc];
	
	Test[
		addIncorrectArgsDefinition[testFunc]
		,
		Null
		,
		TestID -> "Symbol: addIncorrectArgsDefinition evaluation"
	];
	TestMatch[
		DownValues[testFunc]
		,
		Append[Verbatim /@ oldDownValues,
			Verbatim[HoldPattern] @ Verbatim[Pattern][
				pattName_,
				HoldPattern[testFunc][Verbatim[___]]
			] :>
				throwException[pattName_, {"Error", "IncorrectArguments"}]
		]
		,
		TestID -> "Symbol: DownValues"
	];
	
	Test[
		testFunc[testArg1]
		,
		testResult[testArg1]
		,
		TestID -> "Symbol: testFunction evaluation: correct args"
	];
	Test[
		Catch[
			testFunc[testArg1, testArg2];
			,
			_
			,
			HoldComplete
		]
		,
		expectedIncorrectArgsError[testFunc[testArg1, testArg2]]
		,
		TestID -> "Symbol: testFunction evaluation: incorrect args"
	]
]


Block[{testFunc},
	Test[
		addIncorrectArgsDefinition["testFunc"]
		,
		Null
		,
		TestID ->
			"String with symbol name: addIncorrectArgsDefinition evaluation"
	];
	TestMatch[
		DownValues[testFunc]
		,
		{
			Verbatim[HoldPattern] @ Verbatim[Pattern][
				pattName_,
				HoldPattern[testFunc][Verbatim[___]]
			] :>
				throwException[pattName_, {"Error", "IncorrectArguments"}]
		}
		,
		TestID -> "String with symbol name: DownValues"
	]
]


Block[{testSym, testOtherSym},
	testSym = testOtherSym;
	Test[
		Catch[
			addIncorrectArgsDefinition["testSym"];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm @ addIncorrectArgsDefinition["testSym"],
						HoldForm @
							CellsToTeXException["Error", "UnwantedEvaluation"],
						HoldForm @ testSym
					},
					"Type" -> {"Error", "UnwantedEvaluation"}
				]
			],
			CellsToTeXException["Error", "UnwantedEvaluation"]
		}
		,
		TestID -> "String with name of symbol evalauting to other symbol: \
addIncorrectArgsDefinition evaluation"
	];
	TestMatch[
		DownValues[testSym]
		,
		{}
		,
		TestID -> "String with name of symbol evalauting to other symbol: \
passed symbol DownValues"
	];
	TestMatch[
		DownValues[testOtherSym]
		,
		{}
		,
		TestID -> "String with name of symbol evalauting to other symbol: \
other symbol DownValues"
	]
]


Test[
	Catch[
		addIncorrectArgsDefinition["not a symbol name"];
		,
		_
		,
		HoldComplete
	]
	,
	HoldComplete @@ {
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::error,
				"MessageParameters" -> {
					HoldForm @ addIncorrectArgsDefinition["not a symbol name"],
					HoldForm @ CellsToTeXException["Error", "NotSymbolName"],
					HoldForm @ "not a symbol name"
				},
				"Type" -> {"Error", "NotSymbolName"}
			]
		],
		CellsToTeXException["Error", "NotSymbolName"]
	}
	,
	{Message[Symbol::symname, "not a symbol name"]}
	,
	TestID -> "String not a symbol name"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[addIncorrectArgsDefinition[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError @
			addIncorrectArgsDefinition[testArg1, testArg2]
		,
		TestID -> "Incorrect arguments"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
