(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`charToTeX`", {"MUnit`"}]


Get["CellsToTeX`"]

$ContextPath =
	Join[{"CellsToTeX`Configuration`", "CellsToTeX`Backports`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


Block[{$commandCharsToTeX = {"@" -> "test1", "#" -> "test2", "$" -> "test3"}},
	Test[
		charToTeX["\[PlusMinus]", FontWeight -> Plain]
		,
		"@(@pm@)"
		,
		TestID -> "\[PlusMinus]: Plain"
	]
]
Block[{$commandCharsToTeX = {"?" -> "test1", "[" -> "test2", "]" -> "test3"}},
	Test[
		charToTeX["\[PlusMinus]", FontWeight -> Bold]
		,
		"?(?pmb[?pm]?)"
		,
		TestID -> "\[PlusMinus]: Bold"
	]
]


Test[
	charToTeX["\[Equal]", FontWeight -> Plain]
	,
	"=="
	,
	TestID -> "\[Equal]: Plain"
]
Test[
	charToTeX["\[Equal]", FontWeight -> Bold]
	,
	"=="
	,
	TestID -> "\[Equal]: Bold"
]


Test[
	charToTeX["a", FontWeight -> Plain]
	,
	"a"
	,
	TestID -> "letter: Plain"
]
Test[
	charToTeX["a", FontWeight -> Bold]
	,
	"a"
	,
	TestID -> "letter: Bold"
]


(* ::Subsection:: *)
(*Exceptions*)


Module[{unsupportedFontWeight},
	Test[
		Catch[
			charToTeX["a", FontWeight -> unsupportedFontWeight];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::unsupported,
					"MessageParameters" -> {
						HoldForm @ charToTeX[
							"a", FontWeight -> unsupportedFontWeight
						],
						HoldForm @ CellsToTeXException[
							"Unsupported", "OptionValue", FontWeight
						],
						HoldForm @ "OptionValue",
						HoldForm @ unsupportedFontWeight,
						HoldForm @ {Plain, Bold}
					},
					"Type" -> {"Unsupported", "OptionValue", FontWeight}
				]
			],
			CellsToTeXException["Unsupported", "OptionValue", FontWeight]
		}
		,
		TestID -> "Exception: Unsupported OptionValue FontWeight"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[charToTeX], Protected]
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
