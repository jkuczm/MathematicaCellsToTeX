(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`mergeAdjacentTeXCommands`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	mergeAdjacentTeXCommands["\\pmb", "{", "}", "\
a \\pmb{bcde}  \\pmb{fgh}	\\pmb{\\ijk}\\pmb{\\l{m}} \\pmb{n}\\pmb{op} qr\\pmb{st} \\pmb{uv}\[IndentingNewLine]
\\pmb{wxy}z"
	]
	,
	"\
a \\pmb{bcde  fgh	\\ijk\\l{m} n}\\pmb{op} qr\\pmb{st uv}\[IndentingNewLine]
\\pmb{wxy}z"
	,
	TestID -> "mixed"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2, testArg3, testArg4},
	Test[
		Catch[
			mergeAdjacentTeXCommands[testArg1, testArg2, testArg3, testArg4];,
			_,
			HoldComplete
		]
		,
		expectedIncorrectArgsError @
			mergeAdjacentTeXCommands[testArg1, testArg2, testArg3, testArg4]
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
