(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`mergeAdjacentTeXDelims`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	mergeAdjacentTeXDelims["\\(", "\\)", "\
\\(\\a\\)\\(bcd\\) \t  \\(e\\)\\(\\fg\\)
\\(hijk\\)"
	]
	,
	"\
\\(\\a\\)\\(bcd \t  e\\fg\\)
\\(hijk\\)"
	,
	TestID -> "mixed"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2, testArg3},
	Test[
		Catch[
			mergeAdjacentTeXDelims[testArg1, testArg2, testArg3];,
			_,
			HoldComplete
		]
		,
		expectedIncorrectArgsError @
			mergeAdjacentTeXDelims[testArg1, testArg2, testArg3]
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
