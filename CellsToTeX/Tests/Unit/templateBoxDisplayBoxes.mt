(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`templateBoxDisplayBoxes`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	TemplateBox[
		{"a", "b", "c"},
		"tag",
		DisplayFunction -> (RowBox[{"(", #1, "+", #2, "-", #3, ")"}]&)
	] // templateBoxDisplayBoxes
	,
	RowBox[{"(", "a", "+", "b", "-", "c", ")"}]
	,
	TestID -> "explicit DisplayFunction"
]


UsingFrontEnd @ Test[
	TemplateBox[{"a", "b"}, "Binomial"] // templateBoxDisplayBoxes
	,
	RowBox[{"(", GridBox[{{"a"}, {"b"}}], ")"}]
	,
	TestID -> "named style"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[templateBoxDisplayBoxes[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[templateBoxDisplayBoxes[testArg1, testArg2]]
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
