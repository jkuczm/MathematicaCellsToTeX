(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`removeMathMode`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	removeMathMode["some text"]
	,
	"some text"
	,
	TestID -> "no math mode"
]

Test[
	removeMathMode["\\(math"]
	,
	"\\(math"
	,
	TestID -> "math mode only opened"
]

Test[
	removeMathMode["math\\)"]
	,
	"math\\)"
	,
	TestID -> "math mode only closed"
]

Test[
	removeMathMode["\\(math\\)"]
	,
	"math"
	,
	TestID -> "math mode opened and closed"
]

Test[
	removeMathMode["\\(math1\\)text\\(math2\\)"]
	,
	"math1\\)text\\(math2"
	,
	TestID -> "two math modes opened and closed"
]

Test[
	removeMathMode[" \\( math\\) \t "]
	,
	"  math \t "
	,
	TestID -> "math mode opened and closed with whitespace"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[removeMathMode[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[removeMathMode[testArg1, testArg2]]
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
