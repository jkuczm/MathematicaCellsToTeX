(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`prettifyPatterns`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	a:b // prettifyPatterns
	,
	b
	,
	TestID -> "name used in one pattern"
]
Test[
	HoldComplete[a:b] // prettifyPatterns
	,
	HoldComplete[b]
	,
	TestID -> "name used in one pattern: held"
]

Test[
	{a:b, a:c} // prettifyPatterns
	,
	{Pattern["a", b], Pattern["a", c]}
	,
	TestID -> "name used in two patterns"
]
Test[
	HoldComplete[a:b, a:c] // prettifyPatterns
	,
	HoldComplete[Pattern["a", b], Pattern["a", c]]
	,
	TestID -> "name used in two patterns: held"
]

Test[
	{tmpContext`a:b, tmpContext`a:c} // prettifyPatterns
	,
	{Pattern["a", b], Pattern["a", c]}
	,
	TestID -> "name with explicit context used in two patterns"
]
Test[
	HoldComplete[tmpContext`a:b, tmpContext`a:c] // prettifyPatterns
	,
	HoldComplete[Pattern["a", b], Pattern["a", c]]
	,
	TestID -> "name with explicit context used in two patterns: held"
]

Test[
	{a:b /; testQ[a, c]} // prettifyPatterns
	,
	{Pattern["a", b] /; testQ["a", c]}
	,
	TestID -> "name used in pattern and condition"
]
Test[
	HoldComplete[a:b /; testQ[a, c]] // prettifyPatterns
	,
	HoldComplete[Pattern["a", b] /; testQ["a", c]]
	,
	TestID -> "name used in pattern and condition: held"
]

Test[
	f[(tmpContext`a:b) + c__ tmpContext`a_ - d_ /; testQ[c, e]] //
		prettifyPatterns
	,
	f[Pattern["a", b] + Pattern["c", __] Pattern["a", _] - _ /; testQ["c", e]]
	,
	TestID -> "complex expression"
]
Test[
	HoldComplete[(tmpContext`a:b) + c__ tmpContext`a_ - d_ /; testQ[c, e]] //
		prettifyPatterns
	,
	HoldComplete[
		Pattern["a", b] + Pattern["c", __] Pattern["a", _] - _ /; testQ["c", e]
	]
	,
	TestID -> "complex expression: held"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[prettifyPatterns[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[prettifyPatterns[testArg1, testArg2]]
		,
		TestID -> "Incorrect arguments"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["tmpContext`*"]
Quiet[Remove["tmpContext`*"], {Remove::rmnsm}]

Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
