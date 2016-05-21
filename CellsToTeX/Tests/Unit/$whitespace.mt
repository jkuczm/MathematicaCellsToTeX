(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`$whitespace`", {"MUnit`"}]


Get["CellsToTeX`"]


stringMatchWhitespaceQ = StringMatchQ[#, CellsToTeX`Internal`$whitespace]&


(* ::Section:: *)
(*Tests*)


Test[
	" " // stringMatchWhitespaceQ
	,
	True
	,
	TestID -> "space"
]
Test[
	"\t" // stringMatchWhitespaceQ
	,
	True
	,
	TestID -> "tab"
]
Test[
	"\n" // stringMatchWhitespaceQ
	,
	True
	,
	TestID -> "newline"
]
Test[
	"\[IndentingNewLine]" // stringMatchWhitespaceQ
	,
	True
	,
	TestID -> "IndentingNewLine"
]

Test[
	"\[IndentingNewLine] \t\n \t" // stringMatchWhitespaceQ
	,
	True
	,
	TestID -> "mixed whitespace"
]


Test[
	"" // stringMatchWhitespaceQ
	,
	False
	,
	TestID -> "empty string"
]
Test[
	"a" // stringMatchWhitespaceQ
	,
	False
	,
	TestID -> "letter"
]
Test[
	" \t1\n \[Alpha]" // stringMatchWhitespaceQ
	,
	False
	,
	TestID -> "mixed whitespace and non-whitespace"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
