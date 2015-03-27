(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`defaultAnnotationType`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


(* ::Section:: *)
(*Tests*)


Test[
	defaultAnnotationType[Block]
	,
	"DefinedSymbol"
	,
	TestID -> "built-in symbol"
]
Test[
	defaultAnnotationType["Block"]
	,
	"DefinedSymbol"
	,
	TestID -> "built-in symbol name"
]

Test[
	defaultAnnotationType[a]
	,
	"UndefinedSymbol"
	,
	TestID -> "custom symbol"
]
Test[
	defaultAnnotationType["a"]
	,
	"UndefinedSymbol"
	,
	TestID -> "custom symbol name"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
