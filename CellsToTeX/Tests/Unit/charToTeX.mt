(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`charToTeX`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


(* ::Section:: *)
(*Tests*)


Test[
	charToTeX["\[PlusMinus]"]
	,
	"\\(\\pm\\)"
	,
	TestID -> "\[PlusMinus]"
]


Test[
	charToTeX["\[Equal]"]
	,
	"=="
	,
	TestID -> "\[Equal]"
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
