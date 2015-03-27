(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`AssociationQ`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Backports`"]


(* ::Section:: *)
(*Tests*)


Module[{a, b, c, d},
	Test[
		AssociationQ[Association[a -> b, c :> d]]
		,
		True
		,
		TestID -> "Valid Association"
	]
]


Module[{a, b, c, d},
	Test[
		AssociationQ[Association[a -> b, c, d]]
		,
		False
		,
		TestID -> "Invalid Association"
	]
]


Module[{a, b, c, d},
	Test[
		AssociationQ[{a -> b, c :> d}]
		,
		False
		,
		TestID -> "non-Association"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[AssociationQ], Protected]
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
