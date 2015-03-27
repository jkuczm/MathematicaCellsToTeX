(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`Association`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Backports`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Bracket notation*)


Module[{a, b},
	Test[
		Association[a -> b][a]
		,
		b
		,
		TestID -> "existing"
	]
]


Module[{a, b, c},
	Test[
		Association[a -> b][c]
		,
		Missing["KeyAbsent", c]
		,
		TestID -> "missing"
	]
]


(* ::Subsection:: *)
(*Extract*)


Module[{a, b},
	Test[
		Extract[Association[a -> b], Key[a]]
		,
		b
		,
		TestID -> "Extract: no head: existing"
	]
]
Module[{b},
	Test[
		Extract[Association["a" -> b], "a"]
		,
		b
		,
		TestID -> "Extract: no head: existing: String key"
	]
]


Module[{a, b, c},
	Test[
		Extract[Association[a -> b], Key[c]]
		,
		Missing["KeyAbsent", Key[c]]
		,
		TestID -> "Extract: no head: missing"
	]
]
Module[{a, b},
	Test[
		Extract[Association[a -> b], "c"]
		,
		Missing["KeyAbsent", "c"]
		,
		TestID -> "Extract: no head: missing: String key"
	]
]


Module[{a, b},
	Test[
		Extract[Association[a -> b], Key[a], h]
		,
		h[b]
		,
		TestID -> "Extract: with head: existing"
	]
]
Module[{b},
	Test[
		Extract[Association["a" -> b], "a", h]
		,
		h[b]
		,
		TestID -> "Extract: with head: existing: String key"
	]
]
Module[{a, testVar},
	Test[
		Extract[
			Association[a :> (testVar = "Evaluation leaked")], Key[a], Hold
		]
		,
		Hold[testVar = "Evaluation leaked"]
		,
		TestID -> "Extract: with head: existing: evaluation leak"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "Extract: with head: existing: evaluation leak: testVar"
	]
]


Module[{a, b, c},
	Test[
		Extract[Association[a -> b], Key[c], h]
		,
		h[Missing["KeyAbsent", Key[c]]]
		,
		TestID -> "Extract: with head: missing"
	]
]
Module[{a, b},
	Test[
		Extract[Association[a -> b], "c", h]
		,
		h[Missing["KeyAbsent", "c"]]
		,
		TestID -> "Extract: with head: missing: String key"
	]
]


(* ::Subsection:: *)
(*Append*)


Module[{a, b, c, d},
	Test[
		Append[Association[a -> b], c :> d]
		,
		Association[a -> b, c :> d]
		,
		TestID -> "Append: one rule: non-existing"
	]
]
Module[{a, b, c},
	Test[
		Append[Association[a -> b], a -> c]
		,
		Association[a -> c]
		,
		TestID -> "Append: one rule: existing"
	]
]


Module[{a, b, c, d, e, f},
	Test[
		Append[Association[a -> b], {c :> d, e -> f}]
		,
		Association[a -> b, c :> d, e -> f]
		,
		TestID -> "Append: list of rules: non-existing"
	]
]
Module[{a, b, c, d, e},
	Test[
		Append[Association[a -> b], {c -> d, a :> e}]
		,
		Association[c -> d, a :> e]
		,
		TestID -> "Append: list of rules: existing"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[Association], Protected]
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
