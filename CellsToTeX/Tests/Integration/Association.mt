(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`Association`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Backports`"]


pre11Key = If[$VersionNumber >= 11, Identity, Key]

post11QuietExtractkeyw =
	If[$VersionNumber >= 11,
		Function[Null, Quiet[#, Extract::keyw], HoldFirst]
	(* else *),
		Identity
	]


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
Module[{a, b},
	Test[
		Association[_a -> b][_a]
		,
		b
		,
		TestID -> "existing, but not if matched as pattern"
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
Module[{a, b},
	Test[
		Association[a -> b][_]
		,
		Missing["KeyAbsent", _]
		,
		TestID -> "missing, but existing if lookedup as pattern"
	]
]
Module[{a, b},
	Test[
		Association[_ -> b][a]
		,
		Missing["KeyAbsent", a]
		,
		TestID -> "missing, but lookedup match existing as pattern"
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
Module[{a, b},
	Test[
		Extract[Association[_a -> b], Key[_a]]
		,
		b
		,
		TestID -> "Extract: no head: existing, but not if matched as pattern"
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
		Extract[Association[a -> b], Key[c]] // post11QuietExtractkeyw
		,
		Missing["KeyAbsent", pre11Key[c]]
		,
		TestID -> "Extract: no head: missing"
	]
]
Module[{a, b},
	Test[
		Extract[Association[a -> b], Key[_]] // post11QuietExtractkeyw
		,
		Missing["KeyAbsent", pre11Key[_]]
		,
		TestID -> "Extract: no head: missing, \
but existing if lookedup as pattern"
	]
]
Module[{a, b},
	Test[
		Extract[Association[_ -> b], Key[a]] // post11QuietExtractkeyw
		,
		Missing["KeyAbsent", pre11Key[a]]
		,
		TestID -> "Extract: no head: missing, \
but lookedup match existing as pattern"
	]
]
Module[{a, b},
	Test[
		Extract[Association[a -> b], "c"] // post11QuietExtractkeyw
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
Module[{a, leaked = False},
	Test[
		Extract[Association[a :> (leaked = True)], Key[a], Hold]
		,
		Hold[leaked = True]
		,
		TestID -> "Extract: with head: existing: evaluation leak"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Extract: with head: existing: evaluation leak: leaked"
	]
]


Module[{a, b, c},
	Test[
		Extract[Association[a -> b], Key[c], h] // post11QuietExtractkeyw
		,
		h[Missing["KeyAbsent", pre11Key[c]]]
		,
		TestID -> "Extract: with head: missing"
	]
]
Module[{a, b},
	Test[
		Extract[Association[a -> b], "c", h] // post11QuietExtractkeyw
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
