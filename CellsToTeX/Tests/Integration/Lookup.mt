(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`Lookup`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Backports`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No default*)


Module[{a, b},
	Test[
		Lookup[Association[a -> b], a]
		,
		b
		,
		TestID -> "No default: key exist"
	]
]


Module[{a, b},
	Test[
		Lookup[Association[_a -> b], _a]
		,
		b
		,
		TestID -> "No default: key exist, but not if matched as pattern"
	]
]


Module[{a, b, c},
	Test[
		Lookup[Association[a -> b], c]
		,
		Missing["KeyAbsent", c]
		,
		TestID -> "No default: key missing"
	]
]


Module[{a, b},
	Test[
		Lookup[Association[a -> b], _]
		,
		Missing["KeyAbsent", _]
		,
		TestID ->
			"No default: key missing, but existing match lookedup as pattern"
	]
]


Module[{a, b},
	Test[
		Lookup[Association[_ -> a], b]
		,
		Missing["KeyAbsent", b]
		,
		TestID ->
			"No default: key missing, but lookedup match existing as pattern"
	]
]


Module[{a, b},
	assoc = Association[a -> b];
	Test[
		Lookup[assoc, b]
		,
		Missing["KeyAbsent", b]
		,
		TestID -> "No default: first arg non-assoc evaluates to assoc"
	]
]


(* ::Subsection:: *)
(*With default*)


Module[{a, b, d},
	Test[
		Lookup[Association[a -> b], a, d]
		,
		b
		,
		TestID -> "With default: key exist"
	]
]


Module[{a, b},
	Test[
		Lookup[Association[a -> Missing["KeyAbsent", a]], a, b]
		,
		Missing["KeyAbsent", a]
		,
		TestID -> "With default: key exist exactly equal to Missing default"
	]
]


Module[{a, b, c, d},
	Test[
		Lookup[Association[a -> b], c, d]
		,
		d
		,
		TestID -> "With default: key missing"
	]
]


Module[{a, b},
	assoc = Association[a -> b];
	Test[
		Lookup[assoc, b, d]
		,
		d
		,
		TestID -> "With default: first arg non-assoc evaluates to assoc"
	]
]


Module[{a, b, leaked = False},
	Test[
		Lookup[Association[a -> b], a, leaked = True]
		,
		b
		,
		TestID -> "With default: default evalaution leak: key exist"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "With default: default evalaution leak: key exist: leaked"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[Lookup], Protected]
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
