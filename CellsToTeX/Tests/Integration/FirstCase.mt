(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`FirstCase`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Backports`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*1 arg*)


Module[{a},
	Test[
		FirstCase[a] // Hold[#]&
		,
		FirstCase[a] // Hold
		,
		TestID -> "1 arg: no sub args"
	]
]


Module[{f, g, x, y, z},
	Test[
		FirstCase[_f][g[x, f[y, z], f[x], y, f[]]]
		,
		f[y, z]
		,
		TestID -> "1 arg: 1 sub arg: 3 exist: patt"
	]
]
Module[{f, h, x, y, z},
	Test[
		FirstCase[f[a_] :> h[a]][{z, x, f[x, y], y, f[z], z, f[x]}]
		,
		h[z]
		,
		TestID -> "1 arg: 1 sub arg: 2 exist: rule"
	]
]
Module[{x},
	Test[
		FirstCase["str1"][{x, "str2", 5}]
		,
		Missing["NotFound"]
		,
		TestID -> "1 arg: 1 sub arg: missing"
	]
]


(* ::Subsection:: *)
(*2 args*)


Module[{f, g, x, y, z},
	Test[
		FirstCase[{_f, f[x], g[y], f[z]}, _f]
		,
		f[x]
		,
		TestID -> "2 args: 2 exist: patt"
	]
]
Module[{f, g, h, x, y, z},
	Test[
		FirstCase[h[f[x], g[y], f[], f[z]], f[a_] :> a]
		,
		x
		,
		TestID -> "2 args: 3 exist: rule"
	]
]
Module[{f, g, x, y, z},
	Test[
		FirstCase[{f[x], g[y], f[z]}, f[_, _]]
		,
		Missing["NotFound"]
		,
		TestID -> "2 args: missing"
	]
]


(* ::Subsection:: *)
(*3 args*)


Module[{f, leaked = False},
	Test[
		FirstCase[f[1, 2, 1, 3, 2, 2], 2, leaked = True]
		,
		2
		,
		TestID -> "3 args: 3 exist: patt"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "3 args: 3 exist: patt: evaluation leak"
	]
]
Module[{a, b, c, d, default},
	Test[
		FirstCase[{a, c, a, d}, b, default]
		,
		default
		,
		TestID -> "3 args: missing"
	]
]


(* ::Subsection:: *)
(*4 args*)


Module[{f, g, h, default},
	Test[
		FirstCase[f[g[12, 3, {-1}, -4], {0}, {7, 1}, 1],
			x_ /; x < 2 :> h[x], default, {2}
		]
		,
		h[-4]
		,
		TestID -> "4 args: 3 exist on specified level: rule"
	]
]
Module[{x, y, z, default},
	Test[
		FirstCase[{{{x, {y}, z}, y}}, y, default, {3}]
		,
		default
		,
		TestID -> "4 args: missing on specified level"
	]
]


(* ::Subsection:: *)
(*Heads option*)


Module[{f, x, y},
	Test[
		FirstCase[f[x][y], f[a_] :> a, Heads -> True]
		,
		x
		,
		TestID -> "Heads option: True: 1 exist in head: rule"
	]
]
Module[{f, h, x, y, z, default},
	Test[
		FirstCase[h[f[x][y], f[z][][]], _f, default, Infinity, Heads -> False]
		,
		default
		,
		TestID -> "Heads option: False: 2 exist in heads: patt"
	]
]


Test[
	OptionValue[FirstCase, Heads]
	,
	False
	,
	TestID -> "Heads option: default value"
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[FirstCase], Protected]
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
