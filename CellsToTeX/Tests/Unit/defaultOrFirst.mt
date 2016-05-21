(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`defaultOrFirst`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Module[{a, b, c},
	Test[
		defaultOrFirst[{a, b, c}, a]
		,
		a
		,
		TestID -> "Default first"
	]
]
Module[{a, b, c, d},
	Test[
		defaultOrFirst[{a, b, c, d}, b]
		,
		b
		,
		TestID -> "Default inside"
	]
]
Module[{a, b, c, d, e},
	Test[
		defaultOrFirst[{a, b, c, d, e}, e]
		,
		e
		,
		TestID -> "Default last"
	]
]
Module[{a, b, c, d, e, f, g},
	Test[
		defaultOrFirst[{a, b, c, d, e, f}, g]
		,
		a
		,
		TestID -> "Default not in list"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[defaultOrFirst[{}, testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[defaultOrFirst[{}, testArg]]
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
