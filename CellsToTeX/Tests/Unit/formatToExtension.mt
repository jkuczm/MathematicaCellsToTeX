(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`formatToExtension`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	formatToExtension["TEX"]
	,
	".tex"
	,
	TestID -> "TEX"
]

Test[
	formatToExtension["PDF"]
	,
	".pdf"
	,
	TestID -> "PDF"
]

Test[
	formatToExtension["EPS"]
	,
	".eps"
	,
	TestID -> "EPS"
]

Test[
	formatToExtension["PNG"]
	,
	".png"
	,
	TestID -> "PNG"
]

Test[
	formatToExtension["JPEG"]
	,
	".jpg"
	,
	TestID -> "JPEG"
]

Test[
	formatToExtension["unknown export format"]
	,
	""
	,
	TestID -> "unknown export format"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[formatToExtension[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[formatToExtension[testArg1, testArg2]]
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
