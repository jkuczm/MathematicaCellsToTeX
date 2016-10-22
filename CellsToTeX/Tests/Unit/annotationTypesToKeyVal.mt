(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`annotationTypesToKeyVal`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]

$testAnnotationTypeToKey = {"Type1" -> "type1", "Type2" -> "type2"}


(* ::Section:: *)
(*Tests*)


Test[
	annotationTypesToKeyVal[{"a" -> "Type1"}, $testAnnotationTypeToKey]
	,
	{"type1" -> {"a"}}
	,
	TestID -> "1 type: 1 symbol"
]

Test[
	annotationTypesToKeyVal[
		{"a" -> "Type1", "b" -> "Type1"}, $testAnnotationTypeToKey
	]
	,
	{"type1" -> {"a", "b"}}
	,
	TestID -> "1 type: 2 symbols"
]

Test[
	annotationTypesToKeyVal[
		{"a" -> "Type1", "b" -> "Type2"}, $testAnnotationTypeToKey
	]
	,
	{"type1" -> {"a"}, "type2" -> {"b"}}
	,
	TestID -> "2 types: 1 symbol, 1 symbol"
]

Test[
	annotationTypesToKeyVal[
		{"a" -> "Type1", "b" -> "Type2", "c" -> "Type1"},
		$testAnnotationTypeToKey
	]
	,
	{"type1" -> {"a", "c"}, "type2" -> {"b"}}
	,
	TestID -> "2 types: 2 symbols, 1 symbol"
]

Test[
	annotationTypesToKeyVal[
		{"a" -> "Type1", "b" -> "Type2", "c" -> "Type2"},
		$testAnnotationTypeToKey
	]
	,
	{"type1" -> {"a"}, "type2" -> {"b", "c"}}
	,
	TestID -> "2 types: 1 symbol, 2 symbols"
]


Test[
	annotationTypesToKeyVal[
		{"a#%" -> "Type1", "V{}\\5" -> "Type1"},
		$testAnnotationTypeToKey
	]
	,
	{"type1" -> {"a\\#\\%", "V\\{\\}\\\\5"}}
	,
	TestID -> "TeX special characters"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[annotationTypesToKeyVal[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[annotationTypesToKeyVal[testArg]]
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
