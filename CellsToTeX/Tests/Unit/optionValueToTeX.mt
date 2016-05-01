(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`optionValueToTeX`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	optionValueToTeX[""]
	,
	""
	,
	TestID -> "empty string"
]


Test[
	optionValueToTeX[True]
	,
	"true"
	,
	TestID -> "True"
]
Test[
	optionValueToTeX[False]
	,
	"false"
	,
	TestID -> "False"
]


Test[
	optionValueToTeX[2]
	,
	"2"
	,
	TestID -> "Integer"
]


Test[
	optionValueToTeX["abc 3+1"]
	,
	"abc 3+1"
	,
	TestID -> "safe characters"
]

Test[
	optionValueToTeX["{abc 3+1}"]
	,
	"{abc 3+1}"
	,
	TestID -> "safe characters: braced"
]


Test[
	optionValueToTeX["["]
	,
	"{[}"
	,
	TestID -> "left square bracket"
]

Test[
	optionValueToTeX["{[}"]
	,
	"{[}"
	,
	TestID -> "left square bracket: braced"
]


Test[
	optionValueToTeX["]"]
	,
	"{]}"
	,
	TestID -> "right square bracket"
]

Test[
	optionValueToTeX["{]}"]
	,
	"{]}"
	,
	TestID -> "right square bracket: braced"
]


Test[
	optionValueToTeX[","]
	,
	"{,}"
	,
	TestID -> "comma"
]

Test[
	optionValueToTeX["{,}"]
	,
	"{,}"
	,
	TestID -> "comma: braced"
]


Test[
	optionValueToTeX["="]
	,
	"{=}"
	,
	TestID -> "equal sign"
]

Test[
	optionValueToTeX["{=}"]
	,
	"{=}"
	,
	TestID -> "equal sign: braced"
]


Test[
	optionValueToTeX["Out[\\mmaCellIndex]\\mmaCellForm*="]
	,
	"{Out[\\mmaCellIndex]\\mmaCellForm*=}"
	,
	TestID -> "complex expression"
]

Test[
	optionValueToTeX["{Out[\\mmaCellIndex]\\mmaCellForm*=}"]
	,
	"{Out[\\mmaCellIndex]\\mmaCellForm*=}"
	,
	TestID -> "complex expression: braced"
]


Test[
	optionValueToTeX[{
		"opt1" :> "[x]",
		"opt2" -> {"opt2a" -> "val", "opt2b" -> True},
		"opt3" -> 5
	}]
	,
	"{opt1={[x]},opt2={opt2a=val,opt2b=true},opt3=5}"
	,
	TestID -> "nested list of options"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[optionValueToTeX[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[optionValueToTeX[testArg1, testArg2]]
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
