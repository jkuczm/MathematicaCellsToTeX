(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`commonestAnnotationTypes`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[
		{
			"SyntaxAnnotations`",
			"CellsToTeX`Configuration`",
			"CellsToTeX`Internal`"
		},
		$ContextPath
	]


(* ::Section:: *)
(*Tests*)


Block[{defaultAnnotationType},
	defaultAnnotationType["a"] = "testDefaultAnnotationTypeOfa";
	defaultAnnotationType["b"] = "testDefaultAnnotationTypeOfb";
	
	Test[
		"a" // commonestAnnotationTypes
		,
		{"a" -> "DefinedSymbol"}
		,
		TestID -> "1 symbol: 1 time: no"
	];
	Test[
		SyntaxBox["a", "testNonDefault"] // commonestAnnotationTypes
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "1 symbol: 1 time: non-default"
	];
	Test[
		SyntaxBox["a", "testDefaultAnnotationTypeOfa"] //
			commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 1 time: default"
	];
	Test[
		SyntaxBox["a", "testNonDefault", "testDefaultAnnotationTypeOfa"] //
			commonestAnnotationTypes
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "1 symbol: 1 time: non-default + default"
	];
	Test[
		SyntaxBox["a", "testDefaultAnnotationTypeOfa", "testNonDefault"] //
			commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 1 time: default + non-default"
	];
	
	
	Test[
		RowBox[{"a", "a"}] // commonestAnnotationTypes
		,
		{"a" -> "DefinedSymbol"}
		,
		TestID -> "1 symbol: 2 times: 1 no"
	];
	Test[
		RowBox[{
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testNonDefault"]
		}] // commonestAnnotationTypes
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "1 symbol: 2 times: 1 non-default"
	];
	Test[
		RowBox[{
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"]
		}] // commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 2 times: 1 default"
	];
	
	Test[
		RowBox[{"a", SyntaxBox["a", "testNonDefault"]}] //
			commonestAnnotationTypes
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "1 symbol: 2 times: 1 no, 1 non-default"
	];
	Test[
		RowBox[{SyntaxBox["a", "testNonDefault"], "a"}] //
			commonestAnnotationTypes
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "1 symbol: 2 times: 1 non-default, 1 no"
	];
	
	Test[
		RowBox[{"a", SyntaxBox["a", "testDefaultAnnotationTypeOfa"]}] //
			commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 2 times: 1 no, 1 default"
	];
	Test[
		RowBox[{SyntaxBox["a", "testDefaultAnnotationTypeOfa"], "a"}] //
			commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 2 times: 1 default, 1 no"
	];
	
	Test[
		RowBox[{
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"]
		}] // commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 2 times: 1 non-default, 1 default"
	];
	Test[
		RowBox[{
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testNonDefault"]
		}] // commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 2 times: 1 default, 1 non-default"
	];
	
	
	Test[
		RowBox[{"a", "a", SyntaxBox["a", "testNonDefault"]}] //
			commonestAnnotationTypes
		,
		{"a" -> "DefinedSymbol"}
		,
		TestID -> "1 symbol: 3 times: 2 no, 1 non-default"
	];
	Test[
		RowBox[{"a", "a", SyntaxBox["a", "testDefaultAnnotationTypeOfa"]}] //
			commonestAnnotationTypes
		,
		{"a" -> "DefinedSymbol"}
		,
		TestID -> "1 symbol: 3 times: 2 no, 1 default"
	];
	
	Test[
		RowBox[{
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testNonDefault"],
			"a"
		}] // commonestAnnotationTypes
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "1 symbol: 3 times: 2 non-default, 1 no"
	];
	Test[
		RowBox[{
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"]
		}] // commonestAnnotationTypes
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "1 symbol: 3 times: 2 non-default, 1 default"
	];
	
	Test[
		RowBox[{
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			"a"
		}] // commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 3 times: 2 default, 1 no"
	];
	Test[
		RowBox[{
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testNonDefault"]
		}] // commonestAnnotationTypes
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "1 symbol: 3 times: 2 default, 1 non-default"
	];
	
	
	Test[
		RowBox[{
			SyntaxBox["a", "testNonDefaulta"],
			SyntaxBox["b", "testNonDefaultb"]
		}] // commonestAnnotationTypes
		,
		{"a" -> "testNonDefaulta", "b" -> "testNonDefaultb"}
		,
		TestID -> "2 symbols: 1 times: 1 non-default; 1 times: 1 non-default"
	];
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[commonestAnnotationTypes[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError @
			commonestAnnotationTypes[testArg1, testArg2]
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
