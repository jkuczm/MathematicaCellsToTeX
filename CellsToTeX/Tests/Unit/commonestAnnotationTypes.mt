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


SetAttributes[TestCommonestAnnotationTypes, HoldAllComplete]

TestCommonestAnnotationTypes[
	boxes_,
	expected_,
	Shortest[messages_:{}],
	opts:OptionsPattern[Test]
] := (
	Test[
		commonestAnnotationTypes[boxes, False]
		,
		expected
		,
		messages
		,
		opts
		,
		TestFailureMessage -> "False"
	];
	Test[
		commonestAnnotationTypes[boxes, True]
		,
		expected
		,
		messages
		,
		opts
		,
		TestFailureMessage -> "True"
	]
)

TestCommonestAnnotationTypes[args___] :=
	With[{msg = "Incorrect arguments: " <> ToString[Unevaluated[{args}]]},
		MUnit`Package`testError[msg, {}, args]
	]


(* ::Section:: *)
(*Tests*)


(* ::Subection:: *)
(*ASCII*)


Block[{defaultAnnotationType},
	defaultAnnotationType["a"] = "testDefaultAnnotationTypeOfa";
	defaultAnnotationType["b"] = "testDefaultAnnotationTypeOfb";
	
	TestCommonestAnnotationTypes[
		SyntaxBox["a", "testNonDefault"]
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: non-default"
	];
	TestCommonestAnnotationTypes[
		SyntaxBox["a", "testDefaultAnnotationTypeOfa"]
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: default"
	];
	TestCommonestAnnotationTypes[
		SyntaxBox["a", "testNonDefault", "testDefaultAnnotationTypeOfa"]
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: non-default + default"
	];
	TestCommonestAnnotationTypes[
		SyntaxBox["a", "testDefaultAnnotationTypeOfa", "testNonDefault"]
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: default + non-default"
	];
	
	
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testNonDefault"]
		}]
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 non-default"
	];
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"]
		}]
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 default"
	];
	
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"]
		}]
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 non-default, 1 default"
	];
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testNonDefault"]
		}]
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 default, 1 non-default"
	];
	
	
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testNonDefault"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"]
		}]
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 3 times: 2 non-default, 1 default"
	];
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			SyntaxBox["a", "testNonDefault"]
		}]
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 3 times: 2 default, 1 non-default"
	];
	
	
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testNonDefaulta"],
			SyntaxBox["b", "testNonDefaultb"]
		}]
		,
		{"a" -> "testNonDefaulta", "b" -> "testNonDefaultb"}
		,
		TestID ->
			"ASCII: 2 symbols: 1 times: 1 non-default; 1 times: 1 non-default"
	];
	
	TestCommonestAnnotationTypes[
		RowBox[{
			SyntaxBox["a", "testNonDefaulta"],
			SyntaxBox["b", "testNonDefaultb2"],
			SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
			RowBox[{
				SyntaxBox["b", "testNonDefaultb1"],
				SyntaxBox["a", "testDefaultAnnotationTypeOfa"],
				SyntaxBox["a", "testNonDefaulta"],
				SyntaxBox["b", "testNonDefaultb2"],
				SyntaxBox["b", "testNonDefaultb1"],
				SyntaxBox["b", "testNonDefaultb1"]
			}]
		}]
		,
		{"a" -> "testDefaultAnnotationTypeOfa", "b" -> "testNonDefaultb1"}
		,
		TestID -> "ASCII: 2 symbols: \
4 times: 2 non-default, 2 default; 5 times: 3 non-default1, 2 non-default2"
	];
]


(* ::Subection:: *)
(*Non-ASCII*)


Block[{defaultAnnotationType},
	defaultAnnotationType["\[Alpha]"] = "testDefaultAnnotationTypeOfAlpha";
	defaultAnnotationType["xy\[Alpha]z2"] =
		"testDefaultAnnotationTypeOfxyAlphaz2";
	defaultAnnotationType["\[Beta]"] = "testDefaultAnnotationTypeOfBeta";
	defaultAnnotationType["b"] = "testDefaultAnnotationTypeOfb";
	
	
	Test[
		commonestAnnotationTypes[
			SyntaxBox["xy\[Alpha]z2", "testNonDefault"],
			False
		]
		,
		{}
		,
		TestID -> "Non-ASCII: 1 symbol: 1 time: non-default mixed: False"
	];
	Test[
		commonestAnnotationTypes[
			SyntaxBox["xy\[Alpha]z2", "testNonDefault"],
			True
		]
		,
		{"xy\[Alpha]z2" -> "testNonDefault"}
		,
		TestID -> "Non-ASCII: 1 symbol: 1 time: non-default mixed: True"
	];
	
	
	Test[
		commonestAnnotationTypes[
			RowBox[{
				SyntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				SyntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				SyntaxBox["\[Alpha]", "testNonDefault"]
			}]
			,
			False
		]
		,
		{}
		,
		TestID ->
			"Non-ASCII: 1 symbol: 3 times: 2 default, 1 non-default: False"
	];
	Test[
		commonestAnnotationTypes[
			RowBox[{
				SyntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				SyntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				SyntaxBox["\[Alpha]", "testNonDefault"]
			}]
			,
			True
		]
		,
		{"\[Alpha]" -> "testDefaultAnnotationTypeOfAlpha"}
		,
		TestID ->
			"Non-ASCII: 1 symbol: 3 times: 2 default, 1 non-default: True"
	];
	
	
	Test[
		commonestAnnotationTypes[
			RowBox[{
				SyntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				SyntaxBox["\[Beta]", "testNonDefaultBeta"]
			}]
			,
			False
		]
		,
		{}
		,
		TestID -> "Non-ASCII: 2 symbols: \
1 times: 1 non-default; 1 times: 1 non-default: False"
	];
	Test[
		commonestAnnotationTypes[
			RowBox[{
				SyntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				SyntaxBox["\[Beta]", "testNonDefaultBeta"]
			}]
			,
			True
		]
		,
		{
			"\[Alpha]" -> "testNonDefaultAlpha",
			"\[Beta]" -> "testNonDefaultBeta"
		}
		,
		TestID -> "Non-ASCII: 2 symbols: \
1 times: 1 non-default; 1 times: 1 non-default: True"
	];
	
	
	Test[
		commonestAnnotationTypes[
			RowBox[{
				SyntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				SyntaxBox["b", "testNonDefaultb"]
			}]
			,
			False
		]
		,
		{"b" -> "testNonDefaultb"}
		,
		TestID -> "Non-ASCII: 2 symbols: \
1 times: 1 non-default; 1 times: 1 non-default ASCII: False"
	];
	Test[
		commonestAnnotationTypes[
			RowBox[{
				SyntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				SyntaxBox["b", "testNonDefaultb"]
			}]
			,
			True
		]
		,
		{"\[Alpha]" -> "testNonDefaultAlpha", "b" -> "testNonDefaultb"}
		,
		TestID -> "Non-ASCII: 2 symbols: \
1 times: 1 non-default; 1 times: 1 non-default ASCII: True"
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
