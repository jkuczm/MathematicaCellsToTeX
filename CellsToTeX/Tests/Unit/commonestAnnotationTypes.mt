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
	{boxes_, allowedTypes_},
	expected_,
	Shortest[messages_:{}],
	opts:OptionsPattern[Test]
] := (
	Test[
		commonestAnnotationTypes[boxes, allowedTypes, False]
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
		commonestAnnotationTypes[boxes, allowedTypes, True]
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
		{syntaxBox["a", "testNonDefault"], _}
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: non-default"
	];
	TestCommonestAnnotationTypes[
		{syntaxBox["a", "testDefaultAnnotationTypeOfa"], _}
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: default"
	];
	TestCommonestAnnotationTypes[
		{syntaxBox["a", "testNonDefault", "testDefaultAnnotationTypeOfa"], _}
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: non-default + default"
	];
	TestCommonestAnnotationTypes[
		{syntaxBox["a", "testDefaultAnnotationTypeOfa", "testNonDefault"], _}
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 1 time: default + non-default"
	];
	
	
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testNonDefault"],
				syntaxBox["a", "testNonDefault"]
			}],
			_
		}
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 non-default"
	];
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testDefaultAnnotationTypeOfa"],
				syntaxBox["a", "testDefaultAnnotationTypeOfa"]
			}],
			_
		}
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 default"
	];
	
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testNonDefault"],
				syntaxBox["a", "testDefaultAnnotationTypeOfa"]
			}],
			_
		}
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 non-default, 1 default"
	];
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testDefaultAnnotationTypeOfa"],
				syntaxBox["a", "testNonDefault"]
			}],
			_
		}
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 2 times: 1 default, 1 non-default"
	];
	
	
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testNonDefault"],
				syntaxBox["a", "testNonDefault"],
				syntaxBox["a", "testDefaultAnnotationTypeOfa"]
			}],
			_
		}
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 3 times: 2 non-default, 1 default"
	];
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testDefaultAnnotationTypeOfa"],
				syntaxBox["a", "testDefaultAnnotationTypeOfa"],
				syntaxBox["a", "testNonDefault"]
			}],
			_
		}
		,
		{"a" -> "testDefaultAnnotationTypeOfa"}
		,
		TestID -> "ASCII: 1 symbol: 3 times: 2 default, 1 non-default"
	];
	
	
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testNonDefaulta"],
				syntaxBox["b", "testNonDefaultb"]
			}],
			_
		}
		,
		{"a" -> "testNonDefaulta", "b" -> "testNonDefaultb"}
		,
		TestID ->
			"ASCII: 2 symbols: 1 times: 1 non-default; 1 times: 1 non-default"
	];
	
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testNonDefaulta"],
				syntaxBox["b", "testNonDefaultb2"],
				syntaxBox["a", "testDefaultAnnotationTypeOfa"],
				RowBox[{
					syntaxBox["b", "testNonDefaultb1"],
					syntaxBox["a", "testDefaultAnnotationTypeOfa"],
					syntaxBox["a", "testNonDefaulta"],
					syntaxBox["b", "testNonDefaultb2"],
					syntaxBox["b", "testNonDefaultb1"],
					syntaxBox["b", "testNonDefaultb1"]
				}]
			}],
			_
		}
		,
		{"a" -> "testDefaultAnnotationTypeOfa", "b" -> "testNonDefaultb1"}
		,
		TestID -> "ASCII: 2 symbols: \
4 times: 2 non-default, 2 default; 5 times: 3 non-default1, 2 non-default2"
	];
	
	TestCommonestAnnotationTypes[
		{syntaxBox["a", "testNonAllowed"], "testAllowed"}
		,
		{}
		,
		TestID -> "ASCII: 1 symbol: 1 time: not allowed"
	];
	TestCommonestAnnotationTypes[
		{
			RowBox[{
				syntaxBox["a", "testExcluded"],
				syntaxBox["a", "testNonDefault"],
				syntaxBox["a", "testExcluded"]
			}],
			Except["testExcluded"]
		}
		,
		{"a" -> "testNonDefault"}
		,
		TestID -> "ASCII: 1 symbol: 3 times: 1 non-default, 2 not allowed"
	]
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
			syntaxBox["xy\[Alpha]z2", "testNonDefault"],
			_,
			False
		]
		,
		{}
		,
		TestID -> "Non-ASCII: 1 symbol: 1 time: non-default mixed: False"
	];
	Test[
		commonestAnnotationTypes[
			syntaxBox["xy\[Alpha]z2", "testNonDefault"],
			_,
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
				syntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				syntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				syntaxBox["\[Alpha]", "testNonDefault"]
			}]
			,
			_,
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
				syntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				syntaxBox["\[Alpha]", "testDefaultAnnotationTypeOfAlpha"],
				syntaxBox["\[Alpha]", "testNonDefault"]
			}]
			,
			_,
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
				syntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				syntaxBox["\[Beta]", "testNonDefaultBeta"]
			}]
			,
			_,
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
				syntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				syntaxBox["\[Beta]", "testNonDefaultBeta"]
			}]
			,
			_,
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
				syntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				syntaxBox["b", "testNonDefaultb"]
			}]
			,
			_,
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
				syntaxBox["\[Alpha]", "testNonDefaultAlpha"],
				syntaxBox["b", "testNonDefaultb"]
			}]
			,
			_,
			True
		]
		,
		{"\[Alpha]" -> "testNonDefaultAlpha", "b" -> "testNonDefaultb"}
		,
		TestID -> "Non-ASCII: 2 symbols: \
1 times: 1 non-default; 1 times: 1 non-default ASCII: True"
	];
	
	
	Test[
		commonestAnnotationTypes[
			syntaxBox["\[Alpha]", "testNonAllowed"],
			"testAllowed",
			False
		]
		,
		{}
		,
		TestID -> "Non-ASCII: 1 symbol: 1 time: not allowed: False"
	];
	Test[
		commonestAnnotationTypes[
			syntaxBox["\[Alpha]", "testNonAllowed"],
			"testAllowed",
			True
		]
		,
		{}
		,
		TestID -> "Non-ASCII: 1 symbol: 1 time: not allowed: True"
	];
	
	Test[
		commonestAnnotationTypes[
			RowBox[{
				syntaxBox["\[Alpha]", "testExcluded"],
				syntaxBox["\[Alpha]", "testNonDefault"],
				syntaxBox["\[Alpha]", "testExcluded"]
			}],
			Except["testExcluded"],
			False
		]
		,
		{}
		,
		TestID -> "Non-ASCII: 1 symbol: \
3 times: 1 non-default, 2 not allowed: False"
	];
	Test[
		commonestAnnotationTypes[
			RowBox[{
				syntaxBox["\[Alpha]", "testExcluded"],
				syntaxBox["\[Alpha]", "testNonDefault"],
				syntaxBox["\[Alpha]", "testExcluded"]
			}],
			Except["testExcluded"],
			True
		]
		,
		{"\[Alpha]" -> "testNonDefault"}
		,
		TestID -> "Non-ASCII: 1 symbol: \
3 times: 1 non-default, 2 not allowed: True"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2, testArg3},
	Test[
		Catch[
			commonestAnnotationTypes[testArg1, testArg2, testArg3];,
			_,
			HoldComplete
		]
		,
		expectedIncorrectArgsError @
			commonestAnnotationTypes[testArg1, testArg2, testArg3]
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
