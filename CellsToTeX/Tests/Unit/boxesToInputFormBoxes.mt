(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`boxesToInputFormBoxes`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Internal`", "CellsToTeX`Backports`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


UsingFrontEnd @ Test[
	SuperscriptBox["a", "b"] // boxesToInputFormBoxes
	,
	RowBox[{"a", "^", "b"}]
	,
	TestID -> "SuperscriptBox"
]


UsingFrontEnd @ Test[
	RowBox[{"a", "+", "b", "  "}] // boxesToInputFormBoxes
	,
	RowBox[{"a", " ", "+", " ", "b"}]
	,
	TestID -> "RowBox: Plus with non-semantic whitespace"
]

UsingFrontEnd @ Test[
	RowBox[{
		"f", "[", "\[IndentingNewLine]",
		RowBox[{
			SuperscriptBox["x", "2"], ",", "\[IndentingNewLine]",
			"y"
		}], "\[IndentingNewLine]",
		"]"
	}] // boxesToInputFormBoxes
	,
	RowBox[{"f", "[", RowBox[{RowBox[{"x", "^", "2"}], ",", " ", "y"}], "]"}]
	,
	TestID -> "RowBox, nested, SuperscriptBox: \
function, two args, newlines between args and next to brackets"
]

Test[
	" " // boxesToInputFormBoxes
	,
	" "
	,
	TestID -> "String: whitespace: space"
]
Test[
	"\t" // boxesToInputFormBoxes
	,
	"\t"
	,
	TestID -> "String: whitespace: \\t"
]
Test[
	"\n" // boxesToInputFormBoxes
	,
	"\n"
	,
	TestID -> "String: whitespace: \\n"
]
Test[
	"\[IndentingNewLine]" // boxesToInputFormBoxes
	,
	"\[IndentingNewLine]"
	,
	TestID -> "String: whitespace: \\[IndentingNewLine]"
]
Test[
	"\[IndentingNewLine] \t\n" // boxesToInputFormBoxes
	,
	"\[IndentingNewLine] \t\n"
	,
	TestID -> "String: whitespace: combination"
]


UsingFrontEnd @ Test[
	"mySymbol73" // boxesToInputFormBoxes
	,
	"mySymbol73"
	,
	TestID -> "String: symbol name"
]
UsingFrontEnd @ Test[
	"a1 + tmp" // boxesToInputFormBoxes
	,
	RowBox[{"a1", " ", "+", " ", "tmp"}]
	,
	TestID -> "String: expression"
]


UsingFrontEnd @ Test[
	{
		SuperscriptBox["a", "b"],
		"\[IndentingNewLine]",
		SubscriptBox["c", "d"]
	} // boxesToInputFormBoxes
	,
	{
		RowBox[{"a", "^", "b"}],
		"\[IndentingNewLine]",
		RowBox[{"Subscript", "[", RowBox[{"c", ",", " ", "d"}], "]"}]
	}
	,
	TestID -> "List of boxes"
]


Test[
	Catch[
		RowBox[{"f", "["}] // boxesToInputFormBoxes;
		,
		_
		,
		HoldComplete
	]
	,
	HoldComplete @@ {
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::invalid,
				"MessageParameters" -> {
					HoldForm @ boxesToInputFormBoxes[RowBox[{"f", "["}]],
					HoldForm @ CellsToTeXException["Invalid", "Boxes"],
					HoldForm @ "Boxes",
					HoldForm @ RowBox[{"f", "["}]
				},
				"Type" -> {"Invalid", "Boxes"}
			]
		],
		CellsToTeXException["Invalid", "Boxes"]
	}
	,
	Message[Syntax::bktmcp, DisplayForm[RowBox[List["f", "["]]], "]", "", ""]
	,
	TestID -> "Exception: Invalid Boxes"
]


Block[{FrontEndExecute = $Failed&},
	Test[
		Catch[
			SuperscriptBox["a", "b"] // boxesToInputFormBoxes;
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::failed,
					"MessageParameters" -> {
						HoldForm @ boxesToInputFormBoxes @
							SuperscriptBox["a", "b"],
						HoldForm @ CellsToTeXException["Failed", "Parser"],
						HoldForm @ FrontEndExecute @
							FrontEnd`UndocumentedTestFEParserPacket[
								"a^b", False
							]
					},
					"Type" -> {"Failed", "Parser"}
				]
			],
			CellsToTeXException["Failed", "Parser"]
		}
		,
		TestID -> "Exception: Failed Parser"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[boxesToInputFormBoxes[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[boxesToInputFormBoxes[testArg1, testArg2]]
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
