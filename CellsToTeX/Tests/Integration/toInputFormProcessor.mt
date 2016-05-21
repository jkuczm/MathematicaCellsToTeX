(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`toInputFormProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

$ContextPath =
	Join[
		{
			"CellsToTeX`Configuration`",
			"CellsToTeX`Internal`",
			"CellsToTeX`Backports`"
		},
		$ContextPath
	]


(* ::Section:: *)
(*Tests*)


Module[{data},
	data = {
		"Boxes" -> SuperscriptBox["a", "b"],
		testUnusedOption -> testUnusedValue
	};
	
	UsingFrontEnd @ Test[
		data // toInputFormProcessor
		,
		{"Boxes" -> RowBox[{"a", "^", "b"}], data}
		,
		TestID -> "pure boxes"
	]
]


Module[{data},
	data = {"Boxes" -> BoxData[SubscriptBox["a", "b"]]};
	
	UsingFrontEnd @ Test[
		data // toInputFormProcessor
		,
		{
			"Boxes" ->
				BoxData @ RowBox @
					{"Subscript", "[", RowBox[{"a", ",", " ", "b"}], "]"}
			,
			data
		}
		,
		TestID -> "BoxData"
	]
]


Module[{data},
	data = {"Boxes" -> BoxData[{"a", "\[IndentingNewLine]", "b"}]};
	
	UsingFrontEnd @ Test[
		data // toInputFormProcessor
		,
		{"Boxes" -> BoxData[{"a", "\[IndentingNewLine]", "b"}], data}
		,
		TestID -> "BoxData with List"
	]
]


Module[{data},
	data = {
		"Boxes" ->
			Cell[
				BoxData[FractionBox["x", "y"]],
				"Input",
				CellLabel -> "In[1]:="
			]
	};
	
	UsingFrontEnd @ Test[
		data // toInputFormProcessor
		,
		{
			"Boxes" ->
				Cell[
					BoxData[RowBox[{"x", "/", "y"}]],
					"Input",
					CellLabel -> "In[1]:="
				],
			data
		}
		,
		TestID -> "Cell"
	]
]


Test[
	Catch[
		{} // toInputFormProcessor;
		,
		_
		,
		HoldComplete
	]
	,
	HoldComplete @@ {
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::missingProcArg,
				"MessageParameters" -> {
					HoldForm @ toInputFormProcessor[{}],
					HoldForm @ CellsToTeXException[
						"Missing", "Keys", "ProcessorArgument"
					],
					HoldForm @ "Keys",
					HoldForm @ {"Boxes"},
					HoldForm @ {}
				},
				"Type" -> {"Missing", "Keys", "ProcessorArgument"}
			]
		],
		CellsToTeXException["Missing", "Keys", "ProcessorArgument"]
	}
	,
	TestID -> "Exception: Missing keys in processor argument"
]


Test[
	Catch[
		{"Boxes" -> RowBox[{"f", "["}]} // toInputFormProcessor;
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
					HoldForm @
						toInputFormProcessor[{"Boxes" -> RowBox[{"f", "["}]}],
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
			{"Boxes" -> SuperscriptBox["a", "b"]} // toInputFormProcessor;
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
						HoldForm @ toInputFormProcessor @
							{"Boxes" -> SuperscriptBox["a", "b"]},
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
(*Protected attribute*)


Test[
	MemberQ[Attributes[toInputFormProcessor], Protected]
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
