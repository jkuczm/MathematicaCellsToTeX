(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"CellsToTeX`Tests`Integration`annotateSyntaxProcessor`", {"MUnit`"}
]


Get["CellsToTeX`"]

$ContextPath =
	Join[
		{
			"SyntaxAnnotations`",
			"CellsToTeX`Configuration`",
			"CellsToTeX`Internal`",
			"CellsToTeX`Backports`"
		},
		$ContextPath
	]


MakeBoxes[syntaxExpr[expr_, types___], StandardForm] ^:=
	syntaxBox[MakeBoxes[expr], types]

$syntaxBoxToTeXSeq =
	Sequence @@ annotationRulesToBoxRules[$annotationTypesToTeX]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*One symbol*)


With[
	{
		data = {
			"Boxes" -> MakeBoxes[x],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> MakeBoxes[x],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "1 symbol: 1 role: default"
	]
]
With[
	{
		data = {
			"Boxes" -> MakeBoxes[x],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Block[{x = 5},
		Test[
			data // annotateSyntaxProcessor
			,
			{
				"Boxes" -> MakeBoxes[x],
				"BoxRules" -> {$syntaxBoxToTeXSeq},
				"TeXOptions" -> {"moredefined" -> {"x"}},
				data
			}
			,
			TestID -> "1 symbol: 1 role: non-default"
		]
	]
]
With[
	{
		data = {
			"Boxes" -> MakeBoxes[Module[{x}, 5] x],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" ->
				MakeBoxes[Module[{syntaxExpr[x, "LocalVariable"]}, 5] x],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "1 symbol: 2 roles: default dominant"
	]
]
With[
	{
		data = {
			"Boxes" -> MakeBoxes[Module[{x}, x] x],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" ->
				MakeBoxes[Module[{x}, x] syntaxExpr[x, "UndefinedSymbol"]],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {"morelocal" -> {"x"}},
			data
		}
		,
		TestID -> "1 symbol: 2 roles: non-default dominant"
	]
]
With[
	{
		data = {
			"Boxes" -> MakeBoxes[Block[{x=x}, x] Module[{x}, 9]],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> MakeBoxes[
				Block[{x=syntaxExpr[x, "UndefinedSymbol"]}, x] *
				Module[{syntaxExpr[x, "LocalVariable"]}, 9]
			]
			,
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {"morefunctionlocal" -> {"x"}},
			data
		}
		,
		TestID -> "1 symbol: 3 roles: non-default dominant"
	]
]


(* ::Subsection:: *)
(*Two symbols*)


With[
	{
		data = {
			"Boxes" -> MakeBoxes[x y],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> MakeBoxes[x y],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "2 symbols: same role: default"
	]
]
With[
	{
		data = {
			"Boxes" -> MakeBoxes[x y],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Block[{x = "test", y = 2},
		Test[
			data // annotateSyntaxProcessor
			,
			{
				"Boxes" -> MakeBoxes[x y],
				"BoxRules" -> {$syntaxBoxToTeXSeq},
				"TeXOptions" -> {"moredefined" -> {"x", "y"}},
				data
			}
			,
			TestID -> "2 symbols: same role: non-default"
		]
	]
]
With[
	{
		data = {
			"Boxes" -> MakeBoxes[Module[{x}, y]],
			"BoxRules" -> {},
			"TeXOptions" -> {}
		}
	},
	Block[{y = 2},
		Test[
			data // annotateSyntaxProcessor
			,
			{
				"Boxes" -> MakeBoxes[Module[{x}, y]],
				"BoxRules" -> {$syntaxBoxToTeXSeq},
				"TeXOptions" -> {"morelocal" -> {"x"}, "moredefined" -> {"y"}},
				data
			}
			,
			TestID -> "2 symbols: different roles: non-default, non-default"
		]
	]
]


(* ::Subsection:: *)
(*CommonestTypesAsTeXOptions option*)


With[
	{
		data = {
			"Boxes" -> MakeBoxes[f[x_] := x y],
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"CommonestTypesAsTeXOptions" -> True
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> MakeBoxes[f[x_] := x y],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {"morepattern" -> {"x_", "x"}},
			data
		}
		,
		TestID -> "CommonestTypesAsTeXOptions -> True: ASCII symbols"
	]
]
With[
	{
		data = {
			"Boxes" ->
				MakeBoxes[\[Phi][\[Epsilon]_] := \[Epsilon] \[Epsilon]0],
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"CommonestTypesAsTeXOptions" -> True
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" ->
				MakeBoxes[\[Phi][\[Epsilon]_] := \[Epsilon] \[Epsilon]0],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {"morepattern" -> {"\[Epsilon]_", "\[Epsilon]"}},
			data
		}
		,
		TestID -> "CommonestTypesAsTeXOptions -> True: non-ASCII symbols"
	]
]


With[
	{
		data = {
			"Boxes" -> MakeBoxes[f[x_] := x y],
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"CommonestTypesAsTeXOptions" -> "ASCII"
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> MakeBoxes[f[x_] := x y],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {"morepattern" -> {"x_", "x"}},
			data
		}
		,
		TestID -> "CommonestTypesAsTeXOptions -> ASCII: ASCII symbols"
	]
]
With[
	{
		data = {
			"Boxes" ->
				MakeBoxes[\[Phi][\[Epsilon]_] := \[Epsilon] \[Epsilon]0],
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"CommonestTypesAsTeXOptions" -> "ASCII"
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" ->
				RowBox[{
					RowBox[{syntaxBox["\[Phi]", "UndefinedSymbol"], "[",
						syntaxBox["\[Epsilon]_", "PatternVariable"],
					"]"}],
					":=",
					RowBox[{
						syntaxBox["\[Epsilon]", "PatternVariable"],
						" ",
						syntaxBox["\[Epsilon]0", "UndefinedSymbol"]
					}]
				}],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "CommonestTypesAsTeXOptions -> ASCII: non-ASCII symbols"
	]
]


With[
	{
		data = {
			"Boxes" -> MakeBoxes[f[x_] := x y],
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"CommonestTypesAsTeXOptions" -> False
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" ->
				RowBox[{
					RowBox[{syntaxBox["f", "UndefinedSymbol"], "[",
						syntaxBox["x_", "PatternVariable"],
					"]"}],
					":=",
					RowBox[{
						syntaxBox["x", "PatternVariable"],
						" ",
						syntaxBox["y", "UndefinedSymbol"]
					}]
				}],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "CommonestTypesAsTeXOptions -> False: ASCII symbols"
	]
]
With[
	{
		data = {
			"Boxes" ->
				MakeBoxes[\[Phi][\[Epsilon]_] := \[Epsilon] \[Epsilon]0],
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"CommonestTypesAsTeXOptions" -> False
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" ->
				RowBox[{
					RowBox[{syntaxBox["\[Phi]", "UndefinedSymbol"], "[",
						syntaxBox["\[Epsilon]_", "PatternVariable"],
					"]"}],
					":=",
					RowBox[{
						syntaxBox["\[Epsilon]", "PatternVariable"],
						" ",
						syntaxBox["\[Epsilon]0", "UndefinedSymbol"]
					}]
				}],
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "CommonestTypesAsTeXOptions -> False: non-ASCII symbols"
	]
]


(* ::Subsection:: *)
(*Given options handling*)


With[
	{
		data = {
			"Boxes" -> "",
			"BoxRules" -> {testBoxRule},
			"TeXOptions" -> {}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> "",
			"BoxRules" -> {$syntaxBoxToTeXSeq, testBoxRule},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "BoxRules passing"
	]
]
With[
	{
		data = {
			"Boxes" -> "x",
			"BoxRules" -> {testBoxRule},
			"TeXOptions" -> {}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> "x",
			"BoxRules" -> {$syntaxBoxToTeXSeq, testBoxRule},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "BoxRules appending"
	]
]


With[
	{
		data = {
			"Boxes" -> "",
			"BoxRules" -> {},
			"TeXOptions" -> {testTeXOption}
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> "",
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {testTeXOption},
			data
		}
		,
		TestID -> "TeXOptions passing"
	]
]
With[
	{
		data = {
			"Boxes" -> "x",
			"BoxRules" -> {},
			"TeXOptions" -> {testTeXOption}
		}
	},
	Block[{x = 1},
		Test[
			data // annotateSyntaxProcessor
			,
			{
				"Boxes" -> "x",
				"BoxRules" -> {$syntaxBoxToTeXSeq},
				"TeXOptions" -> {testTeXOption, "moredefined" -> {"x"}},
				data
			}
			,
			TestID -> "TeXOptions appending"
		]
	]
]

With[
	{
		data = {
			"Boxes" -> "",
			"BoxRules" -> {},
			"TeXOptions" -> {},
			testUnusedOption -> testUnusedValue
		}
	},
	Test[
		data // annotateSyntaxProcessor
		,
		{
			"Boxes" -> "",
			"BoxRules" -> {$syntaxBoxToTeXSeq},
			"TeXOptions" -> {},
			data
		}
		,
		TestID -> "unused option passing"
	]
]


(* ::Subsection:: *)
(*Exceptions*)


Test[
	Catch[
		{} // annotateSyntaxProcessor;
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
					HoldForm @ annotateSyntaxProcessor[{}],
					HoldForm @ CellsToTeXException[
						"Missing", "Keys", "ProcessorArgument"
					],
					HoldForm @ "Keys",
					HoldForm @ {"Boxes", "TeXOptions"},
					HoldForm @ {
						"BoxRules", "AnnotationTypesToTeX",
						"CommonestTypesAsTeXOptions", "StringBoxToTypes",
						"AnnotateComments"
					}
				},
				"Type" -> {"Missing", "Keys", "ProcessorArgument"}
			]
		],
		CellsToTeXException["Missing", "Keys", "ProcessorArgument"]
	}
	,
	TestID -> "Exception: Missing keys in processor argument"
]


With[
	{
		data = {
			"Boxes" -> "testUndefinedSymbol",
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"AnnotationTypesToTeX" -> {
				"testSupportedAnnotation" -> {"key", "command"}
			}
		}
	},
	Block[{defaultAnnotationType = "testDefaultAnnotation"&},
		Test[
			Catch[
				data // annotateSyntaxProcessor;
				,
				_
				,
				HoldComplete
			]
			,
			HoldComplete @@ {
				Failure[CellsToTeXException,
					Association[
						"MessageTemplate" :> CellsToTeXException::unsupported,
						"MessageParameters" -> {
							HoldForm @ annotateSyntaxProcessor[data],
							HoldForm @ CellsToTeXException[
								"Unsupported", "AnnotationType"
							],
							HoldForm @ "AnnotationType",
							HoldForm @ "UndefinedSymbol",
							HoldForm @ {"testSupportedAnnotation"}
						},
						"Type" -> {"Unsupported", "AnnotationType"}
					]
				],
				CellsToTeXException["Unsupported", "AnnotationType"]
			}
			,
			TestID -> "Exception: Unsupported AnnotationType"
		]
	]
]


With[
	{
		data = {
			"Boxes" -> "testUndefinedSymbol",
			"BoxRules" -> {},
			"TeXOptions" -> {},
			"CommonestTypesAsTeXOptions" -> "testUnsupportedOptVal"
		}
	},
	Test[
		Catch[
			data // annotateSyntaxProcessor;
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::unsupported,
					"MessageParameters" -> {
						HoldForm @ annotateSyntaxProcessor[data],
						HoldForm @ CellsToTeXException[
							"Unsupported", "OptionValue",
							"CommonestTypesAsTeXOptions"
						],
						HoldForm @ "OptionValue",
						HoldForm @ "testUnsupportedOptVal",
						HoldForm @ {True, "ASCII", False}
					},
					"Type" -> {
						"Unsupported", "OptionValue",
						"CommonestTypesAsTeXOptions"
					}
				]
			],
			CellsToTeXException[
				"Unsupported", "OptionValue", "CommonestTypesAsTeXOptions"
			]
		}
		,
		TestID ->
			"Exception: Unsupported OptionValue CommonestTypesAsTeXOptions"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[annotateSyntaxProcessor], Protected]
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
