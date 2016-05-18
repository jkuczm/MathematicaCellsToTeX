(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"CellsToTeX`Tests`Integration`extractCellOptionsProcessor`", {"MUnit`"}
]


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


Module[{testBoxes, testUnusedOption, testUnusedValue, data},
	data = {
		"Boxes" -> testBoxes,
		testUnusedOption -> testUnusedValue
	};
	
	Test[
		data // extractCellOptionsProcessor
		,
		data
		,
		TestID -> "non-cell"
	]
]

Module[
	{
		testBoxes, testCellStyle, testUnusedOption, testUnusedValue,
		testCellOption1Name, testCellOption1Value,
		testCellOption2Name, testCellOption2Value,
		data
	}
	,
	data = {
		"Boxes" ->
			Cell[
				testBoxes, testCellStyle,
				testCellOption1Name -> testCellOption1Value,
				testCellOption2Name -> testCellOption2Value
			],
		testUnusedOption -> testUnusedValue
	};
	
	Test[
		data // extractCellOptionsProcessor
		,
		{
			data,
			{
				testCellOption1Name -> testCellOption1Value,
				testCellOption2Name -> testCellOption2Value
			}
		}
		,
		TestID -> "Cell with options"
	]
]


Test[
	Catch[
		{} // extractCellOptionsProcessor;
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
					HoldForm @ extractCellOptionsProcessor[{}],
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


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[extractCellOptionsProcessor], Protected]
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
