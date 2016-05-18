(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"CellsToTeX`Tests`Integration`mmaCellGraphicsProcessor`", {"MUnit`"}
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


Module[{testUnusedOption, testUnusedValue, data},
	data = {
		"FileName" -> "testFileNameBasic",
		"Style" -> "testStyleBasic",
		"TeXOptions" ->
			{"testTeXOptionNameBasic" -> "testTeXOptionValueBasic"},
		testUnusedOption -> testUnusedValue
	};
	
	Test[
		data // mmaCellGraphicsProcessor
		,
		{
			"TeXCode" -> "\\mmaCellGraphics\
[testTeXOptionNameBasic=testTeXOptionValueBasic]\
{testStyleBasic}{testFileNameBasic}",
			data
		}
		,
		TestID -> "basic"
	]
]


Module[{testUnusedOption, testUnusedValue, data},
	data = {
		"FileName" -> "testFileNameETO",
		"Style" -> "testStyleETO",
		"TeXOptions" -> {},
		testUnusedOption -> testUnusedValue
	};
	
	Test[
		data // mmaCellGraphicsProcessor
		,
		{
			"TeXCode" -> "\\mmaCellGraphics{testStyleETO}{testFileNameETO}",
			data
		}
		,
		TestID -> "empty TeXOptions"
	]
]


Test[
	Catch[
		{} // mmaCellGraphicsProcessor;
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
					HoldForm @ mmaCellGraphicsProcessor[{}],
					HoldForm @ CellsToTeXException[
						"Missing", "Keys", "ProcessorArgument"
					],
					HoldForm @ "Keys",
					HoldForm @ {"FileName", "Style", "TeXOptions"},
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
	MemberQ[Attributes[mmaCellGraphicsProcessor], Protected]
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
