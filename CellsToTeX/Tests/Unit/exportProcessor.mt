(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`exportProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Configuration`", "CellsToTeX`Backports`"}, $ContextPath]


SetAttributes[withMockedFunctions, HoldAll]

withMockedFunctions[lookedupBoxes_, body_] :=
	Module[{testOption, testValue},
		Block[
			{
				processorDataLookup, $processorDataLookupLog = {},
				testLookedupStyle, testLookedupExportFormat,
				testLookedupAdditionalCellOptions
				,
				testLookedupFileNameGenerator,
				$testLookedupFileNameGeneratorLog = {},
				testLookedupFileNameGeneratorResult
				,
				Export, $exportLog = {}, testExportResult
				,
				defaultExportFormat, defaultFileNameGenerator,
				defaultAdditionalCellOptions
				,
				testData
			}
			,
			SetAttributes[processorDataLookup, HoldFirst];
			mockFunction[
				processorDataLookup,
				$processorDataLookupLog,
				{
					lookedupBoxes, testLookedupStyle,
					testLookedupAdditionalCellOptions,
					testLookedupExportFormat, testLookedupFileNameGenerator
				}
			];
			mockFunction[
				testLookedupFileNameGenerator,
				$testLookedupFileNameGeneratorLog,
				testLookedupFileNameGeneratorResult
			];
			mockFunction[Export, $exportLog, testExportResult];
			SetOptions[exportProcessor,
				"AdditionalCellOptions" -> defaultAdditionalCellOptions,
				"ExportFormat" -> defaultExportFormat,
				"FileNameGenerator" -> defaultFileNameGenerator
			];
			testData = {testOption -> testValue};
			
			body
		]
	]


(* ::Section:: *)
(*Tests*)


withMockedFunctions[
	testLookedupBoxes
	,
	testCell =
		Cell[
			BoxData[testLookedupBoxes], testLookedupStyle,
			testLookedupAdditionalCellOptions
		];
	
	Test[
		exportProcessor[testData]
		,
		{"FileName" -> testExportResult, testData}
		,
		TestID -> "bare boxes: returned value"
	];
	
	Test[
		$testLookedupFileNameGeneratorLog
		,
		{HoldComplete @@ {testCell, testLookedupExportFormat}}
		,
		TestID -> "bare boxes: fileNameGenerator"
	];
	Test[
		$exportLog
		,
		{HoldComplete @@ {
			testLookedupFileNameGeneratorResult, testCell,
			testLookedupExportFormat, testData
		}}
		,
		TestID -> "bare boxes: Export"
	];
	
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData},
			{HoldComplete[
				exportProcessor[testData],
				{
					testData,
					{
						"AdditionalCellOptions" ->
							defaultAdditionalCellOptions,
						"ExportFormat" -> defaultExportFormat,
						"FileNameGenerator" -> defaultFileNameGenerator
					}
				},
				{
					"Boxes", "Style", "AdditionalCellOptions", "ExportFormat",
					"FileNameGenerator"
				}
			]}
		]
		,
		TestID -> "passing data to processorDataLookup"
	]
]


withMockedFunctions[
	BoxData[testLookedupBoxes]
	,
	testCell =
		Cell[
			BoxData[testLookedupBoxes], testLookedupStyle,
			testLookedupAdditionalCellOptions
		];
	
	Test[
		exportProcessor[testData]
		,
		{"FileName" -> testExportResult, testData}
		,
		TestID -> "BoxData: returned value"
	];
	
	Test[
		$testLookedupFileNameGeneratorLog
		,
		{HoldComplete @@ {testCell, testLookedupExportFormat}}
		,
		TestID -> "BoxData: fileNameGenerator"
	];
	Test[
		$exportLog
		,
		{HoldComplete @@ {
			testLookedupFileNameGeneratorResult, testCell,
			testLookedupExportFormat, testData
		}}
		,
		TestID -> "BoxData: Export"
	]
]


withMockedFunctions[
	TextData[testLookedupBoxes]
	,
	testCell =
		Cell[
			TextData[testLookedupBoxes], testLookedupStyle,
			testLookedupAdditionalCellOptions
		];
	
	Test[
		exportProcessor[testData]
		,
		{"FileName" -> testExportResult, testData}
		,
		TestID -> "TextData: returned value"
	];
	
	Test[
		$testLookedupFileNameGeneratorLog
		,
		{HoldComplete @@ {testCell, testLookedupExportFormat}}
		,
		TestID -> "TextData: fileNameGenerator"
	];
	Test[
		$exportLog
		,
		{HoldComplete @@ {
			testLookedupFileNameGeneratorResult, testCell,
			testLookedupExportFormat, testData
		}}
		,
		TestID -> "TextData: Export"
	]
]


withMockedFunctions[
	Cell[
		BoxData[testLookedupBoxes], testCellStyle,
		testCellOption -> testCellValue
	]
	,
	testCell =
		Cell[
			BoxData[testLookedupBoxes], testLookedupStyle, testCellStyle,
			testLookedupAdditionalCellOptions,
			testCellOption -> testCellValue
		];
	
	Test[
		exportProcessor[testData]
		,
		{"FileName" -> testExportResult, testData}
		,
		TestID -> "Cell: returned value"
	];
	
	Test[
		$testLookedupFileNameGeneratorLog
		,
		{HoldComplete @@ {testCell, testLookedupExportFormat}}
		,
		TestID -> "Cell: fileNameGenerator"
	];
	Test[
		$exportLog
		,
		{HoldComplete @@ {
			testLookedupFileNameGeneratorResult, testCell,
			testLookedupExportFormat, testData
		}}
		,
		TestID -> "Cell: Export"
	]
]


withMockedFunctions[
	testLookedupBoxes
	,
	testCell =
		Cell[
			BoxData[testLookedupBoxes], testLookedupStyle,
			testLookedupAdditionalCellOptions
		];
	mockFunction[Export, $exportLog, $Failed];
	
	Test[
		Catch[
			exportProcessor[testData]
			,
			_
			,
			HoldComplete
		]
		,
		With[{testCell = testCell, testData = testData},
			HoldComplete @@ {
				Failure[CellsToTeXException,
					Association[
						"MessageTemplate" :> CellsToTeXException::failed,
						"MessageParameters" -> {
							HoldForm @ exportProcessor[testData],
							HoldForm @ CellsToTeXException["Failed", "Export"],
							HoldForm @ Export[
								testLookedupFileNameGeneratorResult, testCell,
								testLookedupExportFormat, testData
							]
						},
						"Type" -> {"Failed", "Export"}
					]
				],
				CellsToTeXException["Failed", "Export"]
			}
		]
		,
		TestID -> "failed export: returned value"
	];
	
	Test[
		$testLookedupFileNameGeneratorLog
		,
		{HoldComplete @@ {testCell, testLookedupExportFormat}}
		,
		TestID -> "failed export: fileNameGenerator"
	];
	Test[
		$exportLog
		,
		{HoldComplete @@ {
			testLookedupFileNameGeneratorResult, testCell,
			testLookedupExportFormat, testData
		}}
		,
		TestID -> "failed export: Export"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[exportProcessor], Protected]
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
