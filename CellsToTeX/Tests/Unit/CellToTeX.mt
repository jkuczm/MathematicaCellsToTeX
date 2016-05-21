(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`CellToTeX`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[
		{
			"CellsToTeX`Configuration`",
			"CellsToTeX`Internal`",
			"CellsToTeX`Backports`"
		},
		$ContextPath
	]


SetAttributes[moduleWithMockedFunctions, HoldAll]
moduleWithMockedFunctions[vars_List, body_] :=
	Module[vars,
		Block[
			{
				extractStyleOptions, $extractStyleOptionsLog = {},
				testExtractStyleOptionsResult
				,
				dataLookup, $dataLookupLog = {},
				testDataLookupResult, testLookedupResult
				,
				testLookedupProcessor, $lookedupProcessorLog = {},
				testLookedupProcessorResult
				,
				catchException, $catchExceptionLog = {},
				testCatchExceptionResult
				,
				testDefaultCellContentsToTeXOptions,
				testDefaultCellStyleOptions,
				testDefaultDataProcOpt, testDefaultDataProcVal,
				testDefaultProcessor
			}
			,
			mockFunction[
				extractStyleOptions,
				$extractStyleOptionsLog,
				testExtractStyleOptionsResult
			];
			mockFunction[dataLookup, $dataLookupLog, testDataLookupResult];
			dataLookup[data_, "Processor", rest___] := (
				AppendTo[$dataLookupLog,
					HoldComplete[data, "Processor", rest]
				];
				testLookedupProcessor
			);
			dataLookup[data_, "TeXCode", rest___] := (
				AppendTo[$dataLookupLog, HoldComplete[data, "TeXCode", rest]];
				testLookedupResult
			);
			mockFunction[
				testLookedupProcessor, $lookedupProcessorLog,
				testLookedupProcessorResult
			];
			mockFunction[
				catchException, $catchExceptionLog,
				testCatchExceptionResult
			];
			
			testDefaultCellContentsToTeXOptions =
				SetOptions[CellToTeX,
					"Style" -> "testCellStyle",
					"SupportedCellStyles" -> "testCellStyle",
					"CellStyleOptions" -> testDefaultCellStyleOptions,
					"ProcessorOptions" ->
						{testDefaultDataProcOpt -> testDefaultDataProcVal},
					"Processor" -> testDefaultProcessor,
					"CatchExceptions" -> False
				];
			
			body
		]
	]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Basic*)


moduleWithMockedFunctions[{testContents, testData},
	testData = {
		"Boxes" -> testContents,
		"Style" -> "testCellStyle",
		{testDefaultDataProcOpt -> testDefaultDataProcVal},
		testExtractStyleOptionsResult,
		testDefaultCellContentsToTeXOptions
	};
	
	Test[
		CellToTeX[testContents]
		,
		testLookedupResult
		,
		TestID -> "Basic: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{HoldComplete["testCellStyle", testDefaultCellStyleOptions]}
		,
		TestID -> "Basic: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{
			HoldComplete @@ {testData, "Processor"},
			HoldComplete[testLookedupProcessorResult, "TeXCode"]
		}
		,
		TestID -> "Basic: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{HoldComplete @@ {testData}}
		,
		TestID -> "Basic: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{}
		,
		TestID -> "Basic: catchException"
	]
]


(* ::Subsection:: *)
(*Unsupported style*)


moduleWithMockedFunctions[{testContents},
	Test[
		Catch[
			CellToTeX[testContents,
				"Style" -> "testUnsupportedCellStyle"
			];
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
						HoldForm @ CellToTeX[testContents,
							"Style" -> "testUnsupportedCellStyle"
						],
						HoldForm @
							CellsToTeXException["Unsupported", "CellStyle"],
						HoldForm @ "CellStyle",
						HoldForm @ "testUnsupportedCellStyle",
						HoldForm @ "testCellStyle"
					},
					"Type" -> {"Unsupported", "CellStyle"}
				]
			],
			CellsToTeXException["Unsupported", "CellStyle"]
		}
		,
		TestID -> "Unsupported style: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{}
		,
		TestID -> "Unsupported style: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{}
		,
		TestID -> "Unsupported style: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{}
		,
		TestID -> "Unsupported style: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{}
		,
		TestID -> "Unsupported style: catchException"
	]
]


(* ::Subsection:: *)
(*CellStyleOptions*)


moduleWithMockedFunctions[
	{testContents, testCustomCellStyleOpt, testCustomCellStyleVal, testData}
	,
	testData = {
		"Boxes" -> testContents,
		"Style" -> "testCellStyle$CellStyleOptions",
		"Style" -> "testCellStyle$CellStyleOptions",
		"SupportedCellStyles" -> "testCellStyle$CellStyleOptions",
		"CellStyleOptions" -> {
			"testCellStyle$CellStyleOptions" ->
				{testCustomCellStyleOpt -> testCustomCellStyleVal}
		},
		{testDefaultDataProcOpt -> testDefaultDataProcVal},
		testExtractStyleOptionsResult,
		testDefaultCellContentsToTeXOptions
	};
	
	Test[
		CellToTeX[testContents,
			"Style" -> "testCellStyle$CellStyleOptions",
			"SupportedCellStyles" -> "testCellStyle$CellStyleOptions",
			"CellStyleOptions" -> {
				"testCellStyle$CellStyleOptions" ->
					{testCustomCellStyleOpt -> testCustomCellStyleVal}
			}
		]
		,
		testLookedupResult
		,
		TestID -> "CellStyleOptions: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{HoldComplete[
			"testCellStyle$CellStyleOptions",
			{
				"testCellStyle$CellStyleOptions" ->
					{testCustomCellStyleOpt -> testCustomCellStyleVal}
			}
		]}
		,
		TestID -> "CellStyleOptions: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{
			HoldComplete @@ {testData, "Processor"},
			HoldComplete[testLookedupProcessorResult, "TeXCode"]
		}
		,
		TestID -> "CellStyleOptions: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{HoldComplete @@ {testData}}
		,
		TestID -> "CellStyleOptions: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{}
		,
		TestID -> "CellStyleOptions: catchException"
	]
]


(* ::Subsection:: *)
(*ProcessorOptions*)


moduleWithMockedFunctions[
	{testContents, testCustomDataProcOpt, testCustomDataProcVal, testData}
	,
	testData = {
		"Boxes" -> testContents,
		"Style" -> "testCellStyle",
		"ProcessorOptions" ->
				{testCustomDataProcOpt -> testCustomDataProcVal},
		{testCustomDataProcOpt -> testCustomDataProcVal},
		testExtractStyleOptionsResult,
		testDefaultCellContentsToTeXOptions
	};
	
	Test[
		CellToTeX[testContents,
			"ProcessorOptions" ->
				{testCustomDataProcOpt -> testCustomDataProcVal}
		]
		,
		testLookedupResult
		,
		TestID -> "ProcessorOptions: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{HoldComplete["testCellStyle", testDefaultCellStyleOptions]}
		,
		TestID -> "ProcessorOptions: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{
			HoldComplete @@ {testData, "Processor"},
			HoldComplete[testLookedupProcessorResult, "TeXCode"]
		}
		,
		TestID -> "ProcessorOptions: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{HoldComplete @@ {testData}}
		,
		TestID -> "ProcessorOptions: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{}
		,
		TestID -> "ProcessorOptions: catchException"
	]
]


(* ::Subsection:: *)
(*Processor*)


moduleWithMockedFunctions[{testContents, testCustomProcessor, testData},
	testData = {
		"Boxes" -> testContents,
		"Style" -> "testCellStyle",
		"Processor" -> testCustomProcessor,
		{testDefaultDataProcOpt -> testDefaultDataProcVal},
		testExtractStyleOptionsResult,
		testDefaultCellContentsToTeXOptions
	};
	
	Test[
		CellToTeX[testContents, "Processor" -> testCustomProcessor]
		,
		testLookedupResult
		,
		TestID -> "Processor: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{HoldComplete["testCellStyle", testDefaultCellStyleOptions]}
		,
		TestID -> "Processor: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{
			HoldComplete @@ {testData, "Processor"},
			HoldComplete[testLookedupProcessorResult, "TeXCode"]
		}
		,
		TestID -> "Processor: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{HoldComplete @@ {testData}}
		,
		TestID -> "Processor: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{}
		,
		TestID -> "Processor: catchException"
	]
]


(* ::Subsection:: *)
(*CatchExceptions*)


moduleWithMockedFunctions[{testContents, testData},
	testData = {
		"Boxes" -> testContents,
		"Style" -> "testCellStyle",
		"CatchExceptions" -> True,
		{testDefaultDataProcOpt -> testDefaultDataProcVal},
		testExtractStyleOptionsResult,
		testDefaultCellContentsToTeXOptions
	};
	
	Test[
		CellToTeX[testContents, "CatchExceptions" -> True]
		,
		testCatchExceptionResult
		,
		TestID -> "CatchExceptions: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{HoldComplete["testCellStyle", testDefaultCellStyleOptions]}
		,
		TestID -> "CatchExceptions: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{
			HoldComplete @@ {testData, "Processor"},
			HoldComplete[testLookedupProcessorResult, "TeXCode"]
		}
		,
		TestID -> "CatchExceptions: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{HoldComplete @@ {testData}}
		,
		TestID -> "CatchExceptions: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{HoldComplete[testLookedupResult]}
		,
		TestID -> "CatchExceptions: catchException"
	]
]


(* ::Subsection:: *)
(*Automatic Style*)


moduleWithMockedFunctions[{testContents, testData},
	testData = {
		"Boxes" -> Cell[testContents, "testCellStyle"],
		"Style" -> "testCellStyle",
		{testDefaultDataProcOpt -> testDefaultDataProcVal},
		testExtractStyleOptionsResult,
		testDefaultCellContentsToTeXOptions
	};
	
	Test[
		CellToTeX[Cell[testContents, "testCellStyle"]]
		,
		testLookedupResult
		,
		TestID -> "Automatic Style: existing: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{HoldComplete["testCellStyle", testDefaultCellStyleOptions]}
		,
		TestID -> "Automatic Style: existing: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{
			HoldComplete @@ {testData, "Processor"},
			HoldComplete[testLookedupProcessorResult, "TeXCode"]
		}
		,
		TestID -> "Automatic Style: existing: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{HoldComplete @@ {testData}}
		,
		TestID -> "Automatic Style: existing: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{}
		,
		TestID -> "Automatic Style: existing: catchException"
	]
]


moduleWithMockedFunctions[{testContents},
	Test[
		Catch[
			CellToTeX[testContents, "Style" -> Automatic];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missingCellStyle,
					"MessageParameters" -> {
						HoldForm @
							CellToTeX[testContents, "Style" -> Automatic],
						HoldForm @ CellsToTeXException["Missing", "CellStyle"],
						HoldForm @ testContents
					},
					"Type" -> {"Missing", "CellStyle"}
				]
			],
			CellsToTeXException["Missing", "CellStyle"]
		}
		,
		TestID -> "Automatic Style: missing: returned value"
	];
	
	Test[
		$extractStyleOptionsLog
		,
		{}
		,
		TestID -> "Automatic Style: missing: extractStyleOptions"
	];
	TestMatch[
		$dataLookupLog
		,
		{}
		,
		TestID -> "Automatic Style: missing: dataLookup"
	];
	Test[
		$lookedupProcessorLog
		,
		{}
		,
		TestID -> "Automatic Style: missing: lookedupProcessor"
	];
	Test[
		$catchExceptionLog
		,
		{}
		,
		TestID -> "Automatic Style: missing: catchException"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[CellToTeX], Protected]
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
