(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`boxesToTeXProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[withMockedFunctions, HoldAll]

withMockedFunctions[body_] :=
	Block[
		{
			processorDataLookup, $processorDataLookupLog = {},
			testLookedupBoxes, testLookedupBoxRuleLHS, testLookedupBoxRuleRHS,
			testLookedupTeXCodeSimplifier
			,
			CellsToTeX`Internal`boxesToString, $boxesToStringLog = {},
			testBoxesToStringResult
			,
			$testTeXCodeSimplifierLog = {}, testTeXCodeSimplifierResult
			,
			testDefaultOptions,
			testDefaultBoxRules, testDefaultTeXCodeSimplifier
			,
			testData, testToStringOptions,
			testBoxes, testBoxRules, testTeXCodeSimplifier, testFormatType,
			testCharacterEncoding, testPageHeight, testUnusedOptionName,
			testUnusedOptionValue
			,
			$basicBoxes, testBasicBoxes,
			testExtendedBoxRules
		}
		,
		SetAttributes[processorDataLookup, HoldFirst];
		mockFunction[
			processorDataLookup,
			$processorDataLookupLog,
			{
				testLookedupBoxes,
				{testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS},
				testLookedupTeXCodeSimplifier
			}
		];
		
		mockFunction[
			CellsToTeX`Internal`boxesToString,
			$boxesToStringLog,
			testBoxesToStringResult
		];
		
		mockFunction[
			testLookedupTeXCodeSimplifier,
			$testTeXCodeSimplifierLog,
			testTeXCodeSimplifierResult
		];
		
		testDefaultOptions = {
			"BoxRules" -> testDefaultBoxRules,
			"TeXCodeSimplifier" -> testDefaultTeXCodeSimplifier
		};
		SetOptions[boxesToTeXProcessor, testDefaultOptions];
		
		testToStringOptions = {
			"FormatType" -> testFormatType,
			"CharacterEncoding" -> testCharacterEncoding,
			"PageHeight" -> testPageHeight
		};
		testData =
			Join[
				{
					"Boxes" -> testBoxes,
					"BoxRules" -> testBoxRules,
					"TeXCodeSimplifier" -> testTeXCodeSimplifier,
					testUnusedOptionName -> testUnusedOptionValue
				},
				testToStringOptions
			];
		
		$basicBoxes = testBasicBoxes;
		
		testExtendedBoxRules = {
			testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS,
			With[{testData = testData},
				HoldPattern[
					Verbatim[Pattern][
						unsupBoxPattName_, Verbatim[Except][testBasicBoxes]
					] :>
						CellsToTeX`Internal`throwException[
							boxesToTeXProcessor[testData],
							{"Unsupported", "Box"},
							{unsupBoxPattName_, {testLookedupBoxRuleLHS}}
						]
				]
			]
		};
			
		body
	]


(* ::Section:: *)
(*Tests*)


withMockedFunctions[
	Test[
		boxesToTeXProcessor[testData]
		,
		{"TeXCode" -> testTeXCodeSimplifierResult, testData}
		,
		TestID -> "basic: returned value"
	];
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData, testDefaultOptions = testDefaultOptions},
			{HoldComplete[
				boxesToTeXProcessor[testData],
				{testData, testDefaultOptions},
				{"Boxes", "BoxRules", "TeXCodeSimplifier"}
			]}
		]
		,
		TestID -> "passing data to processorDataLookup"
	];
	TestMatch[
		$boxesToStringLog
		,
		{HoldComplete @@ {
			testLookedupBoxes, testExtendedBoxRules, testToStringOptions
		}}
		,
		TestID -> "basic: boxesToString"
	];
	TestMatch[
		$testTeXCodeSimplifierLog
		,
		{HoldComplete[testBoxesToStringResult]}
		,
		TestID -> "basic: TeXCodeSimplifier"
	]
]


withMockedFunctions@Module[{testBoxDataContents},
	testLookedupBoxes = BoxData[testBoxDataContents];
	
	Test[
		boxesToTeXProcessor[testData]
		,
		{"TeXCode" -> testTeXCodeSimplifierResult, testData}
		,
		TestID -> "BoxData: returned value"
	];
	TestMatch[
		$boxesToStringLog
		,
		{HoldComplete @@ {
			testBoxDataContents, testExtendedBoxRules, testToStringOptions
		}}
		,
		TestID -> "BoxData: boxesToString"
	];
	TestMatch[
		$testTeXCodeSimplifierLog
		,
		{HoldComplete[testBoxesToStringResult]}
		,
		TestID -> "BoxData: TeXCodeSimplifier"
	]
]


withMockedFunctions@Module[{testBoxDataContents, testCellstyle},
	testLookedupBoxes = Cell[BoxData[testBoxDataContents], testCellstyle];
	
	Test[
		boxesToTeXProcessor[testData]
		,
		{"TeXCode" -> testTeXCodeSimplifierResult, testData}
		,
		TestID -> "Cell: returned value"
	];
	TestMatch[
		$boxesToStringLog
		,
		{HoldComplete @@ {
			testBoxDataContents, testExtendedBoxRules, testToStringOptions
		}}
		,
		TestID -> "Cell: boxesToString"
	];
	TestMatch[
		$testTeXCodeSimplifierLog
		,
		{HoldComplete[testBoxesToStringResult]}
		,
		TestID -> "Cell: TeXCodeSimplifier"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[boxesToTeXProcessor], Protected]
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
