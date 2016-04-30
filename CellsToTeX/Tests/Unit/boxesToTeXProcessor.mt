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
			testLookedupBoxes, testLookedupBoxRuleLHS, testLookedupBoxRuleRHS
			,
			CellsToTeX`Internal`boxesToString, $boxesToStringLog = {},
			testBoxesToStringResult
			,
			$commandCharsToTeX = $commandCharsToTeX
			,
			testDefaultBoxRules
			,
			testData, testToStringOptions,
			testBoxes, testBoxRules, testFormatType, testCharacterEncoding,
			testPageHeight, testUnusedOptionName, testUnusedOptionValue
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
				{testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS}
			}
		];
		
		testBoxesToStringResult = "testBoxesToStringResult";
		mockFunction[
			CellsToTeX`Internal`boxesToString,
			$boxesToStringLog,
			testBoxesToStringResult
		];
		
		SetOptions[boxesToTeXProcessor, {"BoxRules" -> testDefaultBoxRules}];
		
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
		{"TeXCode" -> testBoxesToStringResult, testData}
		,
		TestID -> "basic: returned value"
	];
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData},
			{HoldComplete[
				boxesToTeXProcessor[testData],
				{testData, {"BoxRules" -> testDefaultBoxRules}},
				{"Boxes", "BoxRules"}
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
	]
]


withMockedFunctions@Module[{testBoxDataContents},
	testLookedupBoxes = BoxData[testBoxDataContents];
	
	Test[
		boxesToTeXProcessor[testData]
		,
		{"TeXCode" -> testBoxesToStringResult, testData}
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
	]
]


withMockedFunctions@Module[{testBoxDataContents, testCellstyle},
	testLookedupBoxes = Cell[BoxData[testBoxDataContents], testCellstyle];
	
	Test[
		boxesToTeXProcessor[testData]
		,
		{"TeXCode" -> testBoxesToStringResult, testData}
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
	]
]


withMockedFunctions[
	$commandCharsToTeX = {"$" -> "test1", ":" -> "test2", ";" -> "test3"};
	testBoxesToStringResult = "\
$($alpha$)$($beta$) $($gamma$)\t$($delta$) \t\t  \t$($epsilon$)
$($zeta$)\[IndentingNewLine]$($eta$)\\(a\\)\\(b\\)";
	
	Test[
		boxesToTeXProcessor[testData]
		,
		{
			"TeXCode" ->"\
$($alpha$beta $gamma\t$delta \t\t  \t$epsilon$)
$($zeta$)\[IndentingNewLine]$($eta$)\\(a\\)\\(b\\)"
			,
			testData
		}
		,
		TestID -> "adjacent math modes: returned value"
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
