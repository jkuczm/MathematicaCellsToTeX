(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`mmaCellProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[moduleWithMockedFunctions, HoldAll]

moduleWithMockedFunctions[vars_List, lookedupBoxes_, body_] :=
	Module[vars,
		Block[
			{
				processorDataLookup, $processorDataLookupLog = {},
				testLookedupTeXOption, testLookedupBoxRuleLHS,
				testLookedupBoxRuleRHS, testLookedupNonASCIIHandler
				,
				CellsToTeX`Internal`optionsToTeX, $optionsToTeXLog = {}
				,
				CellsToTeX`Internal`boxesToString, $boxesToStringLog = {}
				,
				$basicBoxes, testBasicBoxes
				,
				$commandCharsToTeX = $commandCharsToTeX
				,
				testDefaultBoxRuleLHS, testDefaultBoxRuleRHS,
				testDefaultNonASCIIHandler
				,
				testData, testExtendedBoxRules,
				testBoxes, testStyle, testBox, testTeXOptions,
				testConvertedBox, testNonASCIIHandler, testIndentation,
				testFormatTypeValue, testCharacterEncodingValue,
				testPageHeightValue, testUnusedOptionName,
				testUnusedOptionValue
			}
			,
			SetAttributes[processorDataLookup, HoldFirst];
			mockFunction[
				processorDataLookup,
				$processorDataLookupLog,
				{
					lookedupBoxes, "testLookedupStyle",
					{testLookedupTeXOption},
					{testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS},
					{"testLookedupStringRuleLHS" ->
						"testLookedupStringRuleRHS"},
					testLookedupNonASCIIHandler,
					"testLookedupIndentation"
				}
			];
			mockFunction[
				CellsToTeX`Internal`optionsToTeX,
				$optionsToTeXLog
				,
				"[testOptionsToTeXResult" <>
				ToString @ Length[$optionsToTeXLog] <> "]"
			];
			mockFunction[
				CellsToTeX`Internal`boxesToString,
				$boxesToStringLog
				,
				"testBoxesToStringResult" <>
				ToString @ Length[$boxesToStringLog]
			];
			$basicBoxes = testBasicBoxes;
			
			SetOptions[mmaCellProcessor,
				"BoxRules" -> {testDefaultBoxRuleLHS -> testDefaultBoxRuleRHS},
				"StringRules" ->
					{"testDefaultStringRuleLHS" -> "testDefaultStringRuleRHS"},
				"NonASCIIHandler" -> testDefaultNonASCIIHandler,
				"Indentation" -> "testDefaultIndentation"
			];
			testData = {
				"Boxes" -> testBoxes,
				"Style" -> testStyle,
				"BoxRules" -> {testBox -> testConvertedBox},
				"StringRules" -> {"testString" -> "testConvertedString"},
				"NonASCIIHandler" -> testNonASCIIHandler,
				"TeXOptions" -> testTeXOptions,
				"Indentation" -> testIndentation,
				"FormatType" -> testFormatTypeValue,
				"CharacterEncoding" -> testCharacterEncodingValue,
				"PageHeight" -> testPageHeightValue,
				testUnusedOptionName -> testUnusedOptionValue
			};
			testExtendedBoxRules = {
				testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS,
				HoldPattern[
					Verbatim[Pattern][strPattName_, Verbatim[Blank][String]] :>
						StringReplace[makeStringDefault[strPattName_], {
							"testLookedupStringRuleLHS" ->
								"testLookedupStringRuleRHS"
							,
							Verbatim[Pattern][charPattName_,
								RegularExpression["[^[:ascii:]]"]
							] :>
								testLookedupNonASCIIHandler[charPattName_]
						}]
				],
				With[{testData = testData},
					HoldPattern[
						Verbatim[Pattern][
							unsupBoxPattName_, Verbatim[Except][testBasicBoxes]
						] :>
							CellsToTeX`Internal`throwException[
								mmaCellProcessor[testData],
								{"Unsupported", "Box"},
								{unsupBoxPattName_, {testLookedupBoxRuleLHS}}
							]
					]
				]
			};
			
			body
		]
	]


(* ::Section:: *)
(*Tests*)


moduleWithMockedFunctions[
	{testLookedupBoxes}
	,
	testLookedupBoxes
	,
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestBoxesToStringResult1
\\end{mmaCell}"
			,
			"BoxRules" -> testExtendedBoxRules,
			testData
		}
		,
		TestID -> "basic: returned value"
	];
	
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData},
			{HoldComplete[
				mmaCellProcessor[testData],
				{
					testData,
					{
						"BoxRules" ->
							{testDefaultBoxRuleLHS -> testDefaultBoxRuleRHS},
						"StringRules" ->
							{"testDefaultStringRuleLHS" ->
								"testDefaultStringRuleRHS"},
						"NonASCIIHandler" -> testDefaultNonASCIIHandler,
						"Indentation" -> "testDefaultIndentation"
					}
				},
				{
					"Boxes", "Style", "TeXOptions", "BoxRules", "StringRules",
					"NonASCIIHandler", "Indentation"
				}
			]}
		]
		,
		TestID -> "passing data to processorDataLookup"
	];
	TestMatch[
		$boxesToStringLog
		,
		{HoldComplete @@ {
			testLookedupBoxes,
			testExtendedBoxRules,
			{
				"FormatType" -> testFormatTypeValue,
				"CharacterEncoding" -> testCharacterEncodingValue,
				"PageHeight" -> testPageHeightValue
			}
		}}
		,
		TestID -> "basic: boxesToString"
	];
	Test[
		$optionsToTeXLog
		,
		{HoldComplete["[", {testLookedupTeXOption}, "]"]}
		,
		TestID -> "basic: optionsToTeX"
	]
]


moduleWithMockedFunctions[
	{testLookedupBoxes1, testLookedupBoxes2}
	,
	{testLookedupBoxes1, testLookedupBoxes2}
	,
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestBoxesToStringResult1
\\end{mmaCell}"
			,
			"BoxRules" -> testExtendedBoxRules,
			testData
		}
		,
		TestID -> "list of boxes: returned value"
	];
	
	TestMatch[
		$boxesToStringLog
		,
		{
			HoldComplete @@ {
				{testLookedupBoxes1, testLookedupBoxes2},
				testExtendedBoxRules,
				{
					"FormatType" -> testFormatTypeValue,
					"CharacterEncoding" -> testCharacterEncodingValue,
					"PageHeight" -> testPageHeightValue
				}
			}
		}
		,
		TestID -> "list of boxes: boxesToString"
	];
	Test[
		$optionsToTeXLog
		,
		{HoldComplete["[", {testLookedupTeXOption}, "]"]}
		,
		TestID -> "list of boxes: optionsToTeX"
	]
]


moduleWithMockedFunctions[
	{testLookedupBoxes}
	,
	BoxData[testLookedupBoxes]
	,
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestBoxesToStringResult1
\\end{mmaCell}"
			,
			"BoxRules" -> testExtendedBoxRules,
			testData
		}
		,
		TestID -> "BoxData: returned value"
	];
	
	TestMatch[
		$boxesToStringLog
		,
		{HoldComplete @@ {
			testLookedupBoxes,
			testExtendedBoxRules,
			{
				"FormatType" -> testFormatTypeValue,
				"CharacterEncoding" -> testCharacterEncodingValue,
				"PageHeight" -> testPageHeightValue
			}
		}}
		,
		TestID -> "BoxData: boxesToString"
	];
	Test[
		$optionsToTeXLog
		,
		{HoldComplete["[", {testLookedupTeXOption}, "]"]}
		,
		TestID -> "BoxData: optionsToTeX"
	]
]


moduleWithMockedFunctions[
	{testLookedupBoxes}
	,
	Cell[BoxData[testLookedupBoxes], testCellstyle]
	,
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestBoxesToStringResult1
\\end{mmaCell}"
			,
			"BoxRules" -> testExtendedBoxRules,
			testData
		}
		,
		TestID -> "Cell: returned value"
	];
	
	TestMatch[
		$boxesToStringLog
		,
		{HoldComplete @@ {
			testLookedupBoxes,
			testExtendedBoxRules,
			{
				"FormatType" -> testFormatTypeValue,
				"CharacterEncoding" -> testCharacterEncodingValue,
				"PageHeight" -> testPageHeightValue
			}
		}}
		,
		TestID -> "Cell: boxesToString"
	];
	Test[
		$optionsToTeXLog
		,
		{HoldComplete["[", {testLookedupTeXOption}, "]"]}
		,
		TestID -> "Cell: optionsToTeX"
	]
]


moduleWithMockedFunctions[
	{testLookedupBoxes}
	,
	testLookedupBoxes
	,
	mockFunction[
		processorDataLookup,
		$processorDataLookupLog,
		{
			lookedupBoxes, "testLookedupStyle",
			{testLookedupTeXOption},
			{testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS},
			{} (* Empty StringRules. *),
			testLookedupNonASCIIHandler,
			"testLookedupIndentation"
		}
	];
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestBoxesToStringResult1
\\end{mmaCell}"
			,
			(*	For empty StringRules and non-Identity NonASCIIHandler only
				autimatic non-ASCII string rule is created. *)
			"BoxRules" -> Delete[testExtendedBoxRules, {2, 1, 2, 2, 1}],
			testData
		}
		,
		TestID -> "empty StringRules: returned value"
	]
]
moduleWithMockedFunctions[
	{testLookedupBoxes}
	,
	testLookedupBoxes
	,
	mockFunction[
		processorDataLookup,
		$processorDataLookupLog,
		{
			lookedupBoxes, "testLookedupStyle",
			{testLookedupTeXOption},
			{testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS},
			{"testLookedupStringRuleLHS" -> "testLookedupStringRuleRHS"},
			Identity (* Identity NonASCIIHandler. *),
			"testLookedupIndentation"
		}
	];
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestBoxesToStringResult1
\\end{mmaCell}"
			,
			(*	For Identity NonASCIIHandler no autimatic non-ASCII string rule
				is created. *)
			"BoxRules" -> Delete[testExtendedBoxRules, {2, 1, 2, 2, 2}],
			testData
		}
		,
		TestID -> "Identity NonASCIIHandler: returned value"
	]
]
moduleWithMockedFunctions[
	{testLookedupBoxes}
	,
	testLookedupBoxes
	,
	mockFunction[
		processorDataLookup,
		$processorDataLookupLog,
		{
			lookedupBoxes, "testLookedupStyle",
			{testLookedupTeXOption},
			{testLookedupBoxRuleLHS -> testLookedupBoxRuleRHS},
			{} (* Empty StringRules. *),
			Identity (* Identity NonASCIIHandler. *),
			"testLookedupIndentation"
		}
	];
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestBoxesToStringResult1
\\end{mmaCell}"
			,
			(*	For empty StringRules and Identity NonASCIIHandler no string
				related box rule is created. *)
			"BoxRules" -> Delete[testExtendedBoxRules, 2],
			testData
		}
		,
		TestID -> "empty StringRules, Identity NonASCIIHandler: returned value"
	]
]


moduleWithMockedFunctions[
	{testLookedupBoxes}
	,
	testLookedupBoxes
	,
	$commandCharsToTeX = {"$" -> "test1", ":" -> "test2", ";" -> "test3"};
	mockFunction[
		CellsToTeX`Internal`boxesToString,
		$boxesToStringLog,
		"$($alpha$) \t$($beta$)\[IndentingNewLine] $($gamma$)\\(a\\)\\(b\\)"
	];
	
	TestMatch[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentation$($alpha \t$beta
testLookedupIndentation $gamma$)\\(a\\)\\(b\\)
\\end{mmaCell}"
			,
			(* For empty StringRules no string related box rule is created. *)
			"BoxRules" -> testExtendedBoxRules,
			testData
		}
		,
		TestID -> "adjacent math modes: returned value"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[mmaCellProcessor], Protected]
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
