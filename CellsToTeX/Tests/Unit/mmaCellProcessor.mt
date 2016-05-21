(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`mmaCellProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[withMockedFunctions, HoldFirst]

withMockedFunctions[body_] :=
	Block[
		{
			processorDataLookup, $processorDataLookupLog = {},
			testLookedupTeXCode, testLookedupTeXOptions,
			testLookedupIndentation
			,
			CellsToTeX`Internal`optionsToTeX, $optionsToTeXLog = {}
			,
			testDefaultIndentation
			,
			testData,
			testTeXCode, testStyle, testTeXOptions, testIndentation,
			testUnusedOptionName, testUnusedOptionValue
		}
		,
		SetAttributes[processorDataLookup, HoldFirst];
		mockFunction[
			processorDataLookup,
			$processorDataLookupLog,
			{
				testLookedupTeXCode, "testLookedupStyle",
				testLookedupTeXOptions, "testLookedupIndentation"
			}
		];
		mockFunction[
			CellsToTeX`Internal`optionsToTeX,
			$optionsToTeXLog
			,
			"[testOptionsToTeXResult" <>
				ToString @ Length[$optionsToTeXLog] <> "]"
		];
		
		SetOptions[mmaCellProcessor, "Indentation" -> testDefaultIndentation];
		
		testData = {
			"TeXCode" -> testTeXCode,
			"Style" -> testStyle,
			"TeXOptions" -> testTeXOptions,
			"Indentation" -> testIndentation,
			testUnusedOptionName -> testUnusedOptionValue
		};
			
		body
	]


(* ::Section:: *)
(*Tests*)


withMockedFunctions[
	testLookedupTeXCode = "testLookedupTeXCode";
	
	Test[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestLookedupTeXCode
\\end{mmaCell}"
			,
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
				{testData, {"Indentation" -> testDefaultIndentation}},
				{"TeXCode", "Style", "TeXOptions", "Indentation"}
			]}
		]
		,
		TestID -> "passing data to processorDataLookup"
	];
	Test[
		$optionsToTeXLog
		,
		{HoldComplete["[", testLookedupTeXOptions, "]"]}
		,
		TestID -> "basic: optionsToTeX"
	]
]


withMockedFunctions[
	testLookedupTeXCode = "\
testLookedupTeXCodeLine1
testLookedupTeXCodeLine2\[IndentingNewLine]testLookedupTeXCodeLine3";
	
	Test[
		mmaCellProcessor[testData]
		,
		{
			"TeXCode" -> "\
\\begin{mmaCell}[testOptionsToTeXResult1]{testLookedupStyle}
testLookedupIndentationtestLookedupTeXCodeLine1
testLookedupIndentationtestLookedupTeXCodeLine2
testLookedupIndentationtestLookedupTeXCodeLine3
\\end{mmaCell}"
			,
			testData
		}
		,
		TestID -> "multiple lines: returned value"
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
