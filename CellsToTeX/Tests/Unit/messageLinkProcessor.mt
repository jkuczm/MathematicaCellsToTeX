(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`messageLinkProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[moduleWithMockedFunctions, HoldAll]

moduleWithMockedFunctions[vars_List, extractMessageLinkResult_, body_] :=
	Module[vars,
		Block[
			{
				processorDataLookup, $processorDataLookupLog = {},
				testLookedupBoxes, testLookedupTeXOption
				,
				CellsToTeX`Internal`extractMessageLink,
				$extractMessageLinkLog = {}
				,
				testData, testBoxes, testTeXOptions, testUnusedOption,
				testUnusedValue
			},
			SetAttributes[processorDataLookup, HoldFirst];
			mockFunction[
				processorDataLookup,
				$processorDataLookupLog,
				{testLookedupBoxes, {testLookedupTeXOption}}
			];
			mockFunction[
				CellsToTeX`Internal`extractMessageLink,
				$extractMessageLinkLog,
				extractMessageLinkResult
			];
			testData = {
				"Boxes" -> testBoxes,
				"TeXOptions" -> testTeXOptions,
				testUnusedOption -> testUnusedValue
			};
			
			body
		]
	]


(* ::Section:: *)
(*Tests*)


moduleWithMockedFunctions[
	{}
	,
	Missing["NotFound"]
	,
	
	Test[
		testData // messageLinkProcessor
		,
		testData
		,
		TestID -> "no link: returned value"
	];
	
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData},
			{HoldComplete[
				messageLinkProcessor[testData],
				testData,
				{"Boxes", "TeXOptions"}
			]}
		]
		,
		TestID -> "no link: processorDataLookup"
	];
	Test[
		$extractMessageLinkLog
		,
		{HoldComplete[testLookedupBoxes]}
		,
		TestID -> "no link: extractMessageLink"
	]
]


moduleWithMockedFunctions[
	{testMessageLinkResult}
	,
	testMessageLinkResult
	,
	
	Test[
		testData // messageLinkProcessor
		,
		{
			"TeXOptions" -> {
				testLookedupTeXOption,
				"messagelink" -> testMessageLinkResult
			},
			testData
		}
		,
		TestID -> "with link: returned value"
	];
	
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData},
			{HoldComplete[
				messageLinkProcessor[testData],
				testData,
				{"Boxes", "TeXOptions"}
			]}
		]
		,
		TestID -> "with link: processorDataLookup"
	];
	Test[
		$extractMessageLinkLog
		,
		{HoldComplete[testLookedupBoxes]}
		,
		TestID -> "with link: extractMessageLink"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[messageLinkProcessor], Protected]
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
