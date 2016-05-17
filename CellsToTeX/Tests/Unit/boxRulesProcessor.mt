(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`boxRulesProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[withMockedFunctions, HoldFirst]

withMockedFunctions[body_] :=
	Block[
		{
			processorDataLookup, $processorDataLookupLog = {},
			testLookedupBoxRule, testLookedupStringRules,
			testLookedupStringRule, testLookedupNonASCIIHandler
			,
			testDefaultOptions,
			testDefaultBoxRule, testDefaultStringRule,
			testDefaultNonASCIIHandler
			,
			testData,
			testBoxRule, testStringRule, testNonASCIIHandler,
			testUnusedOptionName, testUnusedOptionValue
		}
		,
		testLookedupStringRules = {testLookedupStringRule};
		SetAttributes[processorDataLookup, HoldFirst];
		mockFunction[
			processorDataLookup,
			$processorDataLookupLog,
			{
				{testLookedupBoxRule}, testLookedupStringRules,
				testLookedupNonASCIIHandler
			}
		];
		
		testDefaultOptions = {
			"BoxRules" -> {testDefaultBoxRule},
			"StringRules" -> {testDefaultStringRule},
			"NonASCIIHandler" -> testDefaultNonASCIIHandler
		};
		SetOptions[boxRulesProcessor, testDefaultOptions];
		
		testData = {
			"BoxRules" -> {testBoxRule},
			"StringRules" -> {testStringRule},
			"NonASCIIHandler" -> testNonASCIIHandler,
			testUnusedOptionName -> testUnusedOptionValue
		};
		
		body
	]


(* ::Section:: *)
(*Tests*)


withMockedFunctions@With[
	{
		newStringRules = {
			testLookedupStringRule,
			HoldPattern[
				Verbatim[Pattern][
					charPattName_, RegularExpression["[^\\x00-\\x7F]"]
				] :>
					testLookedupNonASCIIHandler[charPattName_]
			]
		}
	}
	,
	TestMatch[
		boxRulesProcessor[testData]
		,
		{
			"BoxRules" -> {
				testLookedupBoxRule,
				HoldPattern[
					Verbatim[Pattern][strPattName_, Verbatim[Blank][String]] :>
						StringReplace[
							makeStringDefault[strPattName_], newStringRules
						]
				]
			},
			"StringRules" -> newStringRules,
			testData
		}
		,
		TestID -> "basic"
	];
	
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData, testDefaultOptions = testDefaultOptions},
			{HoldComplete[
				boxRulesProcessor[testData],
				{testData, testDefaultOptions},
				{"BoxRules", "StringRules", "NonASCIIHandler"}
			]}
		]
		,
		TestID -> "passing data to processorDataLookup"
	]
]


withMockedFunctions@With[
	{
		newStringRules = {
			HoldPattern[
				Verbatim[Pattern][
					charPattName_, RegularExpression["[^\\x00-\\x7F]"]
				] :>
					testLookedupNonASCIIHandler[charPattName_]
			]
		}
	}
	,
	testLookedupStringRules = {};
	
	TestMatch[
		boxRulesProcessor[testData]
		,
		{
			"BoxRules" -> {
				testLookedupBoxRule,
				HoldPattern[
					Verbatim[Pattern][strPattName_, Verbatim[Blank][String]] :>
						StringReplace[
							makeStringDefault[strPattName_], newStringRules
						]
				]
			},
			"StringRules" -> newStringRules,
			testData
		}
		,
		TestID -> "empty StringRules"
	]
]
withMockedFunctions[
	testLookedupNonASCIIHandler = Identity;
	
	TestMatch[
		boxRulesProcessor[testData]
		,
		{
			"BoxRules" -> {
				testLookedupBoxRule,
				HoldPattern[
					Verbatim[Pattern][strPattName_, Verbatim[Blank][String]] :>
						StringReplace[
							makeStringDefault[strPattName_],
							{testLookedupStringRule}
						]
				]
			},
			"StringRules" -> {testLookedupStringRule},
			testData
		}
		,
		TestID -> "Identity NonASCIIHandler"
	]
]
withMockedFunctions[
	testLookedupStringRules = {};
	testLookedupNonASCIIHandler = Identity;
	
	TestMatch[
		boxRulesProcessor[testData]
		,
		{
			"BoxRules" -> {testLookedupBoxRule},
			"StringRules" -> {},
			testData
		}
		,
		TestID -> "empty StringRules, Identity NonASCIIHandler"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[boxRulesProcessor], Protected]
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
