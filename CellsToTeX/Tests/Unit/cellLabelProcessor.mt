(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`cellLabelProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[withMockedFunctions, HoldAll]

withMockedFunctions[
	{
		lookedupIndexed_, lookedupIntype_, lookedupPreviousIntype_,
		lookedupCurrentCellIndex_
	},
	body_
] :=
	Module[{testOption, testValue},
		Block[
			{
				processorDataLookup, $processorDataLookupLog = {},
				testLookedupStyle, testLookedupTeXOption,
				testLookedupCellLabel, testLookedupCellIndex,
				testLookedupCellForm
				,
				CellsToTeX`Internal`labelToKeyVal, $labelToKeyValLog = {},
				testTeXOptionFromLabel, testCurrentCellIndexFromLabel
				,
				testDefaultIndexed, testDefaultIntype, testDefaultCellLabel,
				testDefaultCellIndex, testDefaultCellForm
				,
				testData
			},
			SetAttributes[processorDataLookup, HoldFirst];
			mockFunction[
				processorDataLookup,
				$processorDataLookupLog,
				{
					{testLookedupTeXOption}, lookedupCurrentCellIndex,
					lookedupPreviousIntype, lookedupIndexed,
					lookedupIntype, testLookedupCellLabel,
					testLookedupCellIndex, testLookedupCellForm
				}
			];
			mockFunction[
				CellsToTeX`Internal`labelToKeyVal,
				$labelToKeyValLog,
				{{testTeXOptionFromLabel}, testCurrentCellIndexFromLabel}
			];
			testDefaultOptions = {
				"Indexed" -> testDefaultIndexed,
				"Intype" -> testDefaultIntype,
				"CellLabel" -> testDefaultCellLabel,
				"CellIndex" -> testDefaultCellIndex,
				"CellForm" -> testDefaultCellForm
			};
			SetOptions[cellLabelProcessor, testDefaultOptions];
			testData = {testOption -> testValue};
			
			body
		]
	]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Indexed*)


withMockedFunctions[{True, True, True, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, True, True, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "True, True, True, Automatic: labelToKeyVal"
	];
	
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData, testDefaultOptions = testDefaultOptions},
			{HoldComplete[
				cellLabelProcessor[testData],
				{testData, testDefaultOptions},
				{
					"TeXOptions", "CurrentCellIndex", "PreviousIntype",
					"Indexed", "Intype", "CellLabel", "CellIndex", "CellForm"
				}
			]}
		]
		,
		TestID -> "passing data to processorDataLookup"
	]
]
withMockedFunctions[{True, True, True, 5},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, True, True, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 6
		]}
		,
		TestID -> "True, True, True, Integer: labelToKeyVal"
	]
]

withMockedFunctions[{True, True, False, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, True, False, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "True, True, False, Automatic: labelToKeyVal"
	]
]
withMockedFunctions[{True, True, False, 11},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, True, False, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 12
		]}
		,
		TestID -> "True, True, False, Integer: labelToKeyVal"
	]
]


withMockedFunctions[{True, False, True, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, False, True, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "True, False, True, Automatic: labelToKeyVal"
	]
]
withMockedFunctions[{True, False, True, 39},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, False, True, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 39
		]}
		,
		TestID -> "True, False, True, Integer: labelToKeyVal"
	]
]

withMockedFunctions[{True, False, False, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, False, False, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "True, False, False, Automatic: labelToKeyVal"
	]
]
withMockedFunctions[{True, False, False, 90},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> True,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "True, False, False, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 91
		]}
		,
		TestID -> "True, False, False, Integer: labelToKeyVal"
	]
]


(* ::Subsection:: *)
(*Non-Indexed*)


withMockedFunctions[{False, True, True, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, True, True, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "False, True, True, Automatic: labelToKeyVal"
	]
]
withMockedFunctions[{False, True, True, 5},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, True, True, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 5
		]}
		,
		TestID -> "False, True, True, Integer: labelToKeyVal"
	]
]

withMockedFunctions[{False, True, False, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, True, False, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "False, True, False, Automatic: labelToKeyVal"
	]
]
withMockedFunctions[{False, True, False, 11},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> True,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, True, False, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 11
		]}
		,
		TestID -> "False, True, False, Integer: labelToKeyVal"
	]
]


withMockedFunctions[{False, False, True, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, False, True, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "False, False, True, Automatic: labelToKeyVal"
	]
]
withMockedFunctions[{False, False, True, 39},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, False, True, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 39
		]}
		,
		TestID -> "False, False, True, Integer: labelToKeyVal"
	]
]

withMockedFunctions[{False, False, False, Automatic},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, False, False, Automatic: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, Automatic
		]}
		,
		TestID -> "False, False, False, Automatic: labelToKeyVal"
	]
]
withMockedFunctions[{False, False, False, 90},
	Test[
		testData // cellLabelProcessor
		,
		{
			"TeXOptions" -> {testTeXOptionFromLabel, testLookedupTeXOption},
			"Indexed" -> False,
			"Intype" -> False,
			"CellIndex" -> testCurrentCellIndexFromLabel,
			testData
		}
		,
		TestID -> "False, False, False, Integer: returned value"
	];
	Test[
		$labelToKeyValLog
		,
		{HoldComplete[
			testLookedupCellLabel, testLookedupCellIndex,
			testLookedupCellForm, 90
		]}
		,
		TestID -> "False, False, False, Integer: labelToKeyVal"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[cellLabelProcessor], Protected]
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
