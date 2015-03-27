(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`trackCellIndexProcessor`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[withMockedFunctions, HoldAll]

withMockedFunctions[testLookedupIndexed_, body_] :=
	Module[
		{
			testIndexed, testCellIndex, testIntype, testUnusedOption,
			testUnusedValue
		},
		Block[
			{
				processorDataLookup, $processorDataLookupLog = {},
				testLookedupCellIndex, testLookedupIntype
				,
				CellToTeX,
				testInitialCurrentCellIndex, testInitialPreviousIntype
				,
				testData
			},
			SetAttributes[processorDataLookup, HoldFirst];
			mockFunction[
				processorDataLookup,
				$processorDataLookupLog,
				{
					testLookedupIndexed, testLookedupCellIndex,
					testLookedupIntype
				}
			];
			Options[CellToTeX] = {
				"CurrentCellIndex" -> testInitialCurrentCellIndex,
				"PreviousIntype" -> testInitialPreviousIntype
			};
			testData = {
				"Indexed" -> testIndexed,
				"CellIndex" -> testCellIndex,
				"Intype" -> testIntype,
				testUnusedOption -> testUnusedValue
			};
			
			body
		]
	]


(* ::Section:: *)
(*Tests*)


withMockedFunctions[True,
	
	Test[
		testData // trackCellIndexProcessor
		,
		testData
		,
		TestID -> "indexed: returned value"
	];
	
	Test[
		$processorDataLookupLog
		,
		With[{testData = testData},
			{HoldComplete[
				trackCellIndexProcessor[testData],
				testData,
				{"Indexed", "CellIndex", "Intype"}
			]}
		]
		,
		TestID -> "passing data to processorDataLookup"
	];
	Test[
		OptionValue[CellToTeX, "CurrentCellIndex"]
		,
		testLookedupCellIndex
		,
		TestID -> "indexed: CurrentCellIndex"
	];
	Test[
		OptionValue[CellToTeX, "PreviousIntype"]
		,
		testLookedupIntype
		,
		TestID -> "indexed: PreviousIntype"
	]
]


withMockedFunctions[False,
	
	Test[
		testData // trackCellIndexProcessor
		,
		testData
		,
		TestID -> "non-indexed: returned value"
	];
	
	Test[
		OptionValue[CellToTeX, "CurrentCellIndex"]
		,
		testInitialCurrentCellIndex
		,
		TestID -> "non-indexed: CurrentCellIndex"
	];
	Test[
		OptionValue[CellToTeX, "PreviousIntype"]
		,
		testInitialPreviousIntype
		,
		TestID -> "non-indexed: PreviousIntype"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[trackCellIndexProcessor], Protected]
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
