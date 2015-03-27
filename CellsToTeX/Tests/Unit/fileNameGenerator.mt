(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`fileNameGenerator`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetAttributes[withMockedFunctions, HoldAll]

withMockedFunctions[body_] :=
	Block[
		{
			CellsToTeX`Internal`formatToExtension, $formatToExtensionLog = {}
			,
			testBoxes
		},
		mockFunction[
			CellsToTeX`Internal`formatToExtension, $formatToExtensionLog
			,
			"testFormatToExtensionResult" <>
			ToString @ Length[$formatToExtensionLog]
		];
		
		body
	]


(* ::Section:: *)
(*Tests*)


withMockedFunctions[
	Test[
		fileNameGenerator[testBoxes, "testExportFormatBasic"]
		,
		"81ed9375testFormatToExtensionResult1"
		,
		TestID -> "basic: returned value"
	];
	Test[
		$formatToExtensionLog
		,
		{HoldComplete["testExportFormatBasic"]}
		,
		TestID -> "basic: formatToExtension"
	]
]


withMockedFunctions[
	Test[
		fileNameGenerator[Cell[testBoxes], "testExportFormatCell"]
		,
		"c963960ftestFormatToExtensionResult1"
		,
		TestID -> "Cell: returned value"
	];
	Test[
		$formatToExtensionLog
		,
		{HoldComplete["testExportFormatCell"]}
		,
		TestID -> "Cell: formatToExtension"
	]
]
withMockedFunctions[
	Test[
		fileNameGenerator[
			Cell[testBoxes, CellLabel -> "testCellLabel"],
			"testExportFormatCellCellLabel"
		]
		,
		"c963960ftestFormatToExtensionResult1"
		,
		TestID -> "Cell with CellLabel: returned value"
	];
	Test[
		$formatToExtensionLog
		,
		{HoldComplete["testExportFormatCellCellLabel"]}
		,
		TestID -> "Cell with CellLabel: formatToExtension"
	]
]
withMockedFunctions[
	Test[
		fileNameGenerator[
			Cell[testBoxes, CellChangeTimes -> {3.636438661845006*^9}],
			"testExportFormatCellCellChangeTimes"
		]
		,
		"c963960ftestFormatToExtensionResult1"
		,
		TestID -> "Cell with CellChangeTimes: returned value"
	];
	Test[
		$formatToExtensionLog
		,
		{HoldComplete["testExportFormatCellCellChangeTimes"]}
		,
		TestID -> "Cell with CellChangeTimes: formatToExtension"
	]
]


withMockedFunctions[
	Test[
		fileNameGenerator[
			Cell[testBoxes, customOption -> customValue],
			"testExportFormatCellCustomOption"
		]
		,
		"60c391abtestFormatToExtensionResult1"
		,
		TestID -> "Cell with customOption: returned value"
	];
	Test[
		$formatToExtensionLog
		,
		{HoldComplete["testExportFormatCellCustomOption"]}
		,
		TestID -> "Cell with customOption: formatToExtension"
	]
]
withMockedFunctions[
	Test[
		fileNameGenerator[
			Cell[testBoxes, customOption -> customValue],
			"testExportFormatCellCustomOptionFiltered",
			"CellOptionsFilter" -> Except[customOption]
		]
		,
		"c963960ftestFormatToExtensionResult1"
		,
		TestID -> "Cell with customOption filtered: returned value"
	];
	Test[
		$formatToExtensionLog
		,
		{HoldComplete["testExportFormatCellCustomOptionFiltered"]}
		,
		TestID -> "Cell with customOption filtered: formatToExtension"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[fileNameGenerator], Protected]
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
