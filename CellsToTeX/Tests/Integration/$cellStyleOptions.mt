(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`$cellStyleOptions`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


tmpExtractStyleOptions =
	CellsToTeX`Internal`extractStyleOptions[#, $cellStyleOptions]&


(* ::Section:: *)
(*Tests*)


Test[
	"Code" // tmpExtractStyleOptions
	,
	{
		"Processor" ->
			Composition[
				trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
				boxRulesProcessor, annotateSyntaxProcessor,
				toInputFormProcessor, cellLabelProcessor,
				extractCellOptionsProcessor
			],
		"BoxRules" :> $linearBoxesToTeX,
		"CharacterEncoding" -> "ASCII",
		"FormatType" -> InputForm,
		"Indexed" -> True,
		"Intype" -> True
	}
	,
	TestID -> "Code"
]


Test[
	"Input" // tmpExtractStyleOptions
	,
	{
		"Processor" ->
			Composition[
				trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
				boxRulesProcessor, annotateSyntaxProcessor, cellLabelProcessor,
				extractCellOptionsProcessor
			],
		"BoxRules" :>
			Join[
				$linearBoxesToTeX,
				$boxesToFormattedTeX,
				headRulesToBoxRules[$boxHeadsToTeXCommands]
			],
		"StringRules" :> Join[$stringsToTeX, $commandCharsToTeX],
		"NonASCIIHandler" -> (charToTeX[#, FontWeight -> Bold]&),
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> InputForm,
		"TeXCodeSimplifier" ->
			(CellsToTeX`Internal`mergeAdjacentTeXCommands[
				$commandCharsToTeX[[1, 1]] <> "pmb",
				$commandCharsToTeX[[2, 1]],
				$commandCharsToTeX[[3, 1]],
				CellsToTeX`Internal`mergeAdjacentTeXDelims[
					$commandCharsToTeX[[1, 1]] <> "(",
					$commandCharsToTeX[[1, 1]] <> ")",
					#
				]
			]&),
		"Indexed" -> True,
		"Intype" -> True
	}
	,
	TestID -> "Input"
]


Test[
	"Output" // tmpExtractStyleOptions
	,
	{
		"Processor" ->
			Composition[
				trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
				boxRulesProcessor, cellLabelProcessor,
				extractCellOptionsProcessor
			],
		"BoxRules" :>
			Join[
				$linearBoxesToTeX,
				$boxesToFormattedTeX,
				headRulesToBoxRules[$boxHeadsToTeXCommands]
			],
		"StringRules" :> Join[$stringsToTeX, $commandCharsToTeX],
		"NonASCIIHandler" -> (charToTeX[#, FontWeight -> Plain]&),
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> OutputForm,
		"TeXCodeSimplifier" ->
			(CellsToTeX`Internal`mergeAdjacentTeXDelims[
				$commandCharsToTeX[[1, 1]] <> "(",
				$commandCharsToTeX[[1, 1]] <> ")",
				#
			]&),
		"Indexed" -> True,
		"Intype" -> False
	}
	,
	TestID -> "Output"
]


Test[
	"Print" // tmpExtractStyleOptions
	,
	{
		"Processor" ->
			Composition[
				trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
				boxRulesProcessor, cellLabelProcessor,
				extractCellOptionsProcessor
			],
		"BoxRules" :>
			Join[
				$linearBoxesToTeX,
				$boxesToFormattedTeX,
				headRulesToBoxRules[$boxHeadsToTeXCommands]
			],
		"StringRules" :> Join[$stringsToTeX, $commandCharsToTeX],
		"NonASCIIHandler" -> (charToTeX[#, FontWeight -> Plain]&),
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> OutputForm,
		"TeXCodeSimplifier" ->
			(CellsToTeX`Internal`mergeAdjacentTeXDelims[
				$commandCharsToTeX[[1, 1]] <> "(",
				$commandCharsToTeX[[1, 1]] <> ")",
				#
			]&),
		"Indexed" -> False,
		"Intype" -> False,
		"CellLabel" -> None
	}
	,
	TestID -> "Print"
]


Test[
	"Message" // tmpExtractStyleOptions
	,
	{
		"Processor" ->
			Composition[
				trackCellIndexProcessor, mmaCellProcessor, boxesToTeXProcessor,
				boxRulesProcessor, messageLinkProcessor, cellLabelProcessor,
				extractCellOptionsProcessor
			],
		"BoxRules" :>
			Join[
				$linearBoxesToTeX,
				$boxesToFormattedTeX,
				headRulesToBoxRules[$boxHeadsToTeXCommands]
			],
		"StringRules" :> Join[$stringsToTeX, $commandCharsToTeX],
		"NonASCIIHandler" -> (charToTeX[#, FontWeight -> Plain]&),
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> OutputForm,
		"TeXCodeSimplifier" ->
			(CellsToTeX`Internal`mergeAdjacentTeXDelims[
				$commandCharsToTeX[[1, 1]] <> "(",
				$commandCharsToTeX[[1, 1]] <> ")",
				#
			]&),
		"Indexed" -> False,
		"Intype" -> False,
		"CellLabel" -> None
	}
	,
	TestID -> "Message"
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[$cellStyleOptions], Protected]
	,
	False
	,
	TestID -> "Protected attribute"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
