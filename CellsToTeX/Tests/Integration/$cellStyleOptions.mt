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
				trackCellIndexProcessor, mmaCellProcessor,
				annotateSyntaxProcessor, toInputFormProcessor,
				cellLabelProcessor, extractCellOptionsProcessor
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
				trackCellIndexProcessor, mmaCellProcessor,
				annotateSyntaxProcessor, cellLabelProcessor,
				extractCellOptionsProcessor
			],
		"BoxRules" :> getBoxesToFormattedTeX[],
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> InputForm,
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
				trackCellIndexProcessor, mmaCellProcessor,
				cellLabelProcessor, extractCellOptionsProcessor
			],
		"BoxRules" :> getBoxesToFormattedTeX[],
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> OutputForm,
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
				trackCellIndexProcessor, mmaCellProcessor,
				cellLabelProcessor, extractCellOptionsProcessor
			],
		"BoxRules" :> getBoxesToFormattedTeX[],
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> OutputForm,
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
				trackCellIndexProcessor, mmaCellProcessor,
				messageLinkProcessor, cellLabelProcessor,
				extractCellOptionsProcessor
			],
		"BoxRules" :> getBoxesToFormattedTeX[],
		"CharacterEncoding" -> "Unicode",
		"FormatType" -> OutputForm,
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
