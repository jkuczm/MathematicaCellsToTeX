(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`CellsToTeXException`", {"MUnit`"}]


Get["CellsToTeX`"]


(* ::Section:: *)
(*Tests*)


Test[
	ToString @ StringForm[CellsToTeXException::unsupported,
		HoldForm @ func[arg],
		HoldForm @ CellsToTeXException["Unsupported", "Box"],
		HoldForm @ "Box",
		HoldForm @ SuperscriptBox["a", "b"],
		HoldForm @ {RowBox[_List], StyleBox[_, ___]}
	]
	,
	"Box: SuperscriptBox[a, b] is not one of supported: \
{RowBox[_List], StyleBox[_, ___]}. Exception occurred in func[arg]."
	,
	TestID -> "unsupported"
]


Test[
	ToString @ StringForm[CellsToTeXException::failed,
		HoldForm @ exportProcessor[data],
		HoldForm @ CellsToTeXException["Failed", "Export"],
		HoldForm @ "Export",
		HoldForm @ Export["tmp.pdf", "something", "wrong"]
	]
	,
	"Evaluation of following Export expression failed: \
Export[tmp.pdf, something, wrong]. Exception occurred in \
exportProcessor[data]."
	,
	TestID -> "failed"
]


Test[
	ToString @ StringForm[CellsToTeXException::invalid,
		HoldForm @ func[arg],
		HoldForm @ CellsToTeXException["Invalid", "Boxes"],
		HoldForm @ "Boxes",
		HoldForm @ RowBox[{"f", "["}]
	]
	,
	"Following elements of type Boxes are invalid: RowBox[{f, [}]. \
Exception occurred in func[arg]."
	,
	TestID -> "invalid"
]


Test[
	ToString @ StringForm[CellsToTeXException::missing,
		HoldForm @ func[arg],
		HoldForm @ CellsToTeXException["Missing", "Keys"],
		HoldForm @ "Keys",
		HoldForm @ {"Boxes", "Style"},
		HoldForm @ {"BoxRules", "AnnotationTypesToTeX"}
	]
	,
	"Following elements of type Keys are missing: {Boxes, Style}. \
Available elements of type Keys are: {BoxRules, AnnotationTypesToTeX}. \
Exception occurred in func[arg]."
	,
	TestID -> "missing"
]


Test[
	ToString @ StringForm[CellsToTeXException::missingCellStyle,
		HoldForm @ CellToTeX[Cell[boxes]],
		HoldForm @ CellsToTeXException["Missing", "CellStyle"],
		HoldForm @ Cell[boxes]
	]
	,
	"Couldn't extract cell style from given boxes. Either use \"Style\" \
option or provide Cell expression with defined style. Given boxes are: \
Cell[boxes]. Exception occurred in CellToTeX[Cell[boxes]]."
	,
	TestID -> "missingCellStyle"
]


Test[
	ToString @ StringForm[CellsToTeXException::missingProcArg,
		HoldForm @ processor[data],
		HoldForm @ CellsToTeXException["Missing", "Keys", "ProcessorArgument"],
		HoldForm @ "Keys",
		HoldForm @ {"FileName", "TeXOptions"},
		HoldForm @ {"Style", "BoxRules"}
	]
	,
	"Processor didn't receive required data with following keys: \
{FileName, TeXOptions}. Passed data keys are: {Style, BoxRules}. \
Exception occurred in processor[data]."
	,
	TestID -> "missingProcArg"
]


Test[
	ToString @ StringForm[CellsToTeXException::missingProcRes,
		HoldForm @ CellToTeX[Cell[boxes]],
		HoldForm @ CellsToTeXException["Missing", "Keys", "ProcessorResult"],
		HoldForm @ "Keys",
		HoldForm @ {"TeXCode"},
		HoldForm @ {"TeXOptions", "BoxRules"},
		HoldForm @ Composition[cellLabelProcessor, extractCellOptionsProcessor]
	]
	,
	"Required keys: {TeXCode} are not present in data returned by processor: "
	<> If[$VersionNumber < 10,
		"Composition[cellLabelProcessor, extractCellOptionsProcessor]"
	(* else *),
		"cellLabelProcessor @* extractCellOptionsProcessor"
	] <>". Data contained only {TeXOptions, BoxRules} keys. \
Exception occurred in CellToTeX[Cell[boxes]]."
	,
	TestID -> "missingProcRes"
]


Test[
	ToString @ StringForm[CellsToTeXException::error,
		HoldForm @ func[arg],
		HoldForm @ CellsToTeXException["SomeInternalError"]
	]
	,
	"An internal error occurred. func[arg] expression caused \
CellsToTeXException[SomeInternalError]. Please inform the package maintainer \
about this problem."
	,
	TestID -> "internal"
]


Test[
	ToString @ StringForm[CellsToTeXException::unknownError,
		HoldForm @ val,
		HoldForm @ CellsToTeXException["UnknownInternalError"]
	]
	,
	"An internal error occurred. val was thrown tagged with \
CellsToTeXException[UnknownInternalError]. Please inform the package \
maintainer about this problem."
	,
	TestID -> "unknownError"
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[CellsToTeXException], Protected]
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
