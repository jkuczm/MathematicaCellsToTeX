(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`$linearBoxesToTeX`", {"MUnit`"}]


Get["CellsToTeX`"]


tmpBoxesToString =
	CellsToTeX`Internal`boxesToString[
		#,
		CellsToTeX`Configuration`$linearBoxesToTeX,
		FormatType -> InputForm
	]&


(* ::Section:: *)
(*Tests*)


Test[
	RowBox[{"a", RowBox[{"b", "c"}]}] // tmpBoxesToString
	,
	"abc"
	,
	TestID -> "RowBox"
]

Test[
	StyleBox[StyleBox["d", Red], Green] // tmpBoxesToString
	,
	"d"
	,
	TestID -> "StyleBox"
]

Test[
	ButtonBox[ButtonBox["e", opt1 -> val1], opt2 -> val2] // tmpBoxesToString
	,
	"e"
	,
	TestID -> "ButtonBox"
]

Test[
	InterpretationBox[InterpretationBox["f", "g"], "h"] // tmpBoxesToString
	,
	"f"
	,
	TestID -> "InterpretationBox"
]

Test[
	FormBox[FormBox["i", StandardForm], StandardForm] // tmpBoxesToString
	,
	"i"
	,
	TestID -> "FormBox"
]

Test[
	TagBox[TagBox["j", StandardForm], TraditionalForm] // tmpBoxesToString
	,
	"j"
	,
	TestID -> "TagBox"
]

Test[
	TemplateBox[{"k", "l"}, tag1,
		DisplayFunction -> (
			TemplateBox[{#1, "m", #2}, tag2,
				DisplayFunction -> (RowBox[{#3, "+", #2, "-", #1}]&)
			]&
		)
	] // tmpBoxesToString
	,
	"l+m-k"
	,
	TestID -> "TemplateBox"
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[CellsToTeX`Configuration`$linearBoxesToTeX], Protected]
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
