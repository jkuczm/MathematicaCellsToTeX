(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`boxesToString`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Internal`", "CellsToTeX`Backports`"}, $ContextPath]


$testBoxRules = {
	SubscriptBox[a_, b_] :>
		"\\sb{" <> CellsToTeX`Configuration`makeString[a] <> "}{" <>
		CellsToTeX`Configuration`makeString[b] <> "}"
}


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*OutputForm*)


tmpBoxesToString = boxesToString[##, FormatType -> OutputForm]&


Test[
	tmpBoxesToString["a", {}]
	,
	"a"
	,
	TestID -> "OutputForm: symbol"
]


Test[
	tmpBoxesToString[RowBox[{"a"}], {}]
	,
	"a"
	,
	TestID -> "OutputForm: symbol in RowBox"
]

Test[
	tmpBoxesToString[RowBox[{"a", "b"}], {}]
	,
	"ab"
	,
	TestID -> "OutputForm: two symbols"
]

Test[
	tmpBoxesToString[RowBox[{"a", " ", "b"}], {}]
	,
	"a b"
	,
	TestID -> "OutputForm: two symbols, space separated"
]

Test[
	tmpBoxesToString[RowBox[{"a", "(", RowBox[{"b", "+", "c"}]}], {}]
	,
	"a (b + c"
	,
	TestID -> "OutputForm: unbalanced bracket"
]

Test[
	tmpBoxesToString[RowBox[{"a", "(", RowBox[{"b", "+", "c"}], ")"}], {}]
	,
	"a(b + c)"
	,
	TestID -> "OutputForm: balanced bracket"
]

Test[
	tmpBoxesToString[RowBox[{"-", "b"}], {}]
	,
	"-b"
	,
	TestID -> "OutputForm: minus symbol"
]

Test[
	tmpBoxesToString[RowBox[{RowBox[{"-", "b"}]}], {}]
	,
	"-b"
	,
	TestID -> "OutputForm: minus symbol double RowBox"
]


Test[
	tmpBoxesToString["\"str\"", {}]
	,
	"str"
	,
	TestID -> "OutputForm: string"
]

Test[
	tmpBoxesToString[RowBox[{"\"str\""}], {}]
	,
	"str"
	,
	TestID -> "OutputForm: string in RowBox"
]

Test[
	tmpBoxesToString["\"\\t\"", {}]
	,
	"\t"
	,
	TestID -> "OutputForm: backslash"
]

Test[
	tmpBoxesToString["\"\\\\t\"", {}]
	,
	"\\t"
	,
	TestID -> "OutputForm: double backslash"
]

Test[
	tmpBoxesToString[RowBox[{"\"\\t\""}], {}]
	,
	"\t"
	,
	TestID -> "OutputForm: backslash in RowBox"
]

Test[
	tmpBoxesToString[RowBox[{"\"\\\\t\""}], {}]
	,
	"\\t"
	,
	TestID -> "OutputForm: double backslash in RowBox"
]


Test[
	tmpBoxesToString[RowBox[{"a", "\"str\""}], {}]
	,
	"astr"
	,
	TestID -> "OutputForm: symbol and string"
]


Test[
	tmpBoxesToString[SubscriptBox["a", "\"b\""], {}]
	,
	"\
a
 b"
	,
	TestID -> "OutputForm: formatting box"
]

Test[
	tmpBoxesToString["\"\\!\\(\\*SubscriptBox[a,\\\"b\\\"]\\)\"", {}]
	,
	"\
a
 b"
	,
	TestID -> "OutputForm: nested formatting box embedded in String"
]

Test[
	tmpBoxesToString[RowBox[{"\"d\"", SubscriptBox["a", "\"b\""], "c"}], {}]
	,
	"\
da c
  b"
	,
	TestID -> "OutputForm: formatting box in RowBox"
]

Test[
	tmpBoxesToString["\"d\!\(a\_b\)c\"", {}]
	,
	"\
da c
  b"
	,
	TestID -> "OutputForm: formatting box embedded in String"
]


Test[
	tmpBoxesToString[SubscriptBox["a", "\"b\""], $testBoxRules]
	,
	"\\sb{a}{b}"
	,
	TestID -> "OutputForm: BoxRules: formatting box"
]

Test[
	tmpBoxesToString[
		"\"\\!\\(\\*SubscriptBox[a,\\\"b\\\"]\\)\"",
		$testBoxRules
	]
	,
	"\\sb{a}{b}"
	,
	TestID -> "OutputForm: BoxRules: nested formatting box embedded in String"
]

Test[
	tmpBoxesToString[
		RowBox[{"\"d\"", SubscriptBox["a", "\"b\""], "c"}],
		$testBoxRules
	]
	,
	"d\\sb{a}{b}c"
	,
	TestID -> "OutputForm: BoxRules: formatting box in RowBox"
]

Test[
	tmpBoxesToString["\"d\!\(a\_b\)c\"", $testBoxRules]
	,
	"d\\sb{a}{b}c"
	,
	TestID -> "OutputForm: BoxRules: formatting box embedded in String"
]


Test[
	tmpBoxesToString["\n", {}]
	,
	"\n"
	,
	TestID -> "OutputForm: \\n"
]

Test[
	tmpBoxesToString["\[IndentingNewLine]", {}]
	,
	"\n"
	,
	TestID -> "OutputForm: \\[IndentingNewLine]"
]

Test[
	tmpBoxesToString["\[PlusMinus]", {}, CharacterEncoding -> "Unicode"]
	,
	"\[PlusMinus]"
	,
	TestID -> "OutputForm: Unicode: \\[PlusMinus]"
]
Test[
	tmpBoxesToString["\[PlusMinus]", {}, CharacterEncoding -> "ASCII"]
	,
	"+-"
	,
	TestID -> "OutputForm: ASCII: \\[PlusMinus]"
]
Test[
	tmpBoxesToString[
		"\[PlusMinus]",
		{"\[PlusMinus]" -> "\\(\\pm\\)"},
		CharacterEncoding -> "Unicode"
	]
	,
	"\\(\\pm\\)"
	,
	TestID -> "OutputForm: BoxRules: Unicode: \\[PlusMinus]"
]
Test[
	tmpBoxesToString[
		"\[PlusMinus]",
		{"\[PlusMinus]" -> "\\(\\pm\\)"},
		CharacterEncoding -> "ASCII"
	]
	,
	"\\(\\pm\\)"
	,
	TestID -> "OutputForm: BoxRules: ASCII: \\[PlusMinus]"
]

Test[
	tmpBoxesToString["\"\[PlusMinus]\"", {}, CharacterEncoding -> "Unicode"]
	,
	"\[PlusMinus]"
	,
	TestID -> "OutputForm: Unicode: \"\\[PlusMinus]\""
]
Test[
	tmpBoxesToString["\"\[PlusMinus]\"", {}, CharacterEncoding -> "ASCII"]
	,
	"+-"
	,
	TestID -> "OutputForm: ASCII: \"\\[PlusMinus]\""
]


(* ::Subsection:: *)
(*InputForm*)


tmpBoxesToString = boxesToString[##, FormatType -> InputForm]&


Test[
	tmpBoxesToString["a", {}]
	,
	"a"
	,
	TestID -> "InputForm: symbol"
]


Test[
	tmpBoxesToString[RowBox[{"a"}], {}]
	,
	"a"
	,
	TestID -> "InputForm: symbol in RowBox"
]

Test[
	tmpBoxesToString[RowBox[{"a", "b"}], {}]
	,
	"ab"
	,
	TestID -> "InputForm: two symbols"
]

Test[
	tmpBoxesToString[RowBox[{"a", " ", "b"}], {}]
	,
	"a b"
	,
	TestID -> "InputForm: two symbols, space separated"
]

Test[
	tmpBoxesToString[RowBox[{"a", "(", RowBox[{"b", "+", "c"}]}], {}]
	,
	"a(b+c"
	,
	TestID -> "InputForm: unbalanced bracket"
]

Test[
	tmpBoxesToString[RowBox[{"a", "(", RowBox[{"b", "+", "c"}], ")"}], {}]
	,
	"a(b+c)"
	,
	TestID -> "InputForm: balanced bracket"
]

Test[
	tmpBoxesToString[RowBox[{"-", "b"}], {}]
	,
	"-b"
	,
	TestID -> "InputForm: minus symbol"
]

Test[
	tmpBoxesToString[RowBox[{RowBox[{"-", "b"}]}], {}]
	,
	"-b"
	,
	TestID -> "InputForm: minus symbol double RowBox"
]


Test[
	tmpBoxesToString["\"str\"", {}]
	,
	"\"str\""
	,
	TestID -> "InputForm: string"
]

Test[
	tmpBoxesToString[RowBox[{"\"str\""}], {}]
	,
	"\"str\""
	,
	TestID -> "InputForm: string in RowBox"
]

Test[
	tmpBoxesToString["\"\\t\"", {}]
	,
	"\"\\t\""
	,
	TestID -> "InputForm: backslash"
]

Test[
	tmpBoxesToString["\"\\\\t\"", {}]
	,
	"\"\\\\t\""
	,
	TestID -> "InputForm: double backslash"
]

Test[
	tmpBoxesToString[RowBox[{"\"\\t\""}], {}]
	,
	"\"\\t\""
	,
	TestID -> "InputForm: backslash in RowBox"
]

Test[
	tmpBoxesToString[RowBox[{"\"\\\\t\""}], {}]
	,
	"\"\\\\t\""
	,
	TestID -> "InputForm: double backslash in RowBox"
]


Test[
	tmpBoxesToString[RowBox[{"a", "\"str\""}], {}]
	,
	"a\"str\""
	,
	TestID -> "InputForm: symbol and string"
]


Test[
	tmpBoxesToString[SubscriptBox["a", "\"b\""], {}]
	,
	If[$VersionNumber >= 10.2,
		"SubscriptBox[\"a\", \"\\\"b\\\"\"]"
	(* else *),
		"\\(a\\_\"b\"\\)"
	]
	,
	TestID -> "InputForm: formatting box"
]

Test[
	tmpBoxesToString[RowBox[{"\"d\"", SubscriptBox["a", "\"b\""], "c"}], {}]
	,
	If[$VersionNumber >= 10.2,
		"\"d\"SubscriptBox[\"a\", \"\\\"b\\\"\"]c"
	(* else *),
		"\"d\"\\(a\\_\"b\"\\)c"
	]
	,
	TestID -> "InputForm: formatting box in RowBox"
]

Test[
	tmpBoxesToString["\"d\!\(a\_b\)c\"", {}]
	,
	If[$VersionNumber >= 10.2,
		"dSubscriptBox[\"a\", \"b\"]c"
	(* else *),
		"d\\(a\\_b\\)c"
	]
	,
	TestID -> "InputForm: formatting box embedded in String"
]


Test[
	tmpBoxesToString[SubscriptBox["a", "\"b\""], $testBoxRules]
	,
	"\\sb{a}{\"b\"}"
	,
	TestID -> "InputForm: BoxRules: formatting box"
]

Test[
	tmpBoxesToString[
		RowBox[{"\"d\"", SubscriptBox["a", "\"b\""], "c"}],
		$testBoxRules
	]
	,
	"\"d\"\\sb{a}{\"b\"}c"
	,
	TestID -> "InputForm: BoxRules: formatting box in RowBox"
]

Test[
	tmpBoxesToString["\"d\!\(a\_b\)c\"", $testBoxRules]
	,
	"d\\sb{a}{b}c"
	,
	TestID -> "InputForm: BoxRules: formatting box embedded in String"
]

Test[
	tmpBoxesToString["\n", {}]
	,
	"\n"
	,
	TestID -> "InputForm: \\n"
]

Test[
	tmpBoxesToString["\[IndentingNewLine]", {}]
	,
	"\n"
	,
	TestID -> "InputForm: \\[IndentingNewLine]"
]

Test[
	tmpBoxesToString["\[PlusMinus]", {}, CharacterEncoding -> "Unicode"]
	,
	"\[PlusMinus]"
	,
	TestID -> "InputForm: Unicode: \\[PlusMinus]"
]
Test[
	tmpBoxesToString["\[PlusMinus]", {}, CharacterEncoding -> "ASCII"]
	,
	"\\[PlusMinus]"
	,
	TestID -> "InputForm: ASCII: \\[PlusMinus]"
]
Test[
	tmpBoxesToString[
		"\[PlusMinus]",
		{"\[PlusMinus]" -> "\\(\\pm\\)"},
		CharacterEncoding -> "Unicode"
	]
	,
	"\\(\\pm\\)"
	,
	TestID -> "InputForm: BoxRules: Unicode: \\[PlusMinus]"
]
Test[
	tmpBoxesToString[
		"\[PlusMinus]",
		{"\[PlusMinus]" -> "\\(\\pm\\)"},
		CharacterEncoding -> "ASCII"
	]
	,
	"\\(\\pm\\)"
	,
	TestID -> "InputForm: BoxRules: ASCII: \\[PlusMinus]"
]

Test[
	tmpBoxesToString["\"\[PlusMinus]\"", {}, CharacterEncoding -> "Unicode"]
	,
	"\"\[PlusMinus]\""
	,
	TestID -> "InputForm: Unicode: \"\\[PlusMinus]\""
]
Test[
	tmpBoxesToString["\"\[PlusMinus]\"", {}, CharacterEncoding -> "ASCII"]
	,
	"\"\\[PlusMinus]\""
	,
	TestID -> "InputForm: ASCII: \"\\[PlusMinus]\""
]


(* ::Subsection:: *)
(*Unsupported Form*)


Test[
	Catch[
		boxesToString["a", {}, FormatType -> TeXForm];
		,
		_
		,
		HoldComplete
	]
	,
	HoldComplete @@ {
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::unsupported,
				"MessageParameters" -> {
					HoldForm @ boxesToString["a", {}, FormatType -> TeXForm],
					HoldForm @
						CellsToTeXException["Unsupported", "FormatType"],
					HoldForm @ "FormatType",
					HoldForm @ TeXForm,
					HoldForm @ {InputForm, OutputForm}
				},
				"Type" -> {"Unsupported", "FormatType"}
			]
		],
		CellsToTeXException["Unsupported", "FormatType"]
	}
	,
	TestID -> "Unsupported FormatType: symbol"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[boxesToString[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[boxesToString[testArg]]
		,
		TestID -> "Incorrect arguments"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
