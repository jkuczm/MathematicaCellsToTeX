(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`extractMessageLink`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Test[
	ButtonBox["\[RightSkeleton]",
		ButtonStyle -> "Link",
		ButtonData :> "paclet:ref/message/General/infy",
		ButtonNote -> "Power::infy"
	] // extractMessageLink
	,
	"message/General/infy"
	,
	TestID -> "message link"
]

Test[
	RowBox[{
		StyleBox[RowBox[{"Sin", "::", "argx"}], "MessageName"],
		RowBox[{":", " "}],
		"\
\<\"\\!\\(Sin\\) called with \\!\\(2\\) arguments; 1 argument is expected. \
\\!\\(\\*ButtonBox[\\\">>\\\", ButtonStyle->\\\"Link\\\", ButtonFrame->None, \
ButtonData:>\\\"paclet:ref/message/General/argx\\\", \
ButtonNote -> \\\"Sin::argx\\\"]\\)\"\>"
	}] // extractMessageLink
	,
	"message/General/argx"
	,
	TestID -> "message link in string"
]

Test[
	RowBox[{
		StyleBox[
			"\<\"\!\(\*RowBox[{StyleBox[\\\"x\\\", \\\"TI\\\"], \\\"+\\\", \
			StyleBox[\\\"y\\\", \\\"TI\\\"], \\\"+\\\", StyleBox[\\\"z\\\", \
			\\\"TI\\\"]}]\) represents a sum of terms. \"\>"
			,
			"MSG"
		],
		" ",
		ButtonBox[
			StyleBox[">>", "SR"],
			Active -> True,
			BaseStyle -> "Link",
			ButtonData -> "paclet:ref/Plus"
		]
	}] // extractMessageLink
	,
	"Plus"
	,
	TestID -> "usage message link"
]

Test[
	ButtonBox[
		"Integrate",
		BaseStyle -> "Link",
		ButtonData -> "paclet:ref/Integrate"
	] // extractMessageLink
	,
	Missing["NotFound"]
	,
	TestID -> "non-message documentation link"
]

Test[
	ButtonBox[
		"\"Wolfram Research, Inc.\"",
		BaseStyle -> "Hyperlink",
		ButtonData -> {URL["http://www.wolfram.com"], None},
		ButtonNote -> "http://www.wolfram.com"
	] // extractMessageLink
	,
	Missing["NotFound"]
	,
	TestID -> "external hyperlink"
]

Test[
	RowBox[{"a", " ", "b"}] // extractMessageLink
	,
	Missing["NotFound"]
	,
	TestID -> "no button box"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[extractMessageLink[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[extractMessageLink[testArg1, testArg2]]
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
