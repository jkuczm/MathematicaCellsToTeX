(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`CellsToTeXPreamble`", {"MUnit`"}]


Get["CellsToTeX`"]

$ContextPath =
	Join[{"CellsToTeX`Configuration`", "CellsToTeX`Backports`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Default*)


Test[
	CellsToTeXPreamble[]
	,
	"\\mmaSet{morefv={gobble=2}}"
	,
	TestID -> "Default"
]
Internal`InheritedBlock[{texMathReplacement},
	texMathReplacement["\[Alpha]"] = "\\alpha";
	texMathReplacement["\[Beta]"] = "\\beta";
	
	Test[
		CellsToTeXPreamble[]
		,
		"\
\\mmaSet{morefv={gobble=2}}
\\mmaDefineMathReplacement{\[Alpha]}{\\alpha}
\\mmaDefineMathReplacement{\[Beta]}{\\beta}"
		,
		TestID -> "Default: defined texMathReplacement"
	]
]


(* ::Subsection:: *)
(*Gobble option*)


Internal`InheritedBlock[{mmaCellProcessor},
	SetOptions[mmaCellProcessor, "Indentation" -> "1234557"];
	
	Test[
		CellsToTeXPreamble["Gobble" -> Automatic]
		,
		"\\mmaSet{morefv={gobble=7}}"
		,
		TestID -> "Gobble -> Automatic"
	]
]
Test[
	CellsToTeXPreamble["Gobble" -> 4]
	,
	"\\mmaSet{morefv={gobble=4}}"
	,
	TestID -> "Gobble -> 4"
]
Test[
	CellsToTeXPreamble["Gobble" -> None]
	,
	""
	,
	TestID -> "Gobble -> None"
]
Module[{unsupportedValue},
	With[
		{
			heldMessage =
				HoldForm @ Message[CellsToTeXException::unsupported,
					HoldForm @
						CellsToTeXPreamble["Gobble" -> unsupportedValue],
					HoldForm @ CellsToTeXException[
						"Unsupported", "OptionValue", "Gobble"
					],
					HoldForm @ "OptionValue",
					HoldForm @ unsupportedValue,
					HoldForm @ {Automatic, None, "non-negative integer"}
				]
		}
		,
		Test[
			CellsToTeXPreamble["Gobble" -> unsupportedValue]
			,
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::unsupported,
					"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
					"Type" -> {"Unsupported", "OptionValue", "Gobble"}
				]
			]
			,
			{heldMessage}
			,
			TestID -> "Gobble -> unsupportedValue"
		]
	]
]


(* ::Subsection:: *)
(*UseListings option*)


Internal`InheritedBlock[{annotateSyntaxProcessor},
	SetOptions[annotateSyntaxProcessor, "CommonestTypesAsTeXOptions" -> False];
	
	Test[
		CellsToTeXPreamble["UseListings" -> Automatic]
		,
		"\\mmaSet{uselistings=false,morefv={gobble=2}}"
		,
		TestID -> "UseListings -> Automatic"
	]
]
Test[
	CellsToTeXPreamble["UseListings" -> True]
	,
	"\\mmaSet{uselistings=true,morefv={gobble=2}}"
	,
	TestID -> "UseListings -> True"
]
Test[
	CellsToTeXPreamble["UseListings" -> False]
	,
	"\\mmaSet{uselistings=false,morefv={gobble=2}}"
	,
	TestID -> "UseListings -> False"
]
Test[
	CellsToTeXPreamble["UseListings" -> None]
	,
	"\\mmaSet{morefv={gobble=2}}"
	,
	TestID -> "UseListings -> None"
]
Module[{unsupportedValue},
	With[
		{
			heldMessage =
				HoldForm @ Message[CellsToTeXException::unsupported,
					HoldForm @
						CellsToTeXPreamble["UseListings" -> unsupportedValue],
					HoldForm @ CellsToTeXException[
						"Unsupported", "OptionValue", "UseListings"
					],
					HoldForm @ "OptionValue",
					HoldForm @ unsupportedValue,
					HoldForm @ {Automatic, True, False, None}
				]
		}
		,
		Test[
			CellsToTeXPreamble["UseListings" -> unsupportedValue]
			,
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::unsupported,
					"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
					"Type" -> {"Unsupported", "OptionValue", "UseListings"}
				]
			]
			,
			{heldMessage}
			,
			TestID -> "UseListings -> unsupportedValue"
		]
	]
]


(* ::Subsection:: *)
(*TeXOptions option*)


Block[{opt1, opt2, val1, val2},
	Test[
		CellsToTeXPreamble["TeXOptions" -> {opt1 -> val1, opt2 :> val2}]
		,
		"\\mmaSet{morefv={gobble=2},opt1=val1,opt2=val2}"
		,
		TestID -> "TeXOptions -> Automatic"
	]
]
Module[{unsupportedValue},
	With[
		{
			heldMessage =
				HoldForm @ Message[CellsToTeXException::unsupported,
					HoldForm @
						CellsToTeXPreamble["TeXOptions" -> unsupportedValue],
					HoldForm @ CellsToTeXException[
						"Unsupported", "OptionValue", "TeXOptions"
					],
					HoldForm @ "OptionValue",
					HoldForm @ unsupportedValue,
					HoldForm @ {"list of options"}
				]
		}
		,
		Test[
			CellsToTeXPreamble["TeXOptions" -> unsupportedValue]
			,
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::unsupported,
					"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
					"Type" -> {"Unsupported", "OptionValue", "TeXOptions"}
				]
			]
			,
			{heldMessage}
			,
			TestID -> "TeXOptions -> unsupportedValue"
		]
	]
]


(* ::Subsection:: *)
(*TeXMathReplacement option*)


Module[{testMathRepl},
	testMathRepl["test1"] = "testRepl1";
	testMathRepl["test2"] = "testRepl2";
	testMathRepl["test3"] = "testRepl3";
	
	Test[
		CellsToTeXPreamble["TeXMathReplacement" -> testMathRepl]
		,
		"\
\\mmaSet{morefv={gobble=2}}
\\mmaDefineMathReplacement{test1}{testRepl1}
\\mmaDefineMathReplacement{test2}{testRepl2}
\\mmaDefineMathReplacement{test3}{testRepl3}"
		,
		TestID -> "TeXMathReplacement -> testMathRepl"
	]
]
With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::unsupported,
				HoldForm @ CellsToTeXPreamble[
					"TeXMathReplacement" -> "unsupportedValue"
				],
				HoldForm @ CellsToTeXException[
					"Unsupported", "OptionValue", "TeXMathReplacement"
				],
				HoldForm @ "OptionValue",
				HoldForm @ "unsupportedValue",
				HoldForm @ {"a symbol"}
			]
	}
	,
	Test[
		CellsToTeXPreamble["TeXMathReplacement" -> "unsupportedValue"]
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::unsupported,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Unsupported", "OptionValue", "TeXMathReplacement"}
			]
		]
		,
		{heldMessage}
		,
		TestID -> "TeXMathReplacement -> unsupportedValue"
	]
]


(* ::Subsection:: *)
(*Mixed options*)


Module[{testMathRepl},
	testMathRepl["x"] = "y";
	
	Test[
		CellsToTeXPreamble[
			"TeXMathReplacement" -> testMathRepl,
			"Gobble" -> 3,
			"TeXOptions" -> {"a" -> "b"},
			"UseListings" -> True
		]
		,
		"\
\\mmaSet{uselistings=true,morefv={gobble=3},a=b}
\\mmaDefineMathReplacement{x}{y}"
		,
		TestID -> "Mixed options"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
