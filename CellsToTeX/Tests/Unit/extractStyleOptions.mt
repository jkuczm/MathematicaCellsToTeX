(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`extractStyleOptions`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


(* ::Section:: *)
(*No rules*)


Module[{style},
	Test[
		extractStyleOptions[style, {}]
		,
		{}
		,
		TestID -> "no rules"
	]
]


(* ::Section:: *)
(*One rule*)


Module[{style, style1, opt, val},
	Test[
		extractStyleOptions[style, {{style1, opt} -> val}]
		,
		{}
		,
		TestID -> "1 Rule: non-matching"
	]
]
Module[{style, opt, val},
	Test[
		extractStyleOptions[style, {{style, opt} -> val}]
		,
		{opt -> val}
		,
		TestID -> "1 Rule: matching same symbol"
	]
]
Module[{style, opt, val},
	Test[
		extractStyleOptions[style, {{style, opt} :> val}]
		,
		{opt :> val}
		,
		TestID -> "1 RuleDelayed: matching same symbol"
	]
]
Module[{style, style1, opt, val},
	Test[
		extractStyleOptions[style, {{style | style1, opt} -> val}]
		,
		{opt -> val}
		,
		TestID -> "1 Rule: matching Alternatives"
	]
]
Module[{style, opt, val},
	Test[
		extractStyleOptions[style, {{_, opt} -> val}]
		,
		{opt -> val}
		,
		TestID -> "1 Rule: matching Blank"
	]
]


(* ::Section:: *)
(*Two rules*)


Module[{style, style1, style2, opt1, opt2, val1, val2},
	Test[
		extractStyleOptions[style, {
			{style1, opt1} -> val1, {style2, opt2} -> val2}
		]
		,
		{}
		,
		TestID -> "2 rules: non-matching"
	]
]
Module[{style, style1, opt1, opt2, val1, val2},
	Test[
		extractStyleOptions[style, {
			{style1, opt1} -> val1, {style, opt2} -> val2}
		]
		,
		{opt2 -> val2}
		,
		TestID -> "2 rules: non-matching, matching same symbol"
	]
]
Module[{style, style1, opt1, opt2, val1, val2},
	Test[
		extractStyleOptions[style, {
			{style1 | style, opt1} -> val1, {_, opt2} :> val2}
		]
		,
		{opt1 -> val1, opt2 :> val2}
		,
		TestID -> "2 rules: matching Alternatives, delayed matching Blank"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[extractStyleOptions[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[extractStyleOptions[testArg]]
		,
		TestID -> "Incorrect arguments"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["tmpContext`*"]
Quiet[Remove["tmpContext`*"], {Remove::rmnsm}]

Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
