(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`headRulesToBoxRules`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Configuration`", "CellsToTeX`Internal`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


Block[{$commandCharsToTeX = {"%" -> "%%", "<" -> "%<", ">" -> "%>"}},
	TestMatch[
		FractionBox -> "frac" // headRulesToBoxRules
		,
		Verbatim[HoldPattern] @ FractionBox[
			Verbatim[Pattern][pattName_, Verbatim[___]],
			Verbatim[OptionsPattern[]]
		] :>
			"%frac" <> ("<" <> makeString[#] <> ">"& /@ {pattName_})
		,
		TestID -> "Single rule"
	]
]
	
Block[{$commandCharsToTeX = {"A" -> "B", "C" -> "D", "E" -> "F"}},
	TestMatch[
		myBox -> {"myCommand", {{1}, {3}}} // headRulesToBoxRules
		,
		Verbatim[HoldPattern] @ myBox[
			Verbatim[Pattern][pattName_, Verbatim[___]],
			Verbatim[OptionsPattern[]]
		] :>
			"AmyCommand" <> (
				"C" <> # <> "E"& /@ MapAt[
					removeMathMode,
					makeString /@ {pattName_},
					{{1}, {3}}
				]
			)
		,
		TestID -> "Single rule: with math mode args positions"
	]
]
	
Block[{$commandCharsToTeX = {"|" -> "x", "(" -> "y", ")" -> "z"}},
	TestMatch[
		{SubscriptBox -> {"sub", 1}, UnderoverscriptBox -> "uo"} //
			headRulesToBoxRules
		,
		{
			Verbatim[HoldPattern] @ SubscriptBox[
				Verbatim[Pattern][pattName1_, Verbatim[___]],
				Verbatim[OptionsPattern[]]
			] :>
				"|sub" <> (
					"(" <> # <> ")"& /@
						MapAt[removeMathMode, makeString /@ {pattName1_}, 1]
				)
			,
			Verbatim[HoldPattern] @ UnderoverscriptBox[
				Verbatim[Pattern][pattName2_, Verbatim[___]],
				Verbatim[OptionsPattern[]]
			] :>
				"|uo" <> ("(" <> makeString[#] <> ")"& /@ {pattName2_})
		}
		,
		TestID -> "List of rules"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[headRulesToBoxRules[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[headRulesToBoxRules[testArg1, testArg2]]
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
