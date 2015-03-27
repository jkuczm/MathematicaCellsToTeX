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
		{FractionBox -> "frac"} // headRulesToBoxRules
		,
		{
			FractionBox[Verbatim[Pattern][name_, Verbatim[___]]] :>
				"%frac" <> ("<" <> makeString[#] <> ">"& /@ {name_})
		}
		,
		TestID -> "1 rule"
	];
	
	TestMatch[
		{myBox -> {"myCommand", {{1}, {3}}}} // headRulesToBoxRules
		,
		{
			myBox[Verbatim[Pattern][name_, Verbatim[___]]] :>
				"%myCommand" <> (
					"<" <> # <> ">"& /@ MapAt[
						removeMathMode,
						makeString /@ {name_},
						{{1}, {3}}
					]
				)
		}
		,
		TestID -> "1 rule: with math mode args positions"
	];
	
	TestMatch[
		{SubscriptBox -> "sub", SuperscriptBox -> "sup"} //
			headRulesToBoxRules
		,
		{
			SubscriptBox[Verbatim[Pattern][name_, Verbatim[___]]] :>
				"%sub" <> ("<" <> makeString[#] <> ">"& /@ {name_}),
			SuperscriptBox[Verbatim[Pattern][name_, Verbatim[___]]] :>
				"%sup" <> ("<" <> makeString[#] <> ">"& /@ {name_})
		}
		,
		TestID -> "2 rules"
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
