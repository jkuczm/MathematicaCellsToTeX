(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`headRulesToBoxRules`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


(* ::Section:: *)
(*Tests*)


Block[{$commandCharsToTeX = {"%" -> "%%", "<" -> "%<", ">" -> "%>"}},
	TestMatch[
		FractionBox -> {"frac", 2} // headRulesToBoxRules
		,
		Verbatim[HoldPattern] @ FractionBox[
			Verbatim[Pattern][pattName_, Verbatim @ Repeated[_, {2}]],
			Verbatim[OptionsPattern[]]
		] :>
			"%frac" <> ("<" <> makeString[#] <> ">"& /@ {pattName_})
		,
		TestID -> "Single rule"
	]
]
	
Block[{$commandCharsToTeX = {"|" -> "x", "(" -> "y", ")" -> "z"}},
	TestMatch[
		{SubscriptBox -> {"sub", 1}, UnderoverscriptBox -> {"uo", 3}} //
			headRulesToBoxRules
		,
		{
			Verbatim[HoldPattern] @ SubscriptBox[
				Verbatim[Pattern][pattName1_,  Verbatim @ Repeated[_, {1}]],
				Verbatim[OptionsPattern[]]
			] :>
				"|sub" <> ("(" <> makeString[#] <> ")"& /@ {pattName1_})
			,
			Verbatim[HoldPattern] @ UnderoverscriptBox[
				Verbatim[Pattern][pattName2_,  Verbatim @ Repeated[_, {3}]],
				Verbatim[OptionsPattern[]]
			] :>
				"|uo" <> ("(" <> makeString[#] <> ")"& /@ {pattName2_})
		}
		,
		TestID -> "List of rules"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[headRulesToBoxRules], Protected]
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
