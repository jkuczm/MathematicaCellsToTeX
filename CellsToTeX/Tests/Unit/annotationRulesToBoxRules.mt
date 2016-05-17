(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`annotationRulesToBoxRules`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[
		{
			"SyntaxAnnotations`",
			"CellsToTeX`Configuration`",
			"CellsToTeX`Internal`"
		},
		$ContextPath
	]


(* ::Section:: *)
(*Tests*)


Block[{$commandCharsToTeX = {"%" -> "%%", "<" -> "%<", ">" -> "%>"}},
	TestMatch[
		{"type" -> {"key", "comand"}} // annotationRulesToBoxRules
		,
		{
			syntaxBox[
				Verbatim[Pattern][boxes_, Verbatim[_]], "type", Verbatim[___]
			] :>
				"%comand<" <> makeString[boxes_] <> ">"
		}
		,
		TestID -> "1 rule"
	]
]

Block[{$commandCharsToTeX = {"\\" -> "test1", "{" -> "test2", "}" -> "test3"}},
	TestMatch[
		{"type1" -> {"key1", "comand1"}, "type2" -> {"key2", "comand2"}} //
			annotationRulesToBoxRules
		,
		{
			syntaxBox[
				Verbatim[Pattern][boxes_, Verbatim[_]], "type1", Verbatim[___]
			] :>
				"\\comand1{" <> makeString[boxes_] <> "}"
			,
			syntaxBox[
				Verbatim[Pattern][boxes_, Verbatim[_]], "type2", Verbatim[___]
			] :>
				"\\comand2{" <> makeString[boxes_] <> "}"
		}
		,
		TestID -> "2 rules"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[annotationRulesToBoxRules[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[annotationRulesToBoxRules[testArg]]
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
