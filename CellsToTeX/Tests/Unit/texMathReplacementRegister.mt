(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`texMathReplacementRegister`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


(* ::Section:: *)
(*Tests*)


Block[{texMathReplacement},
	Test[
		texMathReplacementRegister["\[PlusMinus]"]
		,
		"\[PlusMinus]"
		,
		TestID -> "not defined: \\[PlusMinus]: texMathReplacementRegister"
	];
	Test[
		texMathReplacement // DownValues
		,
		{HoldPattern @ texMathReplacement["\[PlusMinus]"] :> "\\pm"}
		,
		TestID -> "not defined: \\[PlusMinus]: texMathReplacement DownValues"
	]
]


Block[{texMathReplacement},
	Test[
		texMathReplacementRegister["\[InvisibleSpace]"]
		,
		"\[InvisibleSpace]"
		,
		TestID ->
			"not defined: \\[InvisibleSpace]: texMathReplacementRegister"
	];
	Test[
		texMathReplacement // DownValues
		,
		{}
		,
		TestID ->
			"not defined: \\[InvisibleSpace]: texMathReplacement DownValues"
	]
]


Block[{texMathReplacement},
	texMathReplacement["\[Alpha]"] = "test old \[Alpha] replacement";
	
	Test[
		texMathReplacementRegister["\[Alpha]"]
		,
		"\[Alpha]"
		,
		TestID -> "defined: \\[Alpha]: texMathReplacementRegister"
	];
	Test[
		texMathReplacement // DownValues
		,
		{HoldPattern @ texMathReplacement["\[Alpha]"] :>
			"test old \[Alpha] replacement"}
		,
		TestID -> "defined: \\[Alpha]: texMathReplacement DownValues"
	]
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[texMathReplacementRegister], Protected]
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
