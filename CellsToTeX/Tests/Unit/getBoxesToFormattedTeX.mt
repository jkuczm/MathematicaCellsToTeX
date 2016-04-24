(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`getBoxesToFormattedTeX`", {"MUnit`"}]


Get["CellsToTeX`"]

PrependTo[$ContextPath, "CellsToTeX`Configuration`"]


SetOptions[getBoxesToFormattedTeX,
	"BoxRules" -> {},
	"BoxHeadsToTeXCommands" -> {},
	"CharacterRules" -> {}
]


(* ::Section:: *)
(*Tests*)


Test[
	getBoxesToFormattedTeX["BoxRules" -> {testBox -> testBoxReplacement}]
	,
	{testBox -> testBoxReplacement}
	,
	TestID -> "BoxRules option"
]

Module[{$log = {}},
	Block[
		{
			CellsToTeX`Internal`headRulesToBoxRules = (
				AppendTo[$log, #];
				{testHeadRulesToBoxRulesResult}
			)&
		},
		
		Test[
			getBoxesToFormattedTeX[
				"BoxHeadsToTeXCommands" -> {testBox -> testTeXCommand}
			]
			,
			{testHeadRulesToBoxRulesResult}
			,
			TestID -> "BoxHeadsToTeXCommands option: returned value"
		];
		Test[
			$log
			,
			{{testBox -> testTeXCommand}}
			,
			TestID ->
				"BoxHeadsToTeXCommands option: passing to headRulesToBoxRules"
		]
	]
]

TestMatch[
	getBoxesToFormattedTeX[
		"CharacterRules" -> {testChar -> testCharReplacement}
	]
	,
	{
		Verbatim[Pattern][pattName_, Verbatim[_String]] :>
			StringReplace[
				StringJoin @ Replace[
					Characters[makeStringDefault[pattName_]],
					{testChar -> testCharReplacement},
					{1}
				],
				"\\)\\(" -> ""
			]
	}
	,
	TestID -> "CharacterRules option"
]


(* ::Subsection:: *)
(*Protected attribute*)


Test[
	MemberQ[Attributes[getBoxesToFormattedTeX], Protected]
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
