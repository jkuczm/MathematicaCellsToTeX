(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`catchException`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


Module[{body},
	Block[{handleException},
		Test[
			catchException[body]
			,
			body
			,
			TestID -> "no exception"
		]
	]
]


Module[{errType, failure},
	Block[{handleException},
		Test[
			catchException @ Throw[failure, CellsToTeXException[errType]]
			,
			handleException[failure, CellsToTeXException[errType]]
			,
			TestID -> "known exception"
		]
	]
]


Module[{value, tag},
	Block[{handleException},
		Test[
			Catch[
				catchException[Throw[value, tag]];
				,
				_
				,
				HoldComplete
			]
			,
			HoldComplete[value, tag]
			,
			TestID -> "unknown exception"
		]
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[catchException[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[catchException[testArg1, testArg2]]
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
