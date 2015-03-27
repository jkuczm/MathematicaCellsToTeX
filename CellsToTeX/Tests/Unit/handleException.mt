(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`handleException`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Internal`", "CellsToTeX`Backports`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


Module[{val1, val2, errType, thrownBy, failure},
	failure =
		Failure[CellsToTeXException[errType],
			Association[
				"MessageTemplate" :> CellsToTeXException::testErrMsg,
				"MessageParameters" -> {
					HoldForm[thrownBy],
					HoldForm @ CellsToTeXException[errType],
					HoldForm[val1],
					HoldForm[val2]
				}
			]
		];
	
	Test[
		handleException[failure, CellsToTeXException[errType]]
		,
		failure
		,
		Message[CellsToTeXException::testErrMsg,
			thrownBy,
			CellsToTeXException[errType],
			val1,
			val2
		]
		,
		TestID -> "Valid package exception"
	]
]


Module[{errType, failure},
	failure =
		Failure[CellsToTeXException[errType],
			Association["MessageTemplate" :> CellsToTeXException::testMsg]
		];
	
	Test[
		handleException[failure, CellsToTeXException[errType]]
		,
		failure
		,
		Message[CellsToTeXException::testMsg,
			"Unknown", CellsToTeXException[errType]
		]
		,
		TestID -> "No MessageParameters"
	]
]


Module[{val, errType},
	With[
		{
			failure =
				Failure[CellsToTeXException[errType],
					Association["MessageParameters" -> {HoldForm[val]}]
				]
		}
		,
		Test[
			handleException[failure, CellsToTeXException[errType]]
			,
			failure
			,
			Message[CellsToTeXException::unknownError,
				failure,
				CellsToTeXException[errType]
			]
			,
			TestID -> "No MessageTemplate"
		]
	]
]
Module[{val, errType, subType},
	Test[
		handleException[val, CellsToTeXException[errType, subType]]
		,
		$Failed
		,
		Message[CellsToTeXException::unknownError,
			val,
			CellsToTeXException[errType, subType]
		]
		,
		TestID -> "non-Failure value"
	]
]


Module[{value, tag},
	Test[
		Catch[
			handleException[value, tag];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete[value, tag]
		,
		TestID -> "non-CellsToTeXException throw"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[handleException[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[handleException[testArg]]
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
