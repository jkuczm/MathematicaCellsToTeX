(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`throwException`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Internal`", "CellsToTeX`Backports`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


Module[{thrownBy, errType},
	Test[
		Catch[
			throwException[thrownBy, {errType}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "no values"
	]
]


Module[{thrownBy, errType, val},
	Test[
		Catch[
			throwException[thrownBy, {errType}, {val}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType],
						HoldForm[val]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "one value"
	]
]


Module[{thrownBy, errType, errSubType, val},
	Test[
		Catch[
			throwException[thrownBy, {errType, errSubType}, {val}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType, errSubType],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType, errSubType],
						HoldForm[val]
					}
				]
			],
			CellsToTeXException[errType, errSubType]
		}
		,
		TestID -> "exception subtype"
	]
]


Module[{thrownBy, val},
	Test[
		Catch[
			throwException[thrownBy, "testExceptionType", {val}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException["testExceptionType"],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException["testExceptionType"],
						HoldForm[val]
					}
				]
			],
			CellsToTeXException["testExceptionType"]
		}
		,
		TestID -> "exception type string"
	]
]


Module[{thrownBy, errType, val},
	Test[
		Catch[
			throwException[HoldComplete[thrownBy], {errType}, {val}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType],
						HoldForm[val]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "thrownBy in HoldComplete"
	]
]


Module[{thrownBy, errType, val1, val2},
	Test[
		Catch[
			throwException[thrownBy, {errType}, {val1, val2}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType],
						HoldForm[val1],
						HoldForm[val2]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "two values"
	]
]


Module[{thrownBy, errType, testVar1, val2},
	Test[
		Catch[
			throwException[thrownBy, {errType},
				HoldComplete[testVar1 = "Evaluation leaked", val2]
			];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType],
						HoldForm[testVar1 = "Evaluation leaked"],
						HoldForm[val2]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "two values: in HoldComplete"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "two values: in HoldComplete: evaluation leak"
	]
]


Module[{errType, testVar},
	Test[
		Catch[
			throwException[testVar = "Evaluation leaked", {errType}, {}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm[testVar = "Evaluation leaked"],
						HoldForm @ CellsToTeXException[errType]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "thrownBy evaluation leak: throwException evaluation"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "thrownBy evaluation leak: test variable"
	]
]


Module[{thrownBy, errType, val},
	Test[
		Catch[
			throwException[thrownBy, {errType}, {val}, "messageName"];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::messageName,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType],
						HoldForm[val]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "explicit message name"
	]
]


Module[{thrownBy, errType},
	Test[
		Catch[
			throwException[thrownBy, {errType}, "messageName"];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[errType],
				Association[
					"MessageTemplate" :> CellsToTeXException::messageName,
					"MessageParameters" -> {
						HoldForm[thrownBy],
						HoldForm @ CellsToTeXException[errType]
					}
				]
			],
			CellsToTeXException[errType]
		}
		,
		TestID -> "explicit message name, no values"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[throwException[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[throwException[testArg]]
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
