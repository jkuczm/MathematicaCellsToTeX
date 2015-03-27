(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`rethrowException`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Internal`", "CellsToTeX`Backports`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No options*)


Module[{body, rethrownBy},
	Test[
		rethrowException[rethrownBy][body]
		,
		body
		,
		TestID -> "No options: no exception"
	]
]


Module[{testVar, value, type, thrownBy},
	Test[
		Catch[
			rethrowException[testVar = "Evaluation leaked"] @ Throw[
				Failure[CellsToTeXException[type],
					Association[
						"MessageTemplate" :> CellsToTeXException::msg,
						"MessageParameters" -> {
							HoldForm @ thrownBy,
							HoldForm @ CellsToTeXException[type],
							HoldForm @ value
						}
					]
				],
				CellsToTeXException[type]
			];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[type],
				Association[
					"MessageTemplate" :> CellsToTeXException::msg,
					"MessageParameters" -> {
						HoldForm[testVar = "Evaluation leaked"],
						HoldForm @ CellsToTeXException[type],
						HoldForm @ value
					}
				]
			],
			CellsToTeXException[type]
		}
		,
		TestID -> "No options: matched exception"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "No options: matched exception: evaluation leak"
	]
]


Module[{rethrownBy, nonFailure, type},
	Test[
		Catch[
			rethrowException[rethrownBy] @ Throw[
				nonFailure,
				CellsToTeXException[type]
			];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[
				CellsToTeXException[
					"Error", "InvalidExceptionValue", "NonFailureObject"
				],
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm @ rethrowException[rethrownBy] @ Throw[
							nonFailure,
							CellsToTeXException[type]
						],
						HoldForm @ CellsToTeXException[
							"Error", "InvalidExceptionValue",
							"NonFailureObject"
						],
						HoldForm @ nonFailure
					}
				]
			],
			CellsToTeXException[
				"Error", "InvalidExceptionValue", "NonFailureObject"
			]
		}
		,
		TestID -> "No options: matched exception: non-Failure value"
	]
]
Module[{rethrownBy, type},
	Test[
		Catch[
			rethrowException[rethrownBy] @ Throw[
				Failure[CellsToTeXException[type],
					Association["MessageTemplate" :> CellsToTeXException::msg]
				],
				CellsToTeXException[type]
			];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[
				CellsToTeXException[
					"Error", "InvalidExceptionValue", "NoMessageParameters"
				]
				,
				Association[
					"MessageTemplate" :> CellsToTeXException::error,
					"MessageParameters" -> {
						HoldForm @ rethrowException[rethrownBy] @ Throw[
							Failure[CellsToTeXException[type],
								Association[
									"MessageTemplate" :>
										CellsToTeXException::msg
								]
							],
							CellsToTeXException[type]
						],
						HoldForm @ CellsToTeXException[
							"Error", "InvalidExceptionValue",
							"NoMessageParameters"
						],
						HoldForm[#]& @ Failure[CellsToTeXException[type],
							Association[
								"MessageTemplate" :> CellsToTeXException::msg
							]
						]
					}
				]
			],
			CellsToTeXException[
				"Error", "InvalidExceptionValue", "NoMessageParameters"
			]
		}
		,
		TestID ->
			"No options: matched exception: no MessageParameters in value"
	]
]


Module[{value, tag, rethrownBy},
	Test[
		Catch[
			rethrowException[rethrownBy][Throw[value, tag]];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete[value, tag]
		,
		TestID -> "No options: unknown exception"
	]
]


(* ::Subsection:: *)
(*All options*)


Module[
	{testVar, value, type, subType, additionalType, additionalVal, thrownBy}
	,
	Test[
		Catch[
			rethrowException[
				testVar = "Evaluation leaked",
				"TagPattern" -> CellsToTeXException[type, subType],
				"AdditionalExceptionSubtypes" -> {additionalType},
				"MessageTemplate" :> CellsToTeXException::newMsg,
				"AdditionalMessageParameters" -> {additionalVal}
			] @ Throw[
				Failure[CellsToTeXException[type, subType],
					Association[
						"MessageTemplate" :> CellsToTeXException::msg,
						"MessageParameters" -> {
							HoldForm @ thrownBy,
							HoldForm @ CellsToTeXException[type, subType],
							HoldForm @ value
						}
					]
				],
				CellsToTeXException[type, subType]
			];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[type, subType, additionalType],
				Association[
					"MessageTemplate" :> CellsToTeXException::newMsg,
					"MessageParameters" -> {
						HoldForm[testVar = "Evaluation leaked"],
						HoldForm @
							CellsToTeXException[type, subType, additionalType],
						HoldForm @ value,
						HoldForm @ additionalVal
					}
				]
			],
			CellsToTeXException[type, subType, additionalType]
		}
		,
		TestID -> "All options: matched exception"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "All options: matched exception: evaluation leak"
	]
]


Module[
	{testVar, value, type, subType, additionalType, additionalVal, thrownBy}
	,
	Test[
		Catch[
			rethrowException[
				testVar = "Evaluation leaked",
				"TagPattern" -> CellsToTeXException[type, subType],
				"AdditionalExceptionSubtypes" -> {additionalType},
				"MessageTemplate" :> CellsToTeXException::newMsg,
				"AdditionalMessageParameters" -> {additionalVal}
			] @ Throw[
				Failure[CellsToTeXException[type],
					Association[
						"MessageTemplate" :> CellsToTeXException::msg,
						"MessageParameters" -> {
							HoldForm @ thrownBy,
							HoldForm @ CellsToTeXException[type],
							HoldForm @ value
						}
					]
				],
				CellsToTeXException[type]
			];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException[type],
				Association[
					"MessageTemplate" :> CellsToTeXException::msg,
					"MessageParameters" -> {
						HoldForm @ thrownBy,
						HoldForm @ CellsToTeXException[type],
						HoldForm @ value
					}
				]
			],
			CellsToTeXException[type]
		}
		,
		TestID -> "All options: non-matched exception"
	];
	TestMatch[
		testVar
		,
		HoldPattern[testVar]
		,
		TestID -> "All options: non-matched exception: evaluation leak"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[rethrowException[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[rethrowException[testArg1, testArg2]]
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
