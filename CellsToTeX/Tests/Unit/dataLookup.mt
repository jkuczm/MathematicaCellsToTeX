(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`dataLookup`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

$ContextPath =
	Join[{"CellsToTeX`Internal`", "CellsToTeX`Backports`"}, $ContextPath]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Two arguments*)


(* ::Subsubsection:: *)
(*One key*)


Module[{a, b},
	Test[
		dataLookup[{a -> b}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing: one Rule"
	]
]

Module[{a, b},
	Test[
		dataLookup[{a :> b}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing: one RuleDelayed"
	]
]

Module[{a, b},
	Test[
		dataLookup[{{a -> b}}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing: one Rule: deep"
	]
]

Block[{a, b},
	Test[
		dataLookup[{"a" -> b}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing String: one Rule"
	]
]

Block[{a, b},
	Test[
		dataLookup[{a -> b}, "a"]
		,
		b
		,
		TestID -> "One key: String: existing Symbol: one Rule"
	]
]

Module[{a, b, c, d},
	Test[
		dataLookup[{a -> b, c -> d}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing first: two Rules"
	]
]

Module[{a, b, c, d},
	Test[
		dataLookup[{a -> b, c -> d}, c]
		,
		d
		,
		TestID -> "One key: Symbol: existing second: two Rules"
	]
]

Module[{a, b, c},
	Test[
		dataLookup[{a -> b, a -> c}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing twice: two Rules"
	]
]

Block[{a, b, c},
	Test[
		dataLookup[{"a" -> b, a -> c}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing twice String, Symbol: two Rules"
	]
]

Block[{a, b, c},
	Test[
		dataLookup[{a -> b, "a" -> c}, a]
		,
		b
		,
		TestID -> "One key: Symbol: existing twice Symbol, String: two Rules"
	]
]


Block[{a},
	Test[
		Catch[
			dataLookup[{}, a];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missing,
					"MessageParameters" -> {
						HoldForm[dataLookup[{}, a]],
						HoldForm[CellsToTeXException["Missing", "Keys"]],
						HoldForm["Keys"],
						HoldForm[{"a"}],
						HoldForm[{}]
					},
					"Type" -> {"Missing", "Keys"}
				]
			],
			CellsToTeXException["Missing", "Keys"]
		}
		,
		TestID -> "One key: Symbol: non-existing: no rules"
	]
]

Block[{a, b, c},
	Test[
		Catch[
			dataLookup[{a -> b}, c];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missing,
					"MessageParameters" -> {
						HoldForm[dataLookup[{a -> b}, c]],
						HoldForm[CellsToTeXException["Missing", "Keys"]],
						HoldForm["Keys"],
						HoldForm[{"c"}],
						HoldForm[{"a"}]
					},
					"Type" -> {"Missing", "Keys"}
				]
			],
			CellsToTeXException["Missing", "Keys"]
		}
		,
		TestID -> "One key: Symbol: non-existing: one Rule"
	]
]

Block[{a, b, c, d, e},
	Test[
		Catch[
			dataLookup[{a -> b, "c" -> d}, e];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missing,
					"MessageParameters" -> {
						HoldForm[dataLookup[{a -> b, "c" -> d}, e]],
						HoldForm[CellsToTeXException["Missing", "Keys"]],
						HoldForm["Keys"],
						HoldForm[{"e"}],
						HoldForm[{"a", "c"}]
					},
					"Type" -> {"Missing", "Keys"}
				]
			],
			CellsToTeXException["Missing", "Keys"]
		}
		,
		TestID -> "One key: Symbol: non-existing: two Rules"
	]
]

Block[{a, b, c, d},
	Test[
		Catch[
			dataLookup[{a -> b, "a" -> c}, d];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missing,
					"MessageParameters" -> {
						HoldForm[dataLookup[{a -> b, "a" -> c}, d]],
						HoldForm[CellsToTeXException["Missing", "Keys"]],
						HoldForm["Keys"],
						HoldForm[{"d"}],
						HoldForm[{"a"}]
					},
					"Type" -> {"Missing", "Keys"}
				]
			],
			CellsToTeXException["Missing", "Keys"]
		}
		,
		TestID -> "One key: Symbol: non-existing: two Rules same name keys"
	]
]


(* ::Subsubsection:: *)
(*Many keys*)


Module[{a, b, c, d},
	Test[
		dataLookup[{a -> b, c -> d}, {a, c}]
		,
		{b, d}
		,
		TestID -> "Two keys: Symbol: existing: two Rules"
	]
]

Module[{a, b, c, d},
	Test[
		dataLookup[{a -> b, c -> d}, {c, a}]
		,
		{d, b}
		,
		TestID -> "Two keys: Symbol: existing: two Rules: different order"
	]
]

Module[{a, b},
	Test[
		dataLookup[{{{a -> b}, c -> d}}, {a, c}]
		,
		{b, d}
		,
		TestID -> "Two keys: Symbol: existing: two Rules: deep"
	]
]

Block[{a, b},
	Test[
		dataLookup[{"a" -> b, c -> d}, {a, "c"}]
		,
		{b, d}
		,
		TestID -> "Two keys: \
Symbol existing String, String: existing Symbol: two Rules"
	]
]

Module[{a, b, c, d, e, f},
	Test[
		dataLookup[{a -> b, c -> d, e -> f}, {a, c}]
		,
		{b, d}
		,
		TestID -> "Two keys: Symbol: existing: three Rules"
	]
]


Block[{a, b, c},
	Test[
		Catch[
			dataLookup[{}, {a, c}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missing,
					"MessageParameters" -> {
						HoldForm[dataLookup[{}, {a, c}]],
						HoldForm[CellsToTeXException["Missing", "Keys"]],
						HoldForm["Keys"],
						HoldForm[{"a", "c"}],
						HoldForm[{}]
					},
					"Type" -> {"Missing", "Keys"}
				]
			],
			CellsToTeXException["Missing", "Keys"]
		}
		,
		TestID -> "Two keys: Symbol: non-existing: no Rules"
	]
]

Block[{a, b, c},
	Test[
		Catch[
			dataLookup[{a -> b}, {a, c}];
			,
			_
			,
			HoldComplete
		]
		,
		HoldComplete @@ {
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::missing,
					"MessageParameters" -> {
						HoldForm[dataLookup[{a -> b}, {a, c}]],
						HoldForm[CellsToTeXException["Missing", "Keys"]],
						HoldForm["Keys"],
						HoldForm[{"c"}],
						HoldForm[{"a"}]
					},
					"Type" -> {"Missing", "Keys"}
				]
			],
			CellsToTeXException["Missing", "Keys"]
		}
		,
		TestID -> "Two keys: Symbol: existing, non-existing: one Rule"
	]
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[dataLookup[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[dataLookup[testArg]]
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
