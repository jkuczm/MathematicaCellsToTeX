(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`CellToTeX`", {"MUnit`"}]


Get["CellsToTeX`"]

$ContextPath =
	Join[{"CellsToTeX`Configuration`", "CellsToTeX`Backports`"}, $ContextPath]


(*	In tests we use pattern matching on contents of Association, it works since
	Mathematica v10.4, and for backported Association for versions < 10.0,
	but not for versions from 10.0 to 10.3, so for this versions we normalize
	Associations. *)
normalAssoc =
	If[10 <= $VersionNumber < 10.4,
		Module[{association},
			(# /. assoc_Association :> association @@ Normal[assoc])&
		]
	(* else *),
		Identity
	]


(* ::Section:: *)
(*Tests*)


Test[
	CellToTeX[
		Integrate[{y^(-3) * (1 - (a / y)^2)^(-2)}, {y, r, Infinity}] //
			MakeBoxes,
		"Style" -> "Input",
		"ProcessorOptions" -> {
			"StringRules" ->
				Join[
					{"\[DifferentialD]" -> "\\(\\mathbbm{d}\\)"},
					$stringsToTeX,
					$commandCharsToTeX
				]
		},
		"TeXOptions" -> {"morelst" -> "{morefvcmdparams=\\mathbbm 1}"}
	]
	,
	"\
\\begin{mmaCell}[morelst={morefvcmdparams=\\mathbbm 1},morefunctionlocal={y}]{Input}
  \\mmaSubSupM{\\int}{r}{\\mmaDef{\\(\\pmb{\\infty}\\)}}\\{\\mmaFrac{1}{\\mmaSup{y}{3} \\mmaSup{(1-\\mmaSup{(\\mmaFrac{a}{y})}{2})}{2}}\\}\\(\\mathbbm{d}\\)y
\\end{mmaCell}"
	,
	TestID -> "pure boxes: formatting, syntax: Input"
]


UsingFrontEnd @ Test[
	CellToTeX[
		Integrate[{1/(y^3 * (1 - (a / y)^2)^2)}, {y, r, Infinity}] //
			MakeBoxes,
		"Style" -> "Code"
	]
	,
	"\
\\begin{mmaCell}[morefunctionlocal={y}]{Code}
  Integrate[{1/(y^3*(1 - (a/y)^2)^2)}, {y, r, Infinity}]
\\end{mmaCell}"
	,
	TestID -> "pure boxes: formatting, syntax: Code"
]


Test[
	CellToTeX[
		RowBox[{RowBox[{"#", " ", "##", " ", "#something", " ", "##4"}], "&"}],
		"Style" -> "Input"
	]
	,
	"\
\\begin{mmaCell}[morepattern={\\#, \\#\\#, \\#something, \\#\\#4}]{Input}
  # ## #something ##4&
\\end{mmaCell}"
	,
	TestID -> "pure boxes: pure function with slots: Input"
]


Block[{\[Phi]1},
	\[Phi]1[x_] := x;
	
	Test[
		CellToTeX[
			Sum[
				\[Phi]1[\[Alpha]]^2 + \[Chi]\[Omega]\[Nu]\[Sigma]\[Tau],
				{\[Alpha], 1, \[Pi]}
			] // MakeBoxes
			,
			"Style" -> "Input"
		]
		,
		"\
\\begin{mmaCell}{Input}
  \\mmaUnderOver{\\(\\pmb{\\sum}\\)}{\\mmaFnc{\\(\\pmb{\\alpha}\\)}=1}{\\mmaDef{\\(\\pmb{\\pi}\\)}}(\\mmaSup{\\mmaDef{\\(\\pmb{\\phi}\\)1}[\\mmaFnc{\\(\\pmb{\\alpha}\\)}]}{2}+\\mmaUnd{\\(\\pmb{\\chi\\omega\\nu\\sigma\\tau}\\)})
\\end{mmaCell}"
		,
		TestID -> "pure boxes: formatting, syntax, non-ASCII symbols: Input"
	]
]


Block[{\[Phi]1},
	\[Phi]1[x_] := x;
	
	UsingFrontEnd @ Test[
		CellToTeX[
			Sum[
				\[Phi]1[\[Alpha]]^2 + \[Chi]\[Omega]\[Nu]\[Sigma]\[Tau],
				{\[Alpha], 1, \[Pi]}
			] // MakeBoxes
			,
			"Style" -> "Code"
		]
		,
		"\
\\begin{mmaCell}{Code}
  Sum[\\mmaDef{\\[Phi]1}[\\mmaFnc{\\[Alpha]}]^2 + \\mmaUnd{\\[Chi]\\[Omega]\\[Nu]\\[Sigma]\\[Tau]}, {\\mmaFnc{\\[Alpha]}, 1, Pi}]
\\end{mmaCell}"
		,
		TestID -> "pure boxes: formatting, syntax, non-ASCII symbols: Code"
	]
]


Block[{texMathReplacement},
	texMathReplacement["\[UnderBracket]"] = "\\textvisiblespace";
	UsingFrontEnd @ Test[
		CellToTeX[
			{
				\[Alpha]_ \[RuleDelayed] \[Alpha],
				x \[Equal] \[Beta]\[UnderBracket]\[Gamma]
			} // MakeBoxes
			,
			"Style" -> "Code",
			"ProcessorOptions" -> {
				"NonASCIIHandler" -> texMathReplacementRegister,
				"CharacterEncoding" -> "Unicode"
			}
		]
		,
		"\
\\begin{mmaCell}{Code}
  {\\mmaPat{\[Alpha]_} :> \\mmaPat{\[Alpha]}, x == \\mmaUnd{\[Beta]\[UnderBracket]\[Gamma]}}
\\end{mmaCell}"
		,
		TestID -> "pure boxes: syntax, non-ASCII symbols (private Unicode): \
Code, Unicode encoding, math replacements"
	];
	Test[
		texMathReplacement // DownValues
		,
		{
			HoldPattern@texMathReplacement@"\[Alpha]" :> "\\alpha",
			HoldPattern@texMathReplacement@"\[Beta]" :> "\\beta",
			HoldPattern@texMathReplacement@"\[Gamma]" :> "\\gamma",
			HoldPattern@texMathReplacement@"\[UnderBracket]" :>
				"\\textvisiblespace"
		}
		,
		TestID -> "pure boxes: syntax, non-ASCII symbols (private Unicode): \
Code, Unicode encoding, math replacements: texMathReplacement"
	]
]


Test[
	CellToTeX[
		BoxData[{
			RowBox[{RowBox[{"f", "::", "usage"}], "=", "\"\<f[] gives something\>\""}],
			"\[IndentingNewLine]",
			RowBox[{
				RowBox[{"f", "[", "]"}], ":=",
				RowBox[{"Module", "[",
					RowBox[{"(*", " ",
						RowBox[{"A", " ", RowBox[{"comment", "."}]}],
					" ", "*)"}],
					RowBox[{
						RowBox[{"{", "x", "}"}], ",",
						RowBox[{
							"\"\<A string\>\"", "<>",
							RowBox[{"ToString", "[", "x", "]"}]
						}]
					}],
				"]"}]
			}]
		}]
		,
		"Style" -> "Input"
	]
	,
	"\
\\begin{mmaCell}[morelocal={x}]{Input}
  f::\\mmaStr{usage}=\"f[] gives something\"
  f[]:=Module[(* A comment. *)\\{x\\},\"A string\"<>ToString[x]]
\\end{mmaCell}"
	,
	TestID -> "BoxData: comment, string, syntax: Input"
]


Test[
	CellToTeX[
		BoxData @ RowBox[{
			StyleBox[RowBox[{"Power", "::", "infy"}], "MessageName"],
			RowBox[{":", " "}],
			"\<\"Infinite expression \\!\\(1\\/0\\) encountered. \\!\\(\\*ButtonBox[\\\">>\\\", ButtonStyle->\\\"Link\\\", ButtonFrame->None, ButtonData:>\\\"paclet:ref/message/General/infy\\\", ButtonNote -> \\\"Power::infy\\\"]\\)\"\>"
		}],
		"Style" -> "Message"
	]
	,
	"\
\\begin{mmaCell}[messagelink=message/General/infy]{Message}
  Power::infy: Infinite expression \\mmaFrac{1}{0} encountered. >>
\\end{mmaCell}"
	,
	TestID -> "BoxData: formatted: Message"
]


Test[
	CellToTeX[
		BoxData[{"a", "\[IndentingNewLine]", "b"}],
		"Style" -> "Output"
	]
	,
	"\
\\begin{mmaCell}{Output}
  a
  b
\\end{mmaCell}"
	,
	TestID -> "BoxData with List: Output"
]


SetOptions[CellToTeX,
	"PreviousIntype" -> False, "CurrentCellIndex" -> Automatic
]
Test[
	CellToTeX[
		Cell[
			BoxData @ MakeBoxes[
				Subscript[x, 1] ==
					(-b \[PlusMinus] Sqrt[b^2 - 4 a c])/(2 a)
			],
			"Input",
			CellLabel -> "In[153]:=",
			ShowCellLabel -> True
		]
	]
	,
	"\
\\begin{mmaCell}{Input}
  \\mmaSub{x}{1}==\\mmaFrac{-b\\(\\pmb{\\pm}\\)\\mmaSqrt{\\mmaSup{b}{2}-4 a c}}{2 a}
\\end{mmaCell}"
	,
	TestID -> "Input cell: formatting, no syntax"
]
Test[
	OptionValue[CellToTeX, "PreviousIntype"]
	,
	True
	,
	TestID -> "Input cell: formatting, no syntax: PreviousIntype"
]
Test[
	OptionValue[CellToTeX, "CurrentCellIndex"]
	,
	153
	,
	TestID -> "Input cell: formatting, no syntax: CurrentCellIndex"
]


SetOptions[CellToTeX, "PreviousIntype" -> False, "CurrentCellIndex" -> 8]
UsingFrontEnd @ Test[
	CellToTeX[
		Cell[
			BoxData[{
				MakeBoxes[Solve[a x + c == 0, x];] /.
					(* Revert change done by make boxes. *)
					"\[Equal]" -> "=="
				,
				"\[IndentingNewLine]",
				Module[{x, a}, x_ = x \[PlusMinus] 1] // MakeBoxes
			}]
			,
			"Input",
			CellLabel -> "In[12]:=",
			ShowCellLabel -> True
		],
		"Style" -> "Code"
	]
	,
	"\
\\begin{mmaCell}[addtoindex=3,morefunctionlocal={x},morelocal={x_}]{Code}
  Solve[a*x + c == 0, x]; 
  Module[{\\mmaLoc{x}, \\mmaLoc{a}}, x_ = \\mmaLoc{x} \\[PlusMinus] 1]
\\end{mmaCell}"
	,
	TestID -> "Code cell: no formatting, different syntax roles, index"
]
Test[
	OptionValue[CellToTeX, "PreviousIntype"]
	,
	True
	,
	TestID ->
		"Code cell: no formatting, different syntax roles: PreviousIntype"
]
Test[
	OptionValue[CellToTeX, "CurrentCellIndex"]
	,
	12
	,
	TestID ->
		"Code cell: no formatting, different syntax roles: CurrentCellIndex"
]


SetOptions[CellToTeX, "PreviousIntype" -> True, "CurrentCellIndex" -> 3]
Test[
	CellToTeX[
		Cell[
			BoxData @ MakeBoxes[x Block[{x}, x/y]],
			"Output",
			CellLabel -> "Out[3]=",
			ShowCellLabel -> True
		]
	]
	,
	"\
\\begin{mmaCell}{Output}
  x Block[\\{x\\},\\mmaFrac{x}{y}]
\\end{mmaCell}"
	,
	TestID -> "Output cell: formatting, different syntax roles"
]
Test[
	OptionValue[CellToTeX, "PreviousIntype"]
	,
	False
	,
	TestID -> "Output cell: formatting, different syntax roles: PreviousIntype"
]	
Test[
	OptionValue[CellToTeX, "CurrentCellIndex"]
	,
	3
	,
	TestID ->
		"Output cell: formatting, different syntax roles: CurrentCellIndex"
]


Block[{boxes, Export = Composition[First, List]},
	Test[
		CellToTeX[
			boxes,
			"Style" -> "Input",
			"Processor" ->
				Composition[mmaCellGraphicsProcessor, exportProcessor]
		]
		,
		"\\mmaCellGraphics{Input}{b3f37ede.pdf}"
		,
		TestID -> "mmaCellGraphics: pure boxes: Input"
	]
]


(* ::Subsection:: *)
(*Exceptions*)


With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::missingCellStyle,
				HoldForm @ CellToTeX[Cell[BoxData["contents"]]],
				HoldForm @ CellsToTeXException["Missing", "CellStyle"],
				HoldForm @ Cell[BoxData["contents"]]
			]
	}
	,
	TestMatch[
		CellToTeX[Cell[BoxData["contents"]]] // normalAssoc
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::missingCellStyle,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Missing", "CellStyle"}
			]
		] // normalAssoc
		,
		{heldMessage}
		,
		TestID -> "Exception: Missing CellStyle: Cell"
	]
]

With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::unsupported,
				HoldForm @ CellToTeX @
					Cell[BoxData["contents"], "testUnsupportedStyle"],
				HoldForm @ CellsToTeXException["Unsupported", "CellStyle"],
				HoldForm @ "CellStyle",
				HoldForm @ "testUnsupportedStyle",
				HoldForm @
					Verbatim["Code" | "Input" | "Output" | "Print" | "Message"]
			]
	}
	,
	TestMatch[
		CellToTeX[Cell[BoxData["contents"], "testUnsupportedStyle"]] //
			normalAssoc
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::unsupported,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Unsupported", "CellStyle"}
			]
		] // normalAssoc
		,
		{heldMessage}
		,
		TestID -> "Exception: Unsupported CellStyle: Cell"
	]
]

With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::unsupported,
				HoldForm @ boxesToTeXProcessor[OptionsPattern[]],
				HoldForm @ CellsToTeXException["Unsupported", "FormatType"],
				HoldForm @ "FormatType",
				HoldForm @ testFormatType,
				HoldForm @ {InputForm, OutputForm}
			]
	}
	,
	TestMatch[
		CellToTeX[
			Cell["contents", "Input"],
			"ProcessorOptions" -> {"FormatType" -> testFormatType}
		] // normalAssoc
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::unsupported,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Unsupported", "FormatType"}
			]
		] // normalAssoc
		,
		{heldMessage}
		,
		TestID -> "Exception: Unsupported FormatType: Cell"
	]
]

With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::unsupported,
				HoldForm @ boxesToTeXProcessor[OptionsPattern[]],
				HoldForm @ CellsToTeXException["Unsupported", "Box"],
				HoldForm @ "Box",
				HoldForm @ testUnsupportedBox,
				HoldForm @ {testSupportedBox[Verbatim[_]], Verbatim[_String]}
			]
	}
	,
	TestMatch[
		CellToTeX[
			BoxData[testUnsupportedBox],
			"Style" -> "Output",
			"ProcessorOptions" -> {"BoxRules" -> {testSupportedBox[b_] :> b}}
		] // normalAssoc
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::unsupported,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Unsupported", "Box"}
			]
		] // normalAssoc
		,
		{heldMessage}
		,
		TestID -> "Exception: Unsupported Box: BoxData"
	]
]

Block[
	{
		defaultAnnotationType = "testDefaultAnnotation"&,
		$annotationTypesToTeX = {
			"testSupportedAnnotation" -> {"key", "command"}
		}
	},
	With[
		{
			heldMessage =
				HoldForm @ Message[CellsToTeXException::unsupported,
					HoldForm @ annotateSyntaxProcessor[OptionsPattern[]],
					HoldForm @ CellsToTeXException[
						"Unsupported", "AnnotationType"
					],
					HoldForm @ "AnnotationType",
					HoldForm @ "UndefinedSymbol",
					HoldForm @ {"testSupportedAnnotation"}
				]
		}
		,
		TestMatch[
			CellToTeX[
				BoxData[RowBox[{"testUndefinedSymbol"}]],
				"Style" -> "Input"
			] // normalAssoc
			,
			Failure[CellsToTeXException,
				Association[
					"MessageTemplate" :> CellsToTeXException::unsupported,
					"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
					"Type" -> {"Unsupported", "AnnotationType"}
				]
			] // normalAssoc
			,
			{heldMessage}
			,
			TestID -> "Exception: Unsupported AnnotationType: BoxData"
		]
	]
]


SetOptions[CellToTeX, "PreviousIntype" -> True, "CurrentCellIndex" -> 10]
With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::unsupported,
				HoldForm @ boxesToTeXProcessor[OptionsPattern[]],
				HoldForm @ CellsToTeXException["Unsupported", "Box"],
				HoldForm @ "Box",
				HoldForm @ testUnsupportedBox,
				HoldForm @ {testSupportedBox, Verbatim[_String]}
			]
	}
	,
	TestMatch[
		CellToTeX[
			Cell[
				BoxData[testUnsupportedBox],
				"Output",
				CellLabel -> "Out[2]=",
				ShowCellLabel -> True
			],
			"ProcessorOptions" -> {"BoxRules" -> {testSupportedBox -> ""}}
		] // normalAssoc
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::unsupported,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Unsupported", "Box"}
			]
		] // normalAssoc
		,
		{heldMessage}
		,
		TestID -> "Exception: Unsupported Box: Cell"
	]
]
Test[
	OptionValue[CellToTeX, "PreviousIntype"]
	,
	True
	,
	TestID -> "Exception: Unsupported Box: Cell: PreviousIntype"
]
Test[
	OptionValue[CellToTeX, "CurrentCellIndex"]
	,
	10
	,
	TestID -> "Exception: Unsupported Box: Cell: CurrentCellIndex"
]


With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::missingProcArg,
				HoldForm @ trackCellIndexProcessor[OptionsPattern[]],
				HoldForm @ CellsToTeXException[
					"Missing", "Keys", "ProcessorArgument"
				],
				HoldForm @ "Keys",
				HoldForm @ {"CellIndex"},
				HoldForm @ {
					"Boxes", "Style", "Processor", "BoxRules", "StringRules",
					"NonASCIIHandler", "CharacterEncoding", "FormatType",
					"TeXCodeSimplifier", "Indexed", "Intype", "CellLabel",
					"SupportedCellStyles", "CellStyleOptions",
					"ProcessorOptions", "TeXOptions", "CatchExceptions",
					"CurrentCellIndex", "PreviousIntype"
				}
			]
	}
	,
	TestMatch[
		CellToTeX[
			Cell[BoxData["contents"], "Print"],
			"Processor" -> trackCellIndexProcessor
		] // normalAssoc
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::missingProcArg,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Missing", "Keys", "ProcessorArgument"}
			]
		] // normalAssoc
		,
		{heldMessage}
		,
		TestID -> "Exception: missing data in Processor"
	]
]


With[
	{
		heldMessage =
			HoldForm @ Message[CellsToTeXException::missingProcRes,
				HoldForm @ CellToTeX[
					Cell[BoxData["contents"], "Message"],
					"Processor" -> Identity
				],
				HoldForm @ CellsToTeXException[
					"Missing", "Keys", "ProcessorResult"
				],
				HoldForm @ "Keys",
				HoldForm @ {"TeXCode"},
				HoldForm @ {
					"Boxes", "Style", "Processor", "BoxRules", "StringRules",
					"NonASCIIHandler", "CharacterEncoding", "FormatType",
					"TeXCodeSimplifier", "Indexed", "Intype", "CellLabel",
					"SupportedCellStyles", "CellStyleOptions",
					"ProcessorOptions", "TeXOptions", "CatchExceptions",
					"CurrentCellIndex", "PreviousIntype"
				},
				HoldForm @ Identity
			]
	}
	,
	Test[
		CellToTeX[
			Cell[BoxData["contents"], "Message"],
			"Processor" -> Identity
		] // normalAssoc
		,
		Failure[CellsToTeXException,
			Association[
				"MessageTemplate" :> CellsToTeXException::missingProcRes,
				"MessageParameters" -> (List @@@ heldMessage)[[1, 2;;]],
				"Type" -> {"Missing", "Keys", "ProcessorResult"}
			]
		] // normalAssoc
		,
		{heldMessage}
		,
		TestID -> "Exception: missing TeXCode in data returned by Processor"
	]
]

(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
