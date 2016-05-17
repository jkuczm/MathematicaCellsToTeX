(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`fullNotebooks`", {"MUnit`"}]


Get["CellsToTeX`"]

$ContextPath =
	Join[{"CellsToTeX`Configuration`", "CellsToTeX`Backports`"}, $ContextPath]


testNb =
	Notebook[{Cell[
		CellGroupData[
			{
				Cell[
					BoxData @ MakeBoxes[
						Solve[
							a \[Chi]1^2 + \[Beta] \[Chi]1 + \[Gamma] == 0,
							\[Chi]1
						]
					],
					"Input",
					CellLabel -> "In[1]:="
				]
				,
				Cell[
					BoxData @ MakeBoxes[{
						{
							\[Chi]1 ->
								(-\[Beta] - Sqrt[\[Beta]^2 - 4 a \[Gamma]]) /
								(2 a)
						},
						{
							\[Chi]1 ->
								(-\[Beta] + Sqrt[\[Beta]^2 - 4 a \[Gamma]]) /
								(2 a)
						}
					}],
					"Output",
					CellLabel -> "Out[1]="
				]
			},
			Open
		]
	]}]


(* ::Section:: *)
(*Tests*)


Test[
	Clear[texMathReplacement];
	SetOptions[CellToTeX, "CurrentCellIndex" -> Automatic];
	StringJoin @ Riffle[
		Prepend[
			Cases[testNb, cell : Cell[_, __] :> CellToTeX[cell], Infinity],
			CellsToTeXPreamble[]
		],
		"\n\n"
	]
	,
	"\
\\mmaSet{morefv={gobble=2}}

\\begin{mmaCell}{Input}
  Solve[a \\mmaSup{\\mmaFnc{\\(\\pmb{\\chi}\\)1}}{2}+\\mmaUnd{\\(\\pmb{\\beta}\\)} \\mmaFnc{\\(\\pmb{\\chi}\\)1}+\\mmaUnd{\\(\\pmb{\\gamma}\\)}==0,\\mmaFnc{\\(\\pmb{\\chi}\\)1}]
\\end{mmaCell}

\\begin{mmaCell}{Output}
  \\{\\{\\(\\chi\\)1\\(\\to\\)\\mmaFrac{-\\(\\beta\\)-\\mmaSqrt{\\mmaSup{\\(\\beta\\)}{2}-4 a \\(\\gamma\\)}}{2 a}\\},\\{\\(\\chi\\)1\\(\\to\\)\\mmaFrac{-\\(\\beta\\)+\\mmaSqrt{\\mmaSup{\\(\\beta\\)}{2}-4 a \\(\\gamma\\)}}{2 a}\\}\\}
\\end{mmaCell}"
	,
	TestID -> "Default"
]


Block[{$cellStyleOptions = $cellStyleOptions, Export = #1&},
	UsingFrontEnd @ Test[
		$cellStyleOptions =
			Join[
				{
					{Except["Code"], "Processor"} ->
						Composition[
							trackCellIndexProcessor, mmaCellGraphicsProcessor,
							exportProcessor, cellLabelProcessor,
							extractCellOptionsProcessor
						]
				},
				$cellStyleOptions
			];
		Clear[texMathReplacement];
		SetOptions[CellToTeX, "CurrentCellIndex" -> Automatic];
		StringJoin @ Riffle[
			Prepend[
				Cases[testNb,
					Cell[boxes_, style_, opts___] :>
						CellToTeX @ Cell[
							boxes, Replace[style, "Input" -> "Code"], opts
						],
					Infinity
				],
				CellsToTeXPreamble[]
			],
			"\n\n"
		]
		,
		"\
\\mmaSet{morefv={gobble=2}}

\\begin{mmaCell}{Code}
  Solve[a*\\mmaFnc{\\[Chi]1}^2 + \\mmaUnd{\\[Beta]}*\\mmaFnc{\\[Chi]1} + \\mmaUnd{\\[Gamma]} == 0, \\mmaFnc{\\[Chi]1}]
\\end{mmaCell}

\\mmaCellGraphics{Output}{756954b2.pdf}"
		,
		TestID -> "InputForm, pdf"
	]
]



Block[{$stringsToTeX = $stringsToTeX},
	Test[
		Clear[texMathReplacement];
		SetOptions[CellToTeX, "CurrentCellIndex" -> Automatic];
		PrependTo[$stringsToTeX, "\[Equal]" -> "=="];
		StringJoin @ Riffle[
			Prepend[
				Cases[testNb,
					cell : Cell[_, __] :>
						CellToTeX[cell, "ProcessorOptions" -> {
						"NonASCIIHandler" -> texMathReplacementRegister
						}]
					,
					Infinity
				],
				CellsToTeXPreamble[]
			],
			"\n\n"
		]
		,
	"\
\\mmaSet{morefv={gobble=2}}
\\mmaDefineMathReplacement{\[Beta]}{\\beta}
\\mmaDefineMathReplacement{\[Gamma]}{\\gamma}
\\mmaDefineMathReplacement{\[Chi]}{\\chi}
\\mmaDefineMathReplacement{\[Rule]}{\\to}

\\begin{mmaCell}{Input}
  Solve[a \\mmaSup{\\mmaFnc{\[Chi]1}}{2}+\\mmaUnd{\[Beta]} \\mmaFnc{\[Chi]1}+\\mmaUnd{\[Gamma]}==0,\\mmaFnc{\[Chi]1}]
\\end{mmaCell}

\\begin{mmaCell}{Output}
  \\{\\{\[Chi]1\[Rule]\\mmaFrac{-\[Beta]-\\mmaSqrt{\\mmaSup{\[Beta]}{2}-4 a \[Gamma]}}{2 a}\\},\\{\[Chi]1\[Rule]\\mmaFrac{-\[Beta]+\\mmaSqrt{\\mmaSup{\[Beta]}{2}-4 a \[Gamma]}}{2 a}\\}\\}
\\end{mmaCell}"
		,
		TestID -> "Math replacements"
	]
]


Test[
	Clear[texMathReplacement];
	SetOptions[CellToTeX, "CurrentCellIndex" -> Automatic];
	StringJoin @ Riffle[
		Prepend[
			Cases[testNb,
				cell : Cell[_, __] :>
					CellToTeX[cell, "ProcessorOptions" -> {
						"CommonestTypesAsTeXOptions" -> False,
						"StringBoxToTypes" -> {Automatic},
						"NonASCIIHandler" -> Identity
					}]
				,
				Infinity
			],
			CellsToTeXPreamble["UseListings" -> False]
		],
		"\n\n"
	]
	,
	"\
\\mmaSet{uselistings=false,morefv={gobble=2}}

\\begin{mmaCell}{Input}
  Solve[\\mmaUnd{a} \\mmaSup{\\mmaFnc{\[Chi]1}}{2}+\\mmaUnd{\[Beta]} \\mmaFnc{\[Chi]1}+\\mmaUnd{\[Gamma]}\[Equal]0,\\mmaFnc{\[Chi]1}]
\\end{mmaCell}

\\begin{mmaCell}{Output}
  \\{\\{\[Chi]1\[Rule]\\mmaFrac{-\[Beta]-\\mmaSqrt{\\mmaSup{\[Beta]}{2}-4 a \[Gamma]}}{2 a}\\},\\{\[Chi]1\[Rule]\\mmaFrac{-\[Beta]+\\mmaSqrt{\\mmaSup{\[Beta]}{2}-4 a \[Gamma]}}{2 a}\\}\\}
\\end{mmaCell}"
	,
	TestID -> "No listings"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
