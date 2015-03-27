(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`labelToCellData`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*In label*)


Test[
	labelToCellData["In[176]:="]
	,
	{In, 176, None}
	,
	TestID -> "In label"
]

Test[
	labelToCellData["In[1]//FullForm:="]
	,
	{None, None, None}
	,
	TestID -> "In label: with form"
]

Test[
	labelToCellData["In[1]="]
	,
	{None, None, None}
	,
	TestID -> "In label: no colon"
]

Test[
	labelToCellData["In[123a]:="]
	,
	{None, None, None}
	,
	TestID -> "In label: non-digit index"
]


(* ::Subsection:: *)
(*Out label*)


Test[
	labelToCellData["Out[21]="]
	,
	{Out, 21, None}
	,
	TestID -> "Out label"
]

Test[
	labelToCellData["Out[x3]="]
	,
	{None, None, None}
	,
	TestID -> "Out label: non-digit index"
]

Test[
	labelToCellData["Out[35]:="]
	,
	{None, None, None}
	,
	TestID -> "Out label: colon equal"
]

Test[
	labelToCellData["Out[231]//FullForm="]
	,
	{Out, 231, "FullForm"}
	,
	TestID -> "Out label: with form"
]


(* ::Subsection:: *)
(*custom label*)


Test[
	labelToCellData["custom"]
	,
	{None, None, None}
	,
	TestID -> "custom label"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[labelToCellData[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[labelToCellData[testArg1, testArg2]]
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
