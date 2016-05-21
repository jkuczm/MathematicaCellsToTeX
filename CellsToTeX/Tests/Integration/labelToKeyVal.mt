(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Integration`labelToKeyVal`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]



(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*No label*)


Test[
	labelToKeyVal[None, Automatic, Automatic, Automatic]
	,
	{{}, Automatic}
	,
	TestID -> "None, Automatic, Automatic, Automatic"
]
Test[
	labelToKeyVal[None, Automatic, Automatic, 999]
	,
	{{}, 999}
	,
	TestID -> "None, Automatic, Automatic, Integer"
]

Test[
	labelToKeyVal[None, Automatic, None, Automatic]
	,
	{{}, Automatic}
	,
	TestID -> "None, Automatic, None, Automatic"
]
Test[
	labelToKeyVal[None, Automatic, "InputForm", Automatic]
	,
	{{"form" -> "InputForm"}, Automatic}
	,
	TestID -> "None, Automatic, String, Automatic"
]

Test[
	labelToKeyVal[None, None, Automatic, Automatic]
	,
	{{}, Automatic}
	,
	TestID -> "None, None, Automatic, Automatic"
]
Test[
	labelToKeyVal[None, None, Automatic, 999]
	,
	{{}, 999}
	,
	TestID -> "None, None, Automatic, Integer"
]
Test[
	labelToKeyVal[None, 1, Automatic, Automatic]
	,
	{{}, 1}
	,
	TestID -> "None, Integer, Automatic, Automatic"
]
Test[
	labelToKeyVal[None, 2, Automatic, 2]
	,
	{{}, 2}
	,
	TestID -> "None, Integer, Automatic, Integer same"
]
Test[
	labelToKeyVal[None, 3, Automatic, 999]
	,
	{{"addtoindex" -> -996}, 3}
	,
	TestID -> "None, Integer, Automatic, Integer different"
]


(* ::Subsection:: *)
(*custom label*)


Test[
	labelToKeyVal["custom", Automatic, Automatic, Automatic]
	,
	{{"label" -> "custom"}, Automatic}
	,
	TestID -> "custom, Automatic, Automatic, Automatic"
]
Test[
	labelToKeyVal["custom", Automatic, Automatic, 999]
	,
	{{"label" -> "custom"}, 999}
	,
	TestID -> "custom, Automatic, Automatic, Integer"
]

Test[
	labelToKeyVal["custom", Automatic, None, Automatic]
	,
	{{"label" -> "custom"}, Automatic}
	,
	TestID -> "custom, Automatic, None, Automatic"
]
Test[
	labelToKeyVal["custom", Automatic, "InputForm", Automatic]
	,
	{{"label" -> "custom", "form" -> "InputForm"}, Automatic}
	,
	TestID -> "custom, Automatic, String, Automatic"
]

Test[
	labelToKeyVal["custom", None, Automatic, Automatic]
	,
	{{"label" -> "custom"}, Automatic}
	,
	TestID -> "custom, None, Automatic, Automatic"
]
Test[
	labelToKeyVal["custom", None, Automatic, 999]
	,
	{{"label" -> "custom"}, 999}
	,
	TestID -> "custom, None, Automatic, Integer"
]
Test[
	labelToKeyVal["custom", 1, Automatic, Automatic]
	,
	{{"label" -> "custom"}, 1}
	,
	TestID -> "custom, Integer, Automatic, Automatic"
]
Test[
	labelToKeyVal["custom", 2, Automatic, 2]
	,
	{{"label" -> "custom"}, 2}
	,
	TestID -> "custom, Integer, Automatic, Integer same"
]
Test[
	labelToKeyVal["custom", 3, Automatic, 999]
	,
	{{"label" -> "custom", "addtoindex" -> -996}, 3}
	,
	TestID -> "custom, Integer, Automatic, Integer different"
]


(* ::Subsection:: *)
(*In label*)


Test[
	labelToKeyVal["In[1]:=", Automatic, Automatic, 999]
	,
	{{"addtoindex" -> -998}, 1}
	,
	TestID -> "In, Automatic, Automatic, Integer different"
]
Test[
	labelToKeyVal["In[2]:=", Automatic, Automatic, 2]
	,
	{{}, 2}
	,
	TestID -> "In, Automatic, Automatic, Integer same"
]
Test[
	labelToKeyVal["In[2]:=", Automatic, Automatic, Automatic]
	,
	{{}, 2}
	,
	TestID -> "In, Automatic, Automatic, Automatic"
]

Test[
	labelToKeyVal["In[5]:=", None, Automatic, 999]
	,
	{{"label" -> "In[5]:="}, 999}
	,
	TestID -> "In, None, Automatic, Integer different"
]
Test[
	labelToKeyVal["In[6]:=", None, Automatic, 6]
	,
	{{"label" -> "In[6]:="}, 6}
	,
	TestID -> "In, None, Automatic, Integer same"
]
Test[
	labelToKeyVal["In[6]:=", None, Automatic, Automatic]
	,
	{{"label" -> "In[6]:="}, Automatic}
	,
	TestID -> "In, None, Automatic, Automatic"
]

Test[
	labelToKeyVal["In[7]:=", Automatic, None, 999]
	,
	{{"addtoindex" -> -992}, 7}
	,
	TestID -> "In, Automatic, None, Integer different"
]
Test[
	labelToKeyVal["In[8]:=", Automatic, None, 8]
	,
	{{}, 8}
	,
	TestID -> "In, Automatic, None, Integer same"
]
Test[
	labelToKeyVal["In[8]:=", Automatic, None, Automatic]
	,
	{{}, 8}
	,
	TestID -> "In, Automatic, None, Automatic"
]

Test[
	labelToKeyVal["In[13]:=", None, None, 999]
	,
	{{"label" -> "In[13]:="}, 999}
	,
	TestID -> "In, None, None, Integer different"
]
Test[
	labelToKeyVal["In[14]:=", None, None, 14]
	,
	{{"label" -> "In[14]:="}, 14}
	,
	TestID -> "In, None, None, Integer same"
]
Test[
	labelToKeyVal["In[14]:=", None, None, Automatic]
	,
	{{"label" -> "In[14]:="}, Automatic}
	,
	TestID -> "In, None, None, Automatic"
]

Test[
	labelToKeyVal["In[19]:=", 5, Automatic, 999]
	,
	{{"label" -> "In[19]:=", "addtoindex" -> -994}, 5}
	,
	TestID -> "In different, Integer different, Automatic, Integer different"
]
Test[
	labelToKeyVal["In[20]:=", 20, Automatic, 999]
	,
	{{"addtoindex" -> -979}, 20}
	,
	TestID -> "In same, Integer same, Automatic, Integer different"
]
Test[
	labelToKeyVal["In[999]:=", 21, Automatic, 21]
	,
	{{"label" -> "In[999]:="}, 21}
	,
	TestID -> "In different, Integer same, Automatic, Integer same"
]
Test[
	labelToKeyVal["In[999]:=", 21, Automatic, Automatic]
	,
	{{"label" -> "In[999]:="}, 21}
	,
	TestID -> "In different, Integer different, Automatic, Automatic"
]
Test[
	labelToKeyVal["In[38]:=", 38, Automatic, Automatic]
	,
	{{}, 38}
	,
	TestID -> "In same, Integer same, Automatic, Automatic"
]

Test[
	labelToKeyVal["In[22]:=", Automatic, "FullForm", 999]
	,
	{{"label" -> "In[22]:=", "addtoindex" -> -977, "form" -> "FullForm"}, 22}
	,
	TestID -> "In, Automatic, String, Integer different"
]
Test[
	labelToKeyVal["In[23]:=", Automatic, "FullForm", 23]
	,
	{{"label" -> "In[23]:=", "form" -> "FullForm"}, 23}
	,
	TestID -> "In, Automatic, String, Integer same"
]
Test[
	labelToKeyVal["In[23]:=", Automatic, "FullForm", Automatic]
	,
	{{"label" -> "In[23]:=", "form" -> "FullForm"}, 23}
	,
	TestID -> "In, Automatic, String, Automatic"
]


(* ::Subsection:: *)
(*Out label*)


Test[
	labelToKeyVal["Out[1]=", Automatic, Automatic, 999]
	,
	{{"addtoindex" -> -998}, 1}
	,
	TestID -> "Out, Automatic, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[2]=", Automatic, Automatic, 2]
	,
	{{}, 2}
	,
	TestID -> "Out, Automatic, Automatic, Integer same"
]
Test[
	labelToKeyVal["Out[2]=", Automatic, Automatic, Automatic]
	,
	{{}, 2}
	,
	TestID -> "Out, Automatic, Automatic, Automatic"
]

Test[
	labelToKeyVal["Out[5]=", None, Automatic, 999]
	,
	{{"label" -> "Out[5]="}, 999}
	,
	TestID -> "Out, None, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[6]=", None, Automatic, 6]
	,
	{{"label" -> "Out[6]="}, 6}
	,
	TestID -> "Out, None, Automatic, Integer same"
]
Test[
	labelToKeyVal["Out[6]=", None, Automatic, Automatic]
	,
	{{"label" -> "Out[6]="}, Automatic}
	,
	TestID -> "Out, None, Automatic, Automatic"
]

Test[
	labelToKeyVal["Out[7]=", Automatic, None, 999]
	,
	{{"addtoindex" -> -992}, 7}
	,
	TestID -> "Out, Automatic, None, Integer different"
]
Test[
	labelToKeyVal["Out[8]=", Automatic, None, 8]
	,
	{{}, 8}
	,
	TestID -> "Out, Automatic, None, Integer same"
]
Test[
	labelToKeyVal["Out[8]=", Automatic, None, Automatic]
	,
	{{}, 8}
	,
	TestID -> "Out, Automatic, None, Automatic"
]

Test[
	labelToKeyVal["Out[13]=", None, None, 999]
	,
	{{"label" -> "Out[13]="}, 999}
	,
	TestID -> "Out, None, None, Integer different"
]
Test[
	labelToKeyVal["Out[14]=", None, None, 14]
	,
	{{"label" -> "Out[14]="}, 14}
	,
	TestID -> "Out, None, None, Integer same"
]
Test[
	labelToKeyVal["Out[14]=", None, None, Automatic]
	,
	{{"label" -> "Out[14]="}, Automatic}
	,
	TestID -> "Out, None, None, Automatic"
]

Test[
	labelToKeyVal["Out[19]=", 5, Automatic, 999]
	,
	{{"label" -> "Out[19]=", "addtoindex" -> -994}, 5}
	,
	TestID -> "Out different, Integer different, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[20]=", 20, Automatic, 999]
	,
	{{"addtoindex" -> -979}, 20}
	,
	TestID -> "Out same, Integer same, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[999]=", 21, Automatic, 21]
	,
	{{"label" -> "Out[999]="}, 21}
	,
	TestID -> "Out different, Integer same, Automatic, Integer same"
]
Test[
	labelToKeyVal["Out[999]=", 21, Automatic, Automatic]
	,
	{{"label" -> "Out[999]="}, 21}
	,
	TestID -> "Out different, Integer different, Automatic, Automatic"
]
Test[
	labelToKeyVal["Out[38]=", 38, Automatic, Automatic]
	,
	{{}, 38}
	,
	TestID -> "Out same, Integer same, Automatic, Automatic"
]

Test[
	labelToKeyVal["Out[22]=", Automatic, "FullForm", 999]
	,
	{{"label" -> "Out[22]=", "addtoindex" -> -977, "form" -> "FullForm"}, 22}
	,
	TestID -> "Out, Automatic, String, Integer different"
]
Test[
	labelToKeyVal["Out[23]=", Automatic, "FullForm", 23]
	,
	{{"label" -> "Out[23]=", "form" -> "FullForm"}, 23}
	,
	TestID -> "Out, Automatic, String, Integer same"
]
Test[
	labelToKeyVal["Out[23]=", Automatic, "FullForm", Automatic]
	,
	{{"label" -> "Out[23]=", "form" -> "FullForm"}, 23}
	,
	TestID -> "Out, Automatic, String, Automatic"
]


(* ::Subsection:: *)
(*Out label with form*)


Test[
	labelToKeyVal["Out[1]//FullForm=", Automatic, Automatic, 999]
	,
	{{"addtoindex" -> -998, "form" -> "FullForm"}, 1}
	,
	TestID -> "Out//form, Automatic, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[2]//FullForm=", Automatic, Automatic, 2]
	,
	{{"form" -> "FullForm"}, 2}
	,
	TestID ->
		"Out//form, Automatic, Automatic, Integer same"
]
Test[
	labelToKeyVal["Out[2]//FullForm=", Automatic, Automatic, Automatic]
	,
	{{"form" -> "FullForm"}, 2}
	,
	TestID -> "Out//form, Automatic, Automatic, Automatic"
]

Test[
	labelToKeyVal["Out[5]//FullForm=", None, Automatic, 999]
	,
	{{"label" -> "Out[5]//FullForm=", "form" -> "FullForm"}, 999}
	,
	TestID -> "Out//form, None, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[6]//FullForm=", None, Automatic, 6]
	,
	{{"label" -> "Out[6]//FullForm=", "form" -> "FullForm"}, 6}
	,
	TestID -> "Out//form, None, Automatic, Integer same"
]
Test[
	labelToKeyVal["Out[6]//FullForm=", None, Automatic, Automatic]
	,
	{{"label" -> "Out[6]//FullForm=", "form" -> "FullForm"}, Automatic}
	,
	TestID -> "Out//form, None, Automatic, Automatic"
]

Test[
	labelToKeyVal["Out[7]//FullForm=", Automatic, None, 999]
	,
	{{"label" -> "Out[7]//FullForm=", "addtoindex" -> -992}, 7}
	,
	TestID -> "Out//form, Automatic, None, Integer different"
]
Test[
	labelToKeyVal["Out[8]//FullForm=", Automatic, None, 8]
	,
	{{"label" -> "Out[8]//FullForm="}, 8}
	,
	TestID -> "Out//form, Automatic, None, Integer same"
]
Test[
	labelToKeyVal["Out[8]//FullForm=", Automatic, None, Automatic]
	,
	{{"label" -> "Out[8]//FullForm="}, 8}
	,
	TestID -> "Out//form, Automatic, None, Automatic"
]

Test[
	labelToKeyVal["Out[13]//FullForm=", None, None, 999]
	,
	{{"label" -> "Out[13]//FullForm="}, 999}
	,
	TestID -> "Out//form, None, None, Integer different"
]
Test[
	labelToKeyVal["Out[14]//FullForm=", None, None, 14]
	,
	{{"label" -> "Out[14]//FullForm="}, 14}
	,
	TestID -> "Out//form, None, None, Integer same"
]
Test[
	labelToKeyVal["Out[14]//FullForm=", None, None, Automatic]
	,
	{{"label" -> "Out[14]//FullForm="}, Automatic}
	,
	TestID -> "Out//form, None, None, Automatic"
]

Test[
	labelToKeyVal["Out[19]//FullForm=", 5, Automatic, 999]
	,
	{
		{
			"label" -> "Out[19]//FullForm=",
			"addtoindex" -> -994,
			"form" -> "FullForm"
		},
		5
	}
	,
	TestID ->
		"Out//form different, Integer different, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[20]//FullForm=", 20, Automatic, 999]
	,
	{{"addtoindex" -> -979, "form" -> "FullForm"}, 20}
	,
	TestID -> "Out//form same, Integer same, Automatic, Integer different"
]
Test[
	labelToKeyVal["Out[999]//FullForm=", 21, Automatic, 21]
	,
	{{"label" -> "Out[999]//FullForm=", "form" -> "FullForm"}, 21}
	,
	TestID -> "Out//form different, Integer same, Automatic, Integer same"
]
Test[
	labelToKeyVal["Out[999]//FullForm=", 21, Automatic, Automatic]
	,
	{{"label" -> "Out[999]//FullForm=", "form" -> "FullForm"}, 21}
	,
	TestID -> "Out//form different, Integer different, Automatic, Automatic"
]
Test[
	labelToKeyVal["Out[38]//FullForm=", 38, Automatic, Automatic]
	,
	{{"form" -> "FullForm"}, 38}
	,
	TestID -> "Out//form same, Integer same, Automatic, Automatic"
]

Test[
	labelToKeyVal["Out[22]//FullForm=", Automatic, "FullForm", 999]
	,
	{{"addtoindex" -> -977, "form" -> "FullForm"}, 22}
	,
	TestID -> "Out//form, Automatic, String same, Integer different"
]
Test[
	labelToKeyVal["Out[23]//FullForm=", Automatic, "FullForm", 23]
	,
	{{"form" -> "FullForm"}, 23}
	,
	TestID -> "Out//form, Automatic, String same, Integer same"
]
Test[
	labelToKeyVal["Out[23]//FullForm=", Automatic, "FullForm", Automatic]
	,
	{{"form" -> "FullForm"}, 23}
	,
	TestID -> "Out//form, Automatic, String same, Automatic"
]

Test[
	labelToKeyVal["Out[24]//FullForm=", Automatic, "TraditionalForm", 999]
	,
	{
		{
			"label" -> "Out[24]//FullForm=",
			"addtoindex" -> -975,
			"form" -> "TraditionalForm"
		},
		24
	}
	,
	TestID -> "Out//form, Automatic, String different, Integer different"
]
Test[
	labelToKeyVal["Out[25]//FullForm=", Automatic, "TraditionalForm", 25]
	,
	{{"label" -> "Out[25]//FullForm=", "form" -> "TraditionalForm"}, 25}
	,
	TestID -> "Out//form, Automatic, String different, Integer same"
]
Test[
	labelToKeyVal[
		"Out[25]//FullForm=", Automatic, "TraditionalForm", Automatic
	]
	,
	{{"label" -> "Out[25]//FullForm=", "form" -> "TraditionalForm"}, 25}
	,
	TestID -> "Out//form, Automatic, String different, Automatic"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg},
	Test[
		Catch[labelToKeyVal[testArg];, _, HoldComplete]
		,
		expectedIncorrectArgsError[labelToKeyVal[testArg]]
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
