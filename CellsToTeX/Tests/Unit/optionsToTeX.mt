(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["CellsToTeX`Tests`Unit`optionsToTeX`", {"MUnit`"}]


Get["CellsToTeX`"]

Get["CellsToTeX`Tests`Utilities`"]

PrependTo[$ContextPath, "CellsToTeX`Internal`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*One argument*)


Test[
	optionsToTeX[{}]
	,
	""
	,
	TestID -> "empty"
]

Test[
	optionsToTeX[{"a" -> "b"}]
	,
	"a=b"
	,
	TestID -> "one rule"
]

Test[
	optionsToTeX[{"c" :> "d"}]
	,
	"c=d"
	,
	TestID -> "one delayed rule"
]

Test[
	optionsToTeX[{"e" -> "f", "g" -> "h"}]
	,
	"e=f,g=h"
	,
	TestID -> "two rules"
]

Test[
	optionsToTeX[{"i" :> "j", "k" :> "l"}]
	,
	"i=j,k=l"
	,
	TestID -> "two delayed rules"
]

Test[
	optionsToTeX[{"m" -> "n", "o" :> "p"}]
	,
	"m=n,o=p"
	,
	TestID -> "rule and delayed rule"
]


Test[
	optionsToTeX[{"label" -> "In[\\mmaCellIndex]:="}]
	,
	"label={In[\\mmaCellIndex]:=}"
	,
	TestID -> "complex value"
]


Test[
	optionsToTeX[{
		"opt1" -> {
			"opt1a" -> "{[a}",
			"opt1b" :> -1,
			"opt1c" -> {"opt1c1" :> False}
		},
		"opt2" -> ""
	}]
	,
	"opt1={opt1a={[a},opt1b=-1,opt1c={opt1c1=false}},opt2="
	,
	TestID -> "nested"
]


(* ::Subsection:: *)
(*pre and post*)


Test[
	optionsToTeX["testPre1", {}, "testPost1"]
	,
	""
	,
	TestID -> "pre and post: empty"
]


Test[
	optionsToTeX["testPre2", {"label" -> "In[\\mmaCellIndex]:="}, "testPost2"]
	,
	"testPre2label={In[\\mmaCellIndex]:=}testPost2"
	,
	TestID -> "pre and post: non-empty"
]


Test[
	optionsToTeX["testPre3", {"name" :> {"subName" -> "val"}}, "testPost3"]
	,
	"testPre3name={subName=val}testPost3"
	,
	TestID -> "pre and post: nested"
]


(* ::Subsection:: *)
(*Incorrect arguments*)


Module[{testArg1, testArg2},
	Test[
		Catch[optionsToTeX[testArg1, testArg2];, _, HoldComplete]
		,
		expectedIncorrectArgsError[optionsToTeX[testArg1, testArg2]]
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
