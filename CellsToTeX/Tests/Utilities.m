(* ::Package:: *)


BeginPackage["CellsToTeX`Tests`Unit`Utilities`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Usage messages*)


mockFunction::usage =
"\
mockFunction[sym, log, body] \
assigns down value to given symbol sym such that when sym[...] is evaluated
it appends all arguments to given log and returns result of evaluation of \
given body."


expectedIncorrectArgsError::usage =
"\
expectedIncorrectArgsError[functionCall] \
returns HoldComplete with value and tag expected to be thrown by given \
function call."


(* ::Section:: *)
(*Implementation*)


addIncorrectArgsDefinition /@ Names["`*"]
	

Begin["`Private`"]


Needs["CellsToTeX`"]
PrependTo[$ContextPath, "CellsToTeX`Backports`"]

Needs["SyntaxAnnotations`"]


(* ::Subsection:: *)
(*mockFunction*)


SetAttributes[mockFunction, HoldAll]

mockFunction[sym_Symbol, log_Symbol, body_] := (
	sym[args___] := (
		AppendTo[log, HoldComplete[args]];
		body
	)
)


(* ::Subsection:: *)
(*expectedIncorrectArgsError*)


SetAttributes[expectedIncorrectArgsError, HoldAllComplete]

expectedIncorrectArgsError[functionCall_] :=
	HoldComplete @@ {
		Failure[CellsToTeXException["Error", "IncorrectArguments"],
			Association[
				"MessageTemplate" :> CellsToTeXException::error,
				"MessageParameters" -> {
					HoldForm @ functionCall,
					HoldForm @
						CellsToTeXException["Error", "IncorrectArguments"]
				}
			]
		],
		CellsToTeXException["Error", "IncorrectArguments"]
	}


(* ::Section:: *)
(*Package Epilogue*)


End[]


Protect["`*"]


EndPackage[]
