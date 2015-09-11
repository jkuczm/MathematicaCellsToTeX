(* ::Package:: *)

Get["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.1/BootstrapInstaller.m"]


BootstrapInstall[
	"CellsToTeX",
	"https://github.com/jkuczm/MathematicaCellsToTeX/releases/download/v0.1.2/CellsToTeX.zip"
	,
	{{
		"SyntaxAnnotations",
		"https://github.com/jkuczm/MathematicaSyntaxAnnotations/releases/download/v0.1.3/SyntaxAnnotations.zip"
	}}
	,
	"AdditionalFailureMessage" ->
		Sequence[
			"You can ",
			Hyperlink[
				"install CellsToTeX package manually",
				"https://github.com/jkuczm/MathematicaCellsToTeX#manual-installation"
			],
			"."
		]
]
