# Cells to TeX

[![releases](https://img.shields.io/github/release/jkuczm/MathematicaCellsToTeX.svg)](https://github.com/jkuczm/MathematicaCellsToTeX/releases)
[![Mathematica 8.0 - 11.0](https://img.shields.io/badge/Mathematica-8.0_--_11.0-brightgreen.svg)](#compatibility)
[![license MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaCellsToTeX/blob/master/LICENSE)
[![SemVer 2.0.0](https://img.shields.io/badge/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)


Convert *Mathematica* cells to TeX, retaining formatting.


* [Description](#description)
* [Installation](#installation)
    * [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
    * [No installation](#no-installation)
* [Compatibility](#compatibility)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [License](#license)
* [Versioning](#versioning)



## Description

This package provides functions converting *Mathematica* cells to specific type
of TeX code, compatible with [mmacells](https://github.com/jkuczm/mmacells)
package. Compilation of this TeX code results in output resembling FrontEnd
appearance of converted cells.

You can find usage examples in
[answer to "How best to embed various cell groups into a latex project?" question](http://mathematica.stackexchange.com/a/73589/14303)
on Mathematica Stack Exchange.



## Installation


### Automatic installation

To install newest version of CellsToTeX package,
in *Mathematica* version 10 or newer, evaluate following code:
```Mathematica
PacletInstall@"http://github.com/jkuczm/MathematicaCellsToTeX/releases/download/v0.2.2/CellsToTeX-0.2.2.paclet"
```

Note that above requires allowing *Mathematica* to use the Internet.

To load CellsToTeX package evaluate:
```Mathematica
Needs@"CellsToTeX`"
```

To uninstall CellsToTeX package evaluate:
```Mathematica
PacletUninstall@"CellsToTeX"
```


### Manual installation

If in your setup *Mathematica* doesn't have Internet access,
or you're using version older than 10, download
[CellsToTeX-0.2.2.paclet](https://github.com/jkuczm/MathematicaCellsToTeX/releases/download/v0.2.2/CellsToTeX-0.2.2.paclet)
file and evaluate `PacletInstall` with path to downloaded file:
```Mathematica
PacletInstall@"path/to/downloaded/CellsToTeX-0.2.2.paclet"
```

To load CellsToTeX package evaluate:
```Mathematica
Needs@"CellsToTeX`"
```

To uninstall CellsToTeX package evaluate:
```Mathematica
PacletUninstall@"CellsToTeX"
```


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Import@"https://raw.githubusercontent.com/jkuczm/MathematicaCellsToTeX/master/NoInstall.m"
```



## Compatibility

This package contains extensive
[automatic test suite](https://github.com/jkuczm/MathematicaCellsToTeX/tree/master/CellsToTeX/Tests).
Package is tested with all *Mathematica* major and minor versions from 8.0 to
11.0 on Linux. Since it doesn't contain any OS specific code it should work
with above versions on all operating systems.

There's also no obvious reason for package not to work on older (6.0+)
and newer (11.1+) versions of *Mathematica*,
but it was not tested with these versions.



## Bugs and requests

If you find any bugs or have feature request please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaCellsToTeX/issues).



## Contributing

Feel free to fork and send pull requests.

All contributions are welcome!



## License

This package is released under
[The MIT License](https://github.com/jkuczm/MathematicaCellsToTeX/blob/master/LICENSE).


### Attribution

Parts of code of this project are a derivative of code written by
[Leonid Shifrin](http://mathematica.stackexchange.com/users/81/leonid-shifrin)
in
[Exception checking and trapping techniques with Throw and Catch](http://mathematica.stackexchange.com/a/635/14303)
thread on Mathematica Stack Exchange, used under
[Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).

Parts of code of this project are a derivative of code written by
[John Fultz](http://mathematica.stackexchange.com/users/309/john-fultz) in
[answer to "How can I get the unchanged Box form of an arbitrary expression?" question](http://mathematica.stackexchange.com/a/13371/14303)
on Mathematica Stack Exchange, used under
[Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).



## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org/).
