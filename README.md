# Cells to TeX

[![releases](http://img.shields.io/github/release/jkuczm/MathematicaCellsToTeX.svg)](https://github.com/jkuczm/MathematicaCellsToTeX/releases)
[![SemVer 2.0.0](http://img.shields.io/badge/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)
[![license MIT](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaCellsToTeX/blob/master/LICENSE)
[![Mathematica 8.0 - 11.0](http://img.shields.io/badge/Mathematica-8.0 -- 11.0-brightgreen.svg)](#compatibility)


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

To install CellsToTeX package evaluate:
```Mathematica
Import["https://raw.githubusercontent.com/jkuczm/MathematicaCellsToTeX/master/BootstrapInstall.m"]
```

Note that this will also install dependency package
[SyntaxAnnotations](https://github.com/jkuczm/MathematicaSyntaxAnnotations)
and
[ProjectInstaller](https://github.com/lshifr/ProjectInstaller)
package, if you don't have it already installed.

To load CellsToTeX package evaluate: ``Needs["CellsToTeX`"]``.


### Manual installation

1. Download latest released
   [CellsToTeX.zip](https://github.com/jkuczm/MathematicaCellsToTeX/releases/download/v0.2.1/CellsToTeX.zip)
   file.

2. Extract downloaded `CellsToTeX.zip` to any directory which is on
   *Mathematica* `$Path`, e.g. to one obtained by evaluating
   `FileNameJoin[{$UserBaseDirectory,"Applications"}]`.

3. Install dependency:
   [SyntaxAnnotations](https://github.com/jkuczm/MathematicaSyntaxAnnotations).

4. To load the package evaluate: ``Needs["CellsToTeX`"]``


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Import["https://raw.githubusercontent.com/jkuczm/MathematicaCellsToTeX/master/NoInstall.m"]
```



## Compatibility

This package contains extensive
[automatic test suite](https://github.com/jkuczm/MathematicaCellsToTeX/tree/master/CellsToTeX/Tests).
Package is tested with all *Mathematica* major and minor versions from 8.0 to
11.0 on Linux. Since it doesn't contain any OS specific code it should work
with above versions on all operating systems.

There's also no obvious reason for package not to work on earlier (6.0+)
versions of *Mathematica*.



## Bugs and requests

If you find any bugs or have feature request please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaCellsToTeX/issues).



## Contributing

Feel free to fork and send pull requests.

If you want to use Ant scripts from this repository you will also need to
install [WWBCommon](https://github.com/jkuczm/WWBCommon) project.

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
