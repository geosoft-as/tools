# Tools

Tools for building and maintaining the GeoSoft software.



### Setup

Capture the tools module to local disk by:

```
$ git clone https://<user>[:<password>]@github.com/geosoft-as/tools --branch <branch-name>
```



### Make ###

[Make](https://www.gnu.org/software/make) s a tool which controls the generation of
executables and other non-source files of a program from the program's source files.

The present make setup consists of Makefiles on three levels examplified by:

* `cc/src/com/rabbagast/cc/event/Makefile` - List source of this package
* `cc/Makefile` - List packages of this module + module jar dependencies
* `tools/Make/Makefile` - Main makefile containing build logic - Not normally touched!

On Linux the standard GNU/Make should work (not recently tested), and
on MS/Windows the present mingw version (renamed to gnumake-<version>) should be used.

Make depends on a few environment variables:

* `DEV_HOME` - Pointing to the development folder.
* `JAVA_HOME` - Pointing to the JDK version to use.
* `IS_UNIX = true` - If on *nix platform. Ignore on MS/Windows.

All modules can be built from their root folder by commands like

```
$ make clean
$ make
$ make jar
```

This will invoke the Makefile in the module root which in turns invoke
the main Makefile (found here). The make process loops over all package
folders which in turn invoke the leaf node makefiles.

Make can also be invoked at the package level:

```
$ make clean
$ make
$ make SomeClass.class
```

As a convenience for quick feature testing at the class level, a main entry
(say `Main = DlisFileReader`) can be included in a package makefile
so that the corresponding main() method in that class can be run by:

```
$ make run
```

Modules that should be executable programs are typcailly built with:

```
$ make exe
$ make setup
$ make install
```

etc.

Documentation for both Java and .Net projects are built by:

```
$ make doc
$ make brand
$ make analytics
```

In addition there might be various tools that can be conveniently executed
through main, see the present Makefile.



### JUnit ###

TODO


### Doxygen ###

Doxygen is for making API documentation on .Net projects.
Build .Net documentation by:

```
$ $(DEV_ROOT)/tools/Doxygen/doxygen.exe
```

or simply:

```
$ make doc
```



### Launch4J ###

[Launch4](Jhttp://launch4j.sourceforge.net/) is a system fro creating an MS/Windows
executable (.exe file) from an executable Jar file and its dependecies.

Laucnh4J depends on a `launch4j.xml` setup file in the project root folder.

The executable can be conveniently created by `make` through:

```
$ make exe
```



### Inno Setup ###

[Inno Setup](http://www.jrsoftware.org) is a system for creating MS/Windows
installer files from a set of application files.

Inno Setup depends on a `setup.iss` setup file in the project root folder.

The setup executable can be conveniently created by `make` through:

```
$ make setup
```



### ProGuard ###

TODO



### cloc ###

[cloc](https://github.com/AlDanial/cloc) is a tool for counting number of lines
of code (LOC) in a subtree.

cloc can be conveniently invoked by `make` through:

```
$ make loc
```

