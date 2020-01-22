# fit

The curve fitting application. It allows to model data by set of curves of selected type.

Fully automatical mode is available.

The application is written in Free Pascal with Lazarus (Delphi is not supported) and is working under Windows and Linux (potentially could be compiled for Mac, but this was not done yet).

## Look how fully automated fitting works

{% include video_fully_automated_fitting.html %}

## Open architecture

[How to add new data loader](how-to-extend-data-loaders)

[How to extend curve types](how-to-extend-curve-types)

To open and modify diagram files use my favourite [UMLet](https://www.umlet.com/).

## Desktop application

![Fitting results](assets/images/2018-12-23_13h17_55.png)

### Executables

There is no installer. The main executable is Fit.exe.

#### Win64

[Fit](https://github.com/dvmorozov/fit/raw/master/Desktop/o/x86_64-win64/Fit-x86_64-win64.exe)
[MathExpr](https://github.com/dvmorozov/fit/raw/master/Desktop/o/x86_64-win64/MathExpr.dll)

#### Win32

[Fit](https://github.com/dvmorozov/fit/raw/master/Desktop/o/i386-win32/Fit-i386-win32.exe)
[MathExpr](https://github.com/dvmorozov/fit/raw/master/Desktop/o/i386-win32/MathExpr.dll)

On Windows MathExpr.dll should be put in the same directory as Fit.exe (this enables experimental feature of curves having shapes defined by arbitrary expression, but it is not supported under Linux for now).

#### Linux-x86_64

[Fit](https://github.com/dvmorozov/fit/raw/master/Desktop/o/x86_64-linux/Fit-x86_64-linux)

### Command line parameters

/INFILE=*file_name* - opens data file on application start up 

## Web application

[FitEasily](fiteasily.html)

## More info

[Autogenerated documentation](doc/index.html)

[Autogenerated class diagram](doc/GVClasses.png)

[Autogenerated module diagram](doc/GVUses.png)

To generate documentation [PasDoc](https://github.com/pasdoc/pasdoc/wiki) is used.

## Related projects

[fitminimizers](https://dvmorozov.github.io/fitminimizers/)

[fitgrids](https://dvmorozov.github.io/fitgrids/)

{% include contacts.html %}

{% include counter.html %}
