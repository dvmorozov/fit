# fit

The curve fitting application. It allows to model data by set of curves of selected type.

## Projects

- Fit - builds ready-to-use desktop application containing client and server part in single executable.
- FitPro - builds client desktop application. The application should be configured to access server. 
Server side should be compiled and installed separately. It is supposed but it is not required that
backend is installed on dedicated machine.
- FitServer - builds computing backend.
- ClientProxy - builds shared library implementing XML-RPC calls from client to server.

All applications are built from the same code base by following compilation keys:
- FIT - controls building of monolithic desktop application. Is mutually exclusive with FITPRO.
- FITPRO - incorporates parts responsible for network communications.
- FITSERVER - incorporates backend code.
- FITCLIENT - incorporates client code.
- SERVER - controls building of server application.

## How to build desktop applications Fit and FitPro

### Install latest version of Lazarus

[Lazarus](https://www.lazarus-ide.org/index.php?page=downloads)

Latest built was done with

[Lazarus 1.8.4](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%201.8.4/lazarus-1.8.4-fpc-3.0.4-win64.exe/download)

### Install components

[Ararat Synapse](http://www.ararat.cz/synapse/doku.php/download)

### Checkout components from GitHub

[fitgrids/master](https://github.com/dvmorozov/fitgrids/tree/master)

[fitminimizers/master](https://github.com/dvmorozov/fitminimizers/tree/master)

Install components into Lazarus, add included directories in project properties and rebuild projects.

## How to build CGI client, server application and proxy shared library

### Install Lazarus 0.9.24

Unfortunately wst-0.5 can not be built without modifications with latest versions of Lazarus.
So, it is recommended to use version 0.9.24.

[Lazarus 0.9.24](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Old%20releases/Lazarus%200.9.24/Lazarus-0.9.24-fpc-2.2.0-20071114-win32.exe/download)

### Checkout components from GitHub

[fitgrids/lazarus-0-9-24](https://github.com/dvmorozov/fitgrids/tree/lazarus-0-9-24)

[fitminimizers/lazarus-0-9-24](https://github.com/dvmorozov/fitminimizers/tree/lazarus-0-9-24)

### Include files

[wst-0.5](https://osdn.net/projects/sfnet_lazarus-ccr/downloads/Web%20Service%20Toolkit/Web%20Service%20Toolkit%200.5/wst-0.5.zip/)

Add included directories in project properties and rebuild projects.

## More info

[fit on GitHub](https://dvmorozov.github.io/fit/)

[Documentation](https://dvmorozov.github.io/fit/doc/index.html)

## UML files

To open and modify diagram files use my favourite [UMLet](https://www.umlet.com/). 
