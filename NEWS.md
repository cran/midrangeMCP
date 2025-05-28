Version 3.1.3
----------------------
- GUI without the dependency on the tkrplot package
- Corrections to the package title

Version 3.1.2
----------------------
- Invisible object inserted in MRwrite function
- Corrections in the GUI
- Created own function for plotting and resizing graphs in tcl/tk language

Version 3.1.1
----------------------
- Bug partially fixed on the tkrplot package. We changed this package from the Imports field to Suggests in DESCRIPTION. But, this needs to be improved.


Version 3.1.0
----------------------
  Corrections in the graphic device in which the 'save-as' button was pressed twice so that the window was opened, when RStudio was being used.
  Corrections to the Plot configuration in the 'save-as' button.
  Corretions to the Plot configuration in the 'Update Plot' button.
  Internationalization of 'Scale of Plot' to the language in Portuguese.
  Internationalization of '...Nor will you need to use quotes between treatment levels...',
part of the message in help button (Average option -> treatment).


Version 3.0.0
----------------------
  Reactivation of the package to CRAN. There was a change in the design of the graphical user interface using the Tcl / tk language, through the tcltk package. The basic settings of the Interface have been preserved.
  However, some features of the interface have been changed, such as its color.
  Another modified feature was the plot area. Some modifications will still be added in the following versions.

Version 2.1.0
----------------------
  Internationalization of the package to the language in Portuguese (Brazil). On   5/21/2020 the midrangeMCP package was removed from the CRAN repository due to a dependency on the gWidgets package. This package has been archived.


Version 2.0.0
----------------------
  Creation of the graphical user interface using the gWidgets package with GTk+ language support (gWidgetsRGtk2). This resulted in the guimidrangeMCP () function.

Version 1.3.0
----------------------
  Removing bugs.

Version 1.2.0
----------------------
  Removing bugs.

Version 1.1.0
----------------------
  Removing bugs.


Version 1.0.0
-----------
  In the first version, we created the basic functions of the `midrangeMCP` package:   `MRtest()`, `MRwrite()` and `MRbarplot()`.
