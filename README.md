
<!--
[![CRAN status](https://www.r-pkg.org/badges/version/midrangeMCP)](https://CRAN.R-project.org/package=midrangeMCP)
[![](https://cranlogs.r-pkg.org/badges/midrangeMCP?color=orange)](https://cran.r-project.org/package=midrangeMCP)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Travis build status](https://travis-ci.com/bendeivide/midrangeMCP.svg?branch=master)](https://travis-ci.com/bendeivide/midrangeMCP)[![R build status](https://github.com/bendeivide/midrangeMCP/workflows/R-CMD-check/badge.svg)](https://github.com/bendeivide/midrangeMCP/actions)
-->

[![CRAN
status](https://www.r-pkg.org/badges/version/midrangeMCP)](https://CRAN.R-project.org/package=midrangeMCP)
[![](https://cranlogs.r-pkg.org/badges/midrangeMCP?color=orange)](https://cran.r-project.org/package=midrangeMCP)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--12--15-yellowgreen.svg)](https://github.com/bendeivide/midrangeMCP/commit/master)
[![codecov](https://codecov.io/gh/bendeivide/midrangeMCP/branch/master/graph/badge.svg)](https://codecov.io/gh/bendeivide/midrangeMCP)
[![Build
Status](https://travis-ci.com/bendeivide/midrangeMCP.svg?branch=master)](https://travis-ci.com/bendeivide/midrangeMCP)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-maturing-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#maturing)

# midrangeMCP <img src='man/figures/logo.png' align="right" height="139" />

[midrangeMCP](https://bendeivide.github.io/midrangeMCP/) is an “R”
package designed to present four new multiple comparison tests. This
work was developed during the doctorate in Statistics and Agricultural
Experimentation, by the [Federal University of Lavras](https://ufla.br/)
(UFLA/BRAZIL). The supervisor of this work was Professor [Daniel Furtado
Ferreira](http://www.dex.ufla.br/~danielff/). I am currently as
Professor of Statistics at the [Federal University of São João
del-Rei](https://ufsj.edu.br/) (UFSJ/BRAZIL). Today, we have the
collaboration of a student of scientific initiation, [Diego Arthur Bispo
Justino de Oliveira](https://digoarthur.github.io/), student of
Mechatronics Engineering at UFSJ.

Four tests are presented, three based on the [distribution of the
externally studentized
midrange](https://www.scielo.br/scielo.php?script=sci_abstract&pid=S1413-70542017000400378&lng=en&nrm=iso&tlng=pt)
and one based on the distribution of the externally studented range,
well documented distribution in the literature. There are many
distributions based on this last distribution, the difference for our
case is that it was used for a test based on grouping methods for
multiple comparisons, something similar to what was done with the
Scott-Knot test.

The tests based on the distribution of the externally studentized
midrange are the Tukey Midrange test (TM test), the SNK Midrange test
(SNKM test) and the Means Grouping based on Midrange test (MGM test).
Finally, the test based on the distribution of the externally
studentized range which is the Means Grouping based on the Range test
(MGR test).

Finally, a versatility of the package is the use of the GUI. This
feature provides users unfamiliar with the R language when using the
[midrangeMCP](https://CRAN.R-project.org/package=midrangeMCP) package.

These tests are available in the
[thesis](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwixqp_O4u3sAhU7EbkGHUTUCOMQFjAAegQIAxAC&url=http%3A%2F%2Frepositorio.ufla.br%2Fjspui%2Fbitstream%2F1%2F11466%2F2%2FTESE_Testes%2520de%2520compara%25C3%25A7%25C3%25B5es%2520m%25C3%25BAltiplas%2520baseados%2520na%2520distribui%25C3%25A7%25C3%25A3o%2520da%2520midrange%2520estudentizada%2520externamente.pdf&usg=AOvVaw1N5jvuEggxmsxhq9GnbCqT)
of the work, and their performance evaluations are presented. The
articles of these are in the submission stage, one of which has already
been approved by the [Journal of Science and
Agrotechnology](https://www.scielo.br/scielo.php?pid=1413-7054&script=sci_serial).

## Instalation

To install the
[midrangeMCP](https://CRAN.R-project.org/package=midrangeMCP) package
via CRAN:

``` r
install.packages("midrangeMCP")
```

To install via GitHub:

``` r
install.packages("devtools")
install_github("bendeivide/midrangeMCP")
```

## Functions

The package’s functions are: `MRbarplot`, `MRwrite`, `guimidrangeMCP`
and `MRtest`. The latter is the main function. From the arguments
inserted in it, the other functions can be performed. In the case of the
`guimidrangeMCP` function, once executed, it will open the GUI. From
there the database and the necessary arguments for the functions can be
entered.

## Citation

The package article is still being submitted. But the works mentioned
above can assist in citing the package. Still, as the midrangeMCP
package is available on CRAN, quote it this way:

    #> Warning in citation("midrangeMCP"): no date field in DESCRIPTION file of package
    #> 'midrangeMCP'
    #> Warning in citation("midrangeMCP"): could not determine year for 'midrangeMCP'
    #> from package DESCRIPTION file
    #> 
    #> To cite package 'midrangeMCP' in publications use:
    #> 
    #>   Ben Deivide and Daniel Furtado (NA). midrangeMCP: Multiples
    #>   Comparisons Procedures Based on Studentized Midrange and Range
    #>   Distributions. https://bendeivide.github.io/midrangeMCP/,
    #>   https://github.com/bendeivide/midrangeMCP.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {midrangeMCP: Multiples Comparisons Procedures Based on Studentized Midrange and Range Distributions},
    #>     author = {Ben Deivide and Daniel Furtado},
    #>     note = {https://bendeivide.github.io/midrangeMCP/, https://github.com/bendeivide/midrangeMCP},
    #>   }

## Website

For a complete description of the `midrangeMCP` package, **visit our
website at
[bendeivide.github.io/midrangeMCP/](https://bendeivide.github.io/midrangeMCP/)**

## News

  - Version 3.1 (2020-06-29): Update GUI (`guimidrangeMCP` function).
    Corrections in the graphic device in which the ‘save-as’ button was
    pressed twice so that the window was opened, when RStudio was being
    used. Corrections to the Plot configuration in the ‘save-as’ button.
    Corretions to the Plot configuration in the ‘Update Plot’ button.
    Internationalization of ‘Scale of Plot’ to the language in
    Portuguese. Internationalization of ‘…Nor will you need to use
    quotes between treatment levels…’, part of the message in help
    button (Average option -\> treatment).

  - Version 3.0 (2020-06-23): Reactivation of the package to CRAN. There
    was a change in the design of the graphical user interface using the
    Tcl/tk language, through the tcltk package. The basic settings of
    the Interface have been preserved. However, some features of the
    interface have been changed, such as its color. Another modified
    feature was the plot area. Some modifications will still be added in
    the following versions.

  - Version 2.1 (2020-04-14): Internationalization of the package to the
    language in Portuguese (Brazil). On 5/21/2020 the
    [midrangeMCP](https://CRAN.R-project.org/package=midrangeMCP)
    package was removed from the CRAN repository due to a dependency on
    the [gWidgets](https://CRAN.R-project.org/package=gWidgets) package.
    This package has been archived.

  - Version 2.0 (2020-04-11): Creation of the graphical user interface
    using the [gWidgets](https://CRAN.R-project.org/package=gWidgets)
    package with GTk+ language support
    ([gWidgetsRGtk2](https://CRAN.R-project.org/package=gWidgetsRGtk2)).
    This resulted in the `guimidrangeMCP` function.

  - Version 1.3 (2016-07-14): Removing bugs.

  - Version 1.2 (2016-04-28): Removing bugs.

  - Version 1.1 (2016-02-21): Removing bugs.

  - Version 1.0 (2015-11-05): In the first version, we created the basic
    functions of the
    [midrangeMCP](https://cran.r-project.org/package=midrangeMCP)
    package: `MRtest`, `MRwrite` and `MRbarplot`.
