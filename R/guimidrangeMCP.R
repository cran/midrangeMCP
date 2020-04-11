#' Graphical User Interface for midrangeMCP function
#'
#' \code{guimidrangeMCP} A Graphical User Interface (GUI) for
#'     function that returns the MGM, MGR, SNKM and TM tests
#' @param gui Logical argument, \code{TRUE} or \code{FALSE}. The default is \code{TRUE}
#' @return \code{guimidrangeMCP} presents a GUI for the results of the four multiple
#'       comparison procedures MGM, MGR, SNKM and TM tests. In addition, the GUI
#'       returns a graph of the results, as well as the export of these results
#'       to three types of file extension and latex code.
#' @examples
#' # Loading package
#' library(midrangeMCP)
#' if (interactive()) {
#'   guimidrangeMCP(gui = FALSE)
#' }
#'
#' @import "gWidgets"
#' @importFrom "pacman" "p_load"
#' @importFrom "grDevices" "dev.new"
#' @importFrom "stats" "aov"
#' @export
guimidrangeMCP <- function(gui = TRUE) {
  def.dir <- paste(getwd(), sep = .Platform$file.sep)
  #gsub("\\", "/", def.dir, fixed=TRUE)

  if (gui == TRUE) {
    #nocov start
    if (!pacman::p_exists("gWidgetsRGtk2", local = TRUE)) {
      pacman::p_load("gWidgetsRGtk2")
    }
    options("guiToolkit" = "RGtk2")

    ###################
    # Parent Window
    ###################
    guimidrangeMCP <- gwindow("GUI to the midrangeMCP package", visible = FALSE, width = 965, height = 614)
    #MCP <- gwindow("midrangeMCP", visible = TRUE, expand = TRUE)
    # size(MCP) Current screen dimension

    ###############
    # Overall group
    ###############
    group.all <- gpanedgroup(container = guimidrangeMCP, horizontal = TRUE)

    ##############
    # Child groups
    ##############
    group1 <- gpanedgroup(container = group.all, horizontal = FALSE, expand = TRUE)
    size(group1) <- c(330,600)
    group2 <- gpanedgroup(container = group.all, horizontal = FALSE, expand = TRUE)

    #######################
    # Working child group 1
    #######################

    child1.group1 <- ggroup(container = group1, horizontal = FALSE, expand = TRUE)
    child2.group1 <- ggroup(container = group1, horizontal = FALSE, expand = TRUE)

    # Label
    glabel("Search the data file (.txt or .csv)", container = child1.group1, anchor = c(-1, 0))

    # Auxiliar function
    f.txt <- NULL
    f.txt <- function(file) {
      if (grepl("\\.txt$", svalue(start_dir))) {
        if (svalue(group_cbox_1)) {
          return(read.table(file, header = TRUE, dec = ",", sep = svalue(group_cbox_2)))
        }
        if (svalue(group_cbox_1) == FALSE) {
          return(read.table(file, header = TRUE, sep = svalue(group_cbox_2)))
        }
      }
      if (grepl("\\.csv$", svalue(start_dir))) {
        if (svalue(group_cbox_1)) {
          return(read.table(file, header = TRUE, dec = ",", sep = svalue(group_cbox_2)))
        }
        if (svalue(group_cbox_1) == FALSE) {
          return(read.table(file, header = TRUE, sep = svalue(group_cbox_2)))
        }
      }
    }

    # Open data.frame and save
    start_dir <- gfilebrowse(text = "Select a file ...",
                             quote = FALSE, container = child1.group1,
                             type = c("open"),
                             filter = c("txt"="txt", "csv"="csv"),
                             handler = function(h, ...){
                               dat <- NULL
                               dat2 <- NULL
                               dat2 <<- dat <<- f.txt(svalue(start_dir))
                               #attach(f.txt(svalue(start_dir)))
                               svalue(search_results) <- c("The variables in the file:",
                                                           "===========================",
                                                           svalue(start_dir),
                                                           "===========================",
                                                           names(f.txt(svalue(start_dir))),
                                                           "===========================",
                                                           "R object created: 'dat'")
                               gmessage("Check that the data has been loaded correctly. To do this, use the 'Edit/View' button or the 'Output' frame button.", icon = "info")
                             })

    # Group of buttons
    group_cbox <- ggroup(container = child1.group1, expand = TRUE)

    # Group of checkbox
    group_cbox_1 <- gcheckbox(
      text      = "Comma as decimal points",
      checked   = FALSE,
      container = group_cbox,
      expand = TRUE
    )
    # delete(group_buttons, group_buttons[1,3])

    # Separator
    gseparator(horizontal = FALSE, container = group_cbox, expand = TRUE)

    glabel("Separator of variables:", container = group_cbox, expand = TRUE)
    group_cbox_2 <- gedit("", container = group_cbox, width = 5, expand = TRUE)

    # Group of buttons
    group_buttons <- ggroup(container = child1.group1, expand = TRUE)

    # Button of Edit/View
    dat2 <- NULL # This variable is internal, not exported to the console
    group_buttons_1 <- gbutton("Edit/View", container = group_buttons,
                               handler = function(h, ...){
                                 dat <- NULL
                                 # while (any(search() == "f.txt(svalue(start_dir))")) {
                                 #   detach(f.txt(svalue(start_dir)))
                                 # }
                                 dat2 <<- dat <- dat <<- edit(dat2)
                                 #attach(dat)
                                 #dat2 <<- dat <<- edit(dat2); attach(dat)
                                 #dat2 <- NULL
                                 #dat2 <<- dat
                                 #attach(f.txt(svalue(start_dir)))
                                 #attach(dat)
                               })

    # Button Console
    brow <- NULL
    group_buttons_2 <- console_button <- gbutton("Choosing the directory", container = group_buttons,
                                                 handler = function(h, ...){
                                                   brow <<- gfile(container = group_buttons, type = "selectdir")
                                                   setwd(brow)
                                                 })

    # Help button
    bhelp <- gbutton("help", container = group_buttons,
                     handler = function(h, ...) {
                       gmessage("To insert a data set, the possible extensions are '.txt' and '.csv'. To do this, use the 'browse' button. If you want to identify a directory, before clicking on the 'browse' button, click on the 'Choosing the directory' button. After entering the data set, if you want to edit or view, click on the 'Edit/View' button. If your data has a comma as a decimal point separator, before clicking on the 'browse' button, activate the 'Comma as decimal points' box. Finally, if the variable separator is different from a blank space, indicate it without quotes.", icon = "info")
                     } )

    # Output
    ########

    # Frame
    frame.output <- gframe("Output:", container = child1.group1, horizontal = FALSE, expand = TRUE)
    size(frame.output) <- c(425,200)
    ##
    search_results <- gtext( "" ,container = frame.output, expand = TRUE)


    # Console
    #########

    # Frame
    frame.console <- gframe("Console:", container = child2.group1, expand = TRUE)
    ##
    console <- gcommandline(container = frame.console,
                            expand = TRUE, width = 300)



    # Button Calculate
    ##################

    calculate_button <- gbutton("Calculate", container = child2.group1)
    ##


    addHandlerChanged(calculate_button, handler = function(h, ...){
      results <- NULL
      if (svalue(entry_radio) == "Model") {
        results <- results <<- midrangeMCP::MRtest(y = aov(eval(parse(text = svalue(gm1d))), data = dat),
                                        trt = svalue(treat),
                                        alpha = eval(parse(text = svalue(sl_gedit))),
                                        MCP = svalue(tests_box_group))
        objtreat <- dat[,svalue(treat)]
        plot_group <- midrangeMCP::MRbarplot(results,
                                             col = heat.colors(length(levels(objtreat))))
        if (svalue(entry.exp_radio) == "latex"){
          svalue(console) <- "results; midrangeMCP::MRwrite(x = results, extension = 'latex')"
        }
        if (svalue(entry.exp_radio) != "latex") {
          midrangeMCP::MRwrite(x = results, MCP = "all", extension = svalue(entry.exp_radio), dataMR = "all")
          svalue(console) <- "results"
        }

      }
      if (svalue(entry_radio) == "Response variable") {
        objtreat <- dat[,svalue(grv1d)]
        objrv <- dat[,svalue(grv2d)]
        results <- results <<- midrangeMCP::MRtest(y = objtreat,
                                                   trt = objrv,
                                                   dferror = eval(parse(text = svalue(grv3d))),
                                                   mserror = eval(parse(text = svalue(grv4d))),
                                                   alpha = eval(parse(text = svalue(sl_gedit))),
                                                   MCP = svalue(tests_box_group))
        plot_group <- midrangeMCP::MRbarplot(results,
                                        col = heat.colors(length(levels(objrv))))
        if (svalue(entry.exp_radio) == "latex"){
          svalue(console) <- "results; midrangeMCP::MRwrite(results, extension = 'latex')"
        } else {
          midrangeMCP::MRwrite(results, extension = svalue(entry.exp_radio))
          svalue(console) <- "results"
        }
      }
      if (svalue(entry_radio) == "Averages") {
        # if (!is.factor(eval(parse(text = svalue(gme2d))))){
        #   gmessage("The trt argument must be factor")
        # }
        # Mean vector
        aver <- paste("c(", svalue(gme1d), ")")
        # Treatment levels
        trat <- strsplit(svalue(gme2d),split = ",", perl = TRUE)[[1]]
        trat <- as.factor(trat)

        # Functions
        results <- results <<- midrangeMCP::MRtest(y = eval(parse(text = aver)),
                                        trt = trat,
                                        dferror = eval(parse(text = svalue(gme3d))),
                                        mserror = eval(parse(text = svalue(gme4d))),
                                        alpha = eval(parse(text = svalue(sl_gedit))),
                                        MCP = svalue(tests_box_group),
                                        replication = eval(parse(text = svalue(gme5d))),
                                        ismean = TRUE)
        plot_group <- midrangeMCP::MRbarplot(results,
                                       col = heat.colors(length(levels(trat))))
        if (svalue(entry.exp_radio) == "latex"){
          svalue(console) <- "results; midrangeMCP::MRwrite(results, extension = 'latex')"
        } else {
          midrangeMCP::MRwrite(results, extension = svalue(entry.exp_radio))
          svalue(console) <- "results"
        }
      }
    })




    #######################
    # Working child group 2
    #######################

    child1.group2 <- ggroup(container = group2, horizontal = FALSE, expand = TRUE)
    child2.group2 <- ggroup(container = group2, horizontal = FALSE, expand = TRUE)
    #delete(group2, child2.group2)



    # Input
    #######

    # Frame
    frame.input <- gframe("Input", container = child1.group2, horizontal = FALSE, expand = TRUE)

    # Group.input.top
    group.input.top <- ggroup(horizontal = TRUE, container = frame.input, expand = TRUE)
    ##
    glabel("Tests:", container = group.input.top, anchor = c(-1, 0))
    glabel( container = group.input.top, anchor = c(-1, 0))
    entry.exp <- c("MGM", "MGR", "SNKM", "TM")
    tests_box_group <- gcombobox(entry.exp, horizontal = TRUE,
                                 selected = , container = group.input.top, anchor = c(-1,0))
    bgit <- gbutton("help", container = group.input.top,
                    handler = function(h, ...) {
                      gmessage("Choose the test for data analysis. The Mean Grouping test based on Midrange (MGM test) and Mean Grouping test based on Range (MGR test) are tests without ambiguity in their results. These tests are similar to the Scott-Knott test. The Tukey Midrange test (TM test) and SNK Midrange test (SNKM test) are tests similar to the Tukey and SNK tests, respectively. these tests are also based on midrange.", icon = "info")
                    } )
    # Separator
    gseparator(horizontal = FALSE, container = group.input.top, expand = TRUE)

    # Export
    ########

    glabel("Extension: ", container = group.input.top, anchor = c(1, 0))
    entry.exp <- c("latex", "txt", "xlsx", "csv")
    entry.exp_radio <- gcombobox(entry.exp, horizontal = TRUE,
                                 selected = 4, container = group.input.top)
    bexp <- gbutton("help", container = group.input.top,
                    handler = function(h, ...) {
                      gmessage("Choose the type of extension for the data output file. If 'latex', the code will be exported to the Console frame. The remaining options will be exported to the selected directory. The choice of the directory can be made in the 'Choosing the directory' button.", icon = "info")
                    } )

    # Separator
    gseparator(horizontal = TRUE, container = frame.input, expand = TRUE)

    group.entry <- ggroup(container = frame.input, horizontal = TRUE, expand = TRUE)

    # Data Entry Options
    ####################
    glabel("Data Entry Options:", container = group.entry, anchor = c(-1, 0))
    ##
    entry <- c("Model", "Response variable", "Averages")
    ##
    entry_radio <- gcombobox(entry, horizontal = TRUE,
                             selected = 1, container = group.entry, handler = function(h,..) {
                               if(svalue(h$obj) == "Model"){
                                 enabled(group.model) <- TRUE
                                 enabled(group.rv) <- FALSE
                                 enabled(groupmeans) <- FALSE
                               }
                               if(svalue(h$obj) == "Response variable"){
                                 enabled(group.model) <- FALSE
                                 enabled(group.rv) <- TRUE
                                 enabled(groupmeans) <- FALSE
                               }
                               if(svalue(h$obj) == "Averages"){
                                 enabled(group.model) <- FALSE
                                 enabled(group.rv) <- FALSE
                                 enabled(groupmeans) <- TRUE
                               }
                             })
    bentry <- gbutton("help", container = group.entry,
                    handler = function(h, ...) {
                      gmessage("Choose the type of data entry. If 'Model', enter the experimental model. If 'Response Variable', enter the object name of the responses and treatments variables and if 'Averages', enter the vector of means and treatments. For more details, use the help button on each option.", icon = "info")
                    } )

    # Separator
    gseparator(horizontal = FALSE, container = group.entry, expand = TRUE)

    # Significance level
    glabel("Significance level:", container = group.entry, anchor = c(1, 0))
    sl_gedit <- gedit("0.05", container = group.entry, width = 5)
    bsl <- gbutton("help", container = group.entry,
                      handler = function(h, ...) {
                        gmessage("Enter the value of the significance level. This value is a number between 0 and 1.", icon = "info")
                      } )


    gMRV <- gpanedgroup(container = child2.group2, expand = TRUE)
    g1 <- ggroup(container = gMRV, horizontal = FALSE, expand = TRUE)
    size(g1) <- c(300,500)
    g2 <- ggroup(container = gMRV, horizontal = FALSE, expand = TRUE)
    size(g2) <- c(100,300)

    # Option 'Model'
    group.model <- gframe("Model", horizontal = FALSE, container = g1, expand = TRUE)
    ##
    gm1 <- ggroup(container = group.model, expand = TRUE)
    glabel("Enter model:", container = gm1, expand = TRUE)
    gm1d <- gedit("", container = gm1, initial.msg = "RV ~ PV", expand = TRUE)
    bm1d <- gbutton("help", container = gm1, expand = TRUE,
                    handler = function(h, ...) {
                      gmessage("Enter the experimental model of type Response Variable (RV) ~ Predictive Variables (PV). These variables are in the 'Output' frame, after entering the data set. For example, in a randomized block design, assuming the 'treat' object corresponding to the treatments, 'block' object corresponding to the blocks and 'resp' object corresponding to the variable response. So, you must enter the following expression: resp ~ trat + block.", icon = "info")
                    } )
    ##
    gm2 <- ggroup(container = group.model, expand = TRUE)
    glabel("Treatment:", container = gm2)
    treat <- gedit("", container = gm2,
                   initial.msg = "Enter label name...", expand = TRUE)
    bm2d <- gbutton("help", container = gm2, expand = TRUE,
                    handler = function(h, ...) {
                      gmessage("Enter the name of the treatments in the experiment model inserted above, in Predictive Variables (PV). The name of the treatment are in the 'Output' frame, after entering the data set. Inserted all the arguments above, click on the 'Calculate' button.", icon = "info")
                    } )

    # Option 'Response variable'
    group.rv <- gframe("Response variable", horizontal = FALSE, container = g1, expand = TRUE)
    #delete(grv2, grv2d)
    ##
    grv1 <- ggroup(container = group.rv, horizontal = TRUE, expand = TRUE)
    glabel("Response:", container = grv1)
    grv1d <- gedit("", container = grv1, initial.msg = "Enter label name...", expand = TRUE)
    bgrv1d <- gbutton("help", container = grv1, expand = TRUE,
                    handler = function(h, ...) {
                      gmessage("Insert the name of the variable response in the experiment model. The name of the response variable are in the 'Output' frame, after inserting the data set.", icon = "info")
                    } )

    ##
    grv2 <- ggroup(container = group.rv, horizontal = TRUE, expand = TRUE)
    glabel("Treatment:", container = grv2)
    grv2d <- gedit("", container = grv2,
                   initial.msg = "Enter label name...", expand = TRUE)
    bgrv2d <- gbutton("help", container = grv2, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the name of the variable response of the experiment model. The name of the response variable are in the 'Output' frame, after entering the data set.", icon = "info")
                      } )

    ##
    grv3 <- ggroup(container = group.rv, horizontal = TRUE, expand = TRUE)
    glabel("DFerror:", container = grv3)
    grv3d <- gedit("", container = grv3,
                   initial.msg = "Enter numeric value...", expand = TRUE)
    bgrv3d <- gbutton("help", container = grv3, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the numeric value of the degrees of freedom of the mean square error of the experiment model. The value is numeric.", icon = "info")
                      } )

    ##
    grv4 <- ggroup(container = group.rv, horizontal = TRUE, expand = TRUE)
    glabel("MSerror:", container = grv4)
    grv4d <- gedit("", container = grv4,
                   initial.msg = "Enter numeric value...", expand = TRUE)
    bgrv3d <- gbutton("help", container = grv4, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the value of the mean square error of the experiment model. The value is numeric. Inserted all the arguments above, click on the 'Calculate' button", icon = "info")
                      } )


    # Option 'Averages'
    groupmeans <- gframe("Averages", horizontal = FALSE, container = g1, expand = TRUE)
    #delete(g1, group.means)
    gme1 <- ggroup(container = groupmeans, horizontal = TRUE, expand = TRUE)
    glabel("Averages:", container = gme1)
    gme1d <- gedit("", container = gme1,
                   initial.msg = "Enter the vector...", expand = TRUE)
    bgme1d <- gbutton("help", container = gme1, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the the values of the averages. Each mean of the vector must be separated by a comma. For example, for the vector of the average of four treatments: 10, 20, 30, 40. You do not need to use the concatenate function, i.e., c().", icon = "info")
                      } )

    ##
    gme2 <- ggroup(container = groupmeans, horizontal = TRUE, expand = TRUE)
    glabel("Treatment:", container = gme2)
    gme2d <- gedit("", container = gme2,
                   initial.msg = "Enter the vector...", expand = TRUE)
    bgme1d <- gbutton("help", container = gme2, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the treatment levels of the treatment. For example, for a character vector of four treatments: A, B, C, D. You do not need to use the concatenate function, i.e., c().", icon = "info")
                      } )

    ##
    gme3 <- ggroup(container = groupmeans, horizontal = TRUE, expand = TRUE)
    glabel("DFerror:", container = gme3)
    gme3d <- gedit("", container = gme3,
                   initial.msg = "Enter the numeric value...", expand = TRUE)
    bme3d <- gbutton("help", container = gme3, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the value of the degrees of freedom of the mean square error of the experiment model. The value is numeric.", icon = "info")
                      } )
    ##
    gme4 <- ggroup(container = groupmeans, horizontal = TRUE, expand = TRUE)
    glabel("MSerror:", container = gme4)
    gme4d <- gedit("", container = gme4,
                   initial.msg = "Enter the numeric value...", expand = TRUE)
    bgme4d <- gbutton("help", container = gme4, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the value of the mean square error of the experiment model. The value is numeric.", icon = "info")
                      } )
    ##
    gme5 <- ggroup(container = groupmeans, horizontal = TRUE, expand = TRUE)
    glabel("Replication:", container = gme5)
    gme5d <- gedit("", container = gme5,
                   initial.msg = "Enter the numeric value...", expand = TRUE)
    bgme5d <- gbutton("help", container = gme5, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the value of number of replications of the treatments. The value is numeric and if the data is unbalanced use the harmonic mean of the replications. Inserted all the arguments above, click on the 'Calculate' button", icon = "info")
                      } )




    # Layout of buttons
    # addSpring(group.model)
    # addSpring(group.means)
    # addSpring(group.rv)

    # Desable
    enabled(groupmeans) <- FALSE
    enabled(group.rv) <- FALSE


    # Plot
    ######

    # Frames
    frame.plot <- gframe("Graphic Parameters", container = g1,
                         horizontal = FALSE, expand = TRUE)
    frame.graf <- gframe("Plot", container = g2, expand = TRUE, horizontal = FALSE)

    # Groups
    ggraf1 <- ggroup(container = frame.plot, horizontal = TRUE, expand = TRUE)
    ##
    glabel("Color:", container = ggraf1)
    ggraf1d <- gedit("", container = ggraf1,
                     initial.msg = "or gray.colors() or...", expand = TRUE)
    bgraf1d <- gbutton("help", container = ggraf1, expand = TRUE,
                      handler = function(h, ...) {
                        gmessage("Enter the color name of the chart bars. Place quotation marks around the color names. For example, if you want the color red for the bars, use 'red'. For functions, quotation marks are not necessary, for example gray.colors() function.", icon = "info")
                      } )
    ##
    ggraf2 <- ggroup(container = frame.plot, horizontal = TRUE, expand = TRUE)
    ##
    glabel("Horizontal:", container = ggraf2)
    ggraf2d <- gedit("", container = ggraf2,
                     initial.msg = "FALSE or TRUE...", expand = TRUE)
    bgraf2d <- gbutton("help", container = ggraf2, expand = TRUE,
                       handler = function(h, ...) {
                         gmessage("Choose horizontal or vertical bars (FALSE or TRUE).", icon = "info")
                       } )
    ###
    ggraf3 <- ggroup(container = frame.plot, horizontal = TRUE, expand = TRUE)
    glabel("Axes", container = ggraf3)
    ggraf3d <- gedit("", container = ggraf3,
                     initial.msg = "xlab = ...; ylab = ...",
                     expand = TRUE)
    bgraf3d <- gbutton("help", container = ggraf3, expand = TRUE,
                       handler = function(h, ...) {
                         gmessage("Enter the axes. Separate them by semicolons. To add the names on the X and Y axes: xlab = 'Label X-axix'; ylab = 'Label Y-axis'.", icon = "info")
                       } )
    ##
    plot_button <- gbutton("Update plot", container = g1, expand = TRUE)
    addHandlerChanged(plot_button, handler = function(h, ...) {
      xlab <- NULL; ylab <- NULL
      hor <- if (is.null(eval(parse(text = svalue(ggraf2d))))) FALSE else eval(parse(text = svalue(ggraf2d)))
      color <- if (is.null(eval(parse(text = svalue(ggraf1d))))) heat.colors(length(levels(eval(parse(text = svalue(treat)))))) else eval(parse(text = svalue(ggraf1d)))
      eval(parse(text = svalue(ggraf3d)))
      plot_group <<- midrangeMCP::MRbarplot(results,
                                            col = color,
                                            horiz = hor,
                                            xlab = xlab,
                                            ylab = ylab
      )
      #visible(plot_group) <- TRUE
    })

    # Plot toolbar
    #--------------
    aCopy <-  gaction(label = "Copy", icon = "copy",  handler = function(...) print("Maria"))
    aSaveas <- gaction(label = "Save as...", icon = "save-as",  handler = function(h, ...) {
      dev.new()
      if (svalue(ggraf1d) == "") color <- heat.colors(length(levels(eval(parse(text = svalue(treat))))))
      if (svalue(ggraf1d) != "") color <- eval(parse(text = svalue(ggraf1d)))
      if (svalue(ggraf2d) == "") hor <- FALSE
      if (svalue(ggraf2d) != "") hor <- eval(parse(text = svalue(ggraf2d)))
      xlab <- NULL; ylab <- NULL
      eval(parse(text = svalue(ggraf3d)))
      midrangeMCP::MRbarplot(results, col = color, horiz = hor, xlab = xlab, ylab = ylab)
    })
    ##
    #tbl <- list(copy = aCopy, saveas = aSaveas)
    tbl <- list(saveas = aSaveas)
    plot_toolbar <- gtoolbar(tbl, container = frame.graf)
    ##


    ##
    graphics.off() # Erasing All Graphics Devices
    plot_group <- ggraphics(container = frame.graf, expand = TRUE)

    #delete(frame.plot, plot_group)
    #size(plot_group) <- c(50,340)


    ##########
    # Messages
    ##########
    close <- addHandlerUnrealize(guimidrangeMCP, handler = function(h , ...) {
      # while (any(search() == "dat")) {
      #   detach(dat)
      # }
      # while (any(search() == "dat2")) {
      #   detach(dat2)
      # }
      # while (any(search() == "f.txt(svalue(start_dir))")) {
      #   detach(f.txt(svalue(start_dir)))
      # }
      setwd(def.dir)
      svalue(console) <- "pcreatobj <- c('brow', 'dat2', 'dat', 'results');
                          for(i in pcreatobj){if (!any(ls() == i)) pcreatobj <- pcreatobj[-which(pcreatobj == i)]}
                          rm(list = c(pcreatobj,'pcreatobj', 'i'), envir = .GlobalEnv)"
      !gconfirm("Really close", parent = h$obj)
    })
    ##
    # GUI visible
    visible(guimidrangeMCP) <- TRUE
  }
  if (gui == FALSE) {
    print("Use the MRtest function! For help, use ?MRtest.")
  }
}


