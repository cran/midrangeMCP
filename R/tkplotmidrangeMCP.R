#' @importFrom "grDevices" "dev.off" "png"
tkplotmidrangeMCP <- function(results, MCP = "all",
                              col = grDevices::heat.colors(10),
                              horiz = FALSE, ...) {
  # Main Window
  base <- tktoplevel(padx=10, pady=10)
  tkwm.geometry(base, "600x400")
  # Change title using tk window manager
  tkwm.title(base,'Plot')
  # Main Window Frame
  main_frame <- tkframe(base, relief="sunken", borderwidth = 1)

  # Canvas
  canvas <- tkcanvas(main_frame)
  # Building
  tkpack(main_frame, canvas, fill = "both", expand=TRUE)


  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)



    # Graph geometry
    height <- as.numeric(tclvalue(tkwinfo("height", canvas)))
    width <- as.numeric(tclvalue(tkwinfo("width", canvas)))

    # Temp file
    fp <- tempfile(pattern = "midrangeMCPMCPtests.",
                   tmpdir = tempdir(),
                   fileext = ".png")

    #op <- par(oma=c(5,7,1,1))

    # Create Image
    png(
      filename = fp,
      width = width,
      height = height,
      units = "px"
    )

    # omit error message which is due to the graphics device,
    # but this is not damaging the image
    try(midrangeMCP::MRbarplot(results, MCP, col, horiz, ...), silent = TRUE)


    #par(op)

    dev.off()

    tkimage.create("photo", "::image::imgteste", file = fp)

    # Set image to element
    #tkpack.forget(canvas)

    # Create a new canvas with the new image
    #new_canvas <- tkcanvas(main_frame)
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste")

    # Pack new canvas and copy ID to global variable
    # tkpack(new_canvas, fill = "both", expand = TRUE)
    # canvas <- new_canvas
    options(warn = oldw)
  }


  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }

  # Window Event Binds
  tkbind(base, '<Configure>', onResize)

  #tkwait.window(main_frame)
}
# Usa-se como janela
tkplotmidrangeMCP2 <- function(parent, results, MCP = "all",
                              col = grDevices::heat.colors(10),
                              horiz = FALSE, ...) {
  # Canvas
  canvas <- tkcanvas(parent)
  # Building
  tkpack(canvas, fill = "both", expand=TRUE)

  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)



    # Graph geometry
    height <- as.numeric(tclvalue(tkwinfo("height", canvas)))
    width <- as.numeric(tclvalue(tkwinfo("width", canvas)))

    # Temp file
    fp2 <- tempfile(pattern = "midrangeMCPMCPtests2.",
                   tmpdir = tempdir(),
                   fileext = ".png")

    #op <- par(oma=c(5,7,1,1))

    # Create Image
    png(
      filename = fp2,
      width = width,
      height = height,
      units = "px"
    )

    # omit error message which is due to the graphics device,
    # but this is not damaging the image
    try(midrangeMCP::MRbarplot(results, MCP, col, horiz, ...), silent = TRUE)


    #par(op)

    dev.off()

    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Set image to element
    #tkpack.forget(canvas)

    # Create a new canvas with the new image
    #new_canvas <- tkcanvas(main_frame)
    tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")

    # Pack new canvas and copy ID to global variable
    # tkpack(new_canvas, fill = "both", expand = TRUE)
    # canvas <- new_canvas
    options(warn = oldw)
  }

  drawGraph()
  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }

  # Window Event Binds
  tkbind(parent, '<Configure>', onResize)

  #tkwait.window(parent)
}

# Usar o widget canvas, a imagem do grafico so aparece de imediato se o pai
# for uma janela toplevel. Para resolver esse problema, ao inves de um canvas
# usamos um label e inserimos a imagem dentro dela
tkplotmidrangeMCP3 <- function(parent, results, MCP = "all",
                               col = grDevices::heat.colors(10),
                               horiz = FALSE, ...) {
  # Label
  canvas <- tklabel(parent)
  # Building
  tkpack(canvas, fill = "both", expand=TRUE)

  drawGraph <- function() {
    oldw <- getOption("warn")
    options(warn = -1)



    # Graph geometry
    height <- as.numeric(tclvalue(tkwinfo("height", canvas)))
    width <- as.numeric(tclvalue(tkwinfo("width", canvas)))

    # Temp file
    fp2 <- tempfile(pattern = "midrangeMCPMCPtests2.",
                    tmpdir = tempdir(),
                    fileext = ".png")

    #op <- par(oma=c(5,7,1,1))

    # Create Image
    png(
      filename = fp2,
      width = width,
      height = height,
      units = "px"
    )

    # omit error message which is due to the graphics device,
    # but this is not damaging the image
    try(midrangeMCP::MRbarplot(results, MCP, col, horiz, ...), silent = TRUE)


    #par(op)

    dev.off()

    tkimage.create("photo", "::image::imgteste2", file = fp2)

    # Set image to element
    #tkpack.forget(canvas)

    # Create a new canvas with the new image
    #new_canvas <- tkcanvas(main_frame)
    #tkcreate(canvas, "image", 0, 0, anchor = "nw", image = "::image::imgteste2")
    tkconfigure(canvas, image = "::image::imgteste2", width = width, height = height)

    # Pack new canvas and copy ID to global variable
    # tkpack(new_canvas, fill = "both", expand = TRUE)
    # canvas <- new_canvas
    options(warn = oldw)
  }


  # CALLBACKS
  onResize <- function() {
    drawGraph()
  }

  # Window Event Binds
  tkbind(parent, '<Configure>', onResize)

  #tkwait.window(parent)
}

tkplotmidrangeMCparent <- function(parent, image) {
  # Label
  label <- tklabel(parent)
  # Building
  tkpack(parent, label, fill = "both", expand=TRUE)

  # Imagem
  tkimage.create("photo", "::image::imgteste4", file = image)

  tkimage.create("photo", "::image::imgredimens")

  tcl("::image::imgredimens", "copy", "::image::imgteste4", "-subsample", 2 , 2)

  tkconfigure(label, image = "::image::imgredimens")
}

# Examples
# library(tcltk)
# # Janel principal
# tk <- tktoplevel()
# # Grafico
# # Simulated data (completely randomized design)
# rv <- c(100.08, 105.66, 97.64, 100.11, 102.60, 121.29, 100.80,
#         99.11, 104.43, 122.18, 119.49, 124.37, 123.19, 134.16,
#         125.67, 128.88, 148.07, 134.27, 151.53, 127.31)
# # Treatments
# treat <- factor(rep(LETTERS[1:5], each = 4))
# # Anova
# res     <- aov(rv~treat)
# # Loading the midrangeMCP package
# library(midrangeMCP)
# # Choosing tests
# results <- MRtest(y = res, trt = "treat", alpha = 0.05,
#                   main = "Multiple Comparison Procedures",
#                   MCP = c("MGM"))
# # Anexando funcao interna tkplotmidrangeMCP3
# tkplotmidrangeMCP3(tk, results, col = heat.colors(4))
# tkplotmidrangeMCP2(tk, results, col = heat.colors(4))
# tkplotmidrangeMCP(results, col = heat.colors(4))
