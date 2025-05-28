# Enquanto eu nao entender como foi criado o arquivo "tkrplot.dll" que produz
# o objeto de imagem Rplot no tcl, irei utilizar o pacote tkrplot. Posteriormente,
# ao entendimento, volto ao codigo abaixo!

# .onLoad <- function(libname, pkgname) {
#   chname <- "midrangeMCP"
#   file.ext <- .Platform$dynlib.ext
#   dlname <- paste(chname, file.ext, sep = "")
#   if (is.character(.Platform$r_arch) && .Platform$r_arch != "")
#     path <- file.path("libs", .Platform$r_arc, dlname)
#   else path <- file.path("libs", dlname)
#   file <- system.file(path, package = pkgname, lib.loc = libname)[1]
#   tryCatch(tcl("load", file, "Rplot"),
#            error = function(e)
#              warning("loading Rplot failed", call. = FALSE))
# }
