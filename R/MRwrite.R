#' Export the results of the \code{MRtest} function.
#'
#' The \code{x} object from a \code{MRtest} is written to file arguments.
#' @param x object from the \code{MRtest} functions.
#' @param MCP Allows choosing the multiple comparison test.
#'     The \emph{defaut} is "all". This option will go perform all tests
#'     from the \code{MRtest} object.
#' @param extension Type of format of the file. Four options
#'     \code{"csv"}, \code{"txt"} \code{"xlsx"} and \code{"latex"}.
#'      The \emph{default} is \code{"csv"}.
#' @param dataMR Allows to choose the results to bee written. Three options are
#'     available: \code{"groups"}, \code{"summary"} or \code{"all"}. The option
#'     \code{"groups"} writes the treatment mean groups avaluated by the chosen test
#'     in the \code{MCP} argument.  The \code{"summary"} writes the descriptive
#'     statistics of the response variable. The options \code{"all"} should be chosen
#'     for both results.
#' @return \code{MRwrite} writes the most important results for the chosen
#'     tests in the \code{MCP} argument.
#' @details Note that the choice of the tests in the \code{MRwrite}
#'     function must be in accordance with the tests chosen
#'     in the \code{x} argument.
#' @examples
#' # Simulated data (completely randomized design)
#'
#' rv <- c(100.08, 105.66, 97.64, 100.11, 102.60, 121.29, 100.80,
#'         99.11, 104.43, 122.18, 119.49, 124.37, 123.19, 134.16,
#'         125.67, 128.88, 148.07, 134.27, 151.53, 127.31)
#'
#' # Treatments
#' treat <- factor(rep(LETTERS[1:5], each = 4))
#'
#' # Anova
#' res <- aov(rv~treat)
#'
#' # Loading the midrangeMCP package
#' library(midrangeMCP)
#'
#' # Choosing any tests
#' results <- MRtest(y = res, trt = "treat", alpha = 0.05,
#'                    main = "Multiple Comparison Procedures",
#'                    MCP = c("MGM", "TM"))
#'
#' #Export file in latex (Output in Console)
#' MRwrite(results, MCP = "all", extension = "latex", dataMR = "all")
#'
#' #Observation: The MRwrite function export
#' #             only one extension at a time
#' @importFrom "writexl" "write_xlsx"
#' @importFrom "xtable" "xtable"
#' @export
MRwrite <- function(x, MCP = "all", extension = "csv",
                    dataMR = "all"){
  mcps  <- c("MGM", "MGR", "SNKM", "TM")
  tests <- sort(x$Tests) # Tests ordered
  nmcps <- sort(pmatch(tests, mcps)) # Number of tests chosen

  # For option MCP = "all"
  if (all(MCP == "all")){
    MCP = tests
  }

  # Verify the tests selected
  nas  <- pmatch(MCP, mcps)

  # For option dataMR = "all"
  if (dataMR == "all"){
    dataMR <- c("groups", "summary")
  }

  MCP <- sort(MCP) # MCP's ordered
  #################################################
  # Defensive programming: the length of the extension argument
  #                        must be less or equal to 1
  if(length(extension) > 1){
    stop("The length of the extension argument is greater than 1. ", "\n Options: ", " csv", " txt", " xlsx", " latex",
         call. = FALSE)
  }
  #################################################

  #Initial groups:
  test.MGM  <- NA
  test.MGR  <- NA
  test.SNKM <- NA
  test.TM   <- NA



  #################################################
  # Defensive programming: any invalid MCP
  namcp <- pmatch(MCP, tests)
  if (any(is.na(namcp))){
    if (length(tests) == 1) {
      stop("The choice of the tests in the MCP argument must be in accordance with the tests chosen in the x argument \n Options: ",
           tests, call. = FALSE)
    } else{
      writest <- tests[1]
      for (i in 2:length(tests)) {
        writest <- paste(writest, tests[i], sep = " ")
      }
      stop("The choice of the tests in the MCP argument must be in accordance with the tests chosen in the x argument \n Options: ",
           writest, call. = FALSE)
    }
  }
  #################################################

  #################################################
  #Defensive programming: For any invalid dataMR
  database <- c("groups", "summary")
  if (any(dataMR == database) == FALSE) {
    stop("Any dataMR argument is invalid \n Options: ",
         "groups", " summary", call. = FALSE)
  }
  #################################################



  #################################################
  #Defensive programming: For any invalid extension
  extensionbase <- c("csv", "txt", "xlsx", "latex")
  if (any(extension == extensionbase) == FALSE) {
    stop("Any extension argument is invalid \n Options: ",
         "csv", " txt", " xlsx", " latex" , call. = FALSE)
  }
  #################################################

  # Write the results for option dataMR = "groups"
  if (any(dataMR == "groups")) {
    if (any(MCP == "MGM")) {
      name <- "groupMGM"
      name <- paste(name, extension, sep = ".")
      if (extension == "csv") {
        trt <- rownames(x[[2]][[1]])
        dat <- data.frame(trt, x[[2]][[1]])
        utils::write.table(dat, name, sep = ";", row.names = FALSE)
      }
      if (extension == "txt"){
        trt <- rownames(x[[2]][[1]])
        dat <- data.frame(trt, x[[2]][[1]])
        utils::write.table(dat, name, sep = "\t", row.names = FALSE, quote = FALSE)
      }
      if (extension == "xlsx"){
        trt <- rownames(x[[2]][[1]])
        dat <- data.frame(trt, x[[2]][[1]])
        name <- paste(getwd(), name, sep = "/")
        writexl::write_xlsx(dat, name)
      }
      if (extension == "latex"){
        trt  <- rownames(x[[2]][[1]])
        ntrt <- length(trt)
        dat  <- data.frame(trt, x[[2]][[1]])
        rownames(dat) <- 1:ntrt
        cat("Table in latex of results of the MGM test\n\n")
        test.MGM <- xtable::xtable(dat)
        print(test.MGM, include.rownames = FALSE)
      }
    }
    if (any(MCP == "MGR")) {
      name <- "groupMGR"
      name <- paste(name, extension, sep = ".")
      cont <- nmcps <= 1
      cont <- length(cont[cont == TRUE])
      if(extension == "csv") {
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        utils::write.table(dat, name, sep = ";", row.names = FALSE)
      }
      if (extension == "txt") {
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        utils::write.table(dat, name, sep = "\t", row.names = FALSE , quote = FALSE)
      }
      if (extension == "xlsx") {
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        name <- paste(getwd(), name, sep = "/")
        writexl::write_xlsx(dat, name)
      }
      if (extension == "latex") {
        trt  <- rownames(x[[2]][[cont + 1]])
        ntrt <- length(trt)
        dat  <- data.frame(trt, x[[2]][[cont + 1]])
        rownames(dat) <- 1:ntrt
        cat("\n\nTable in latex of results of the MGR test\n\n")
        test.MGR <- xtable::xtable(dat)
        print(test.MGR, include.rownames = FALSE)
      }
    }
    if (any(MCP == "SNKM")) {
      name <- "groupSNKM"
      name <- paste(name, extension, sep = ".")
      cont <- nmcps <= 2
      cont <- length(cont[cont == TRUE])
      if(extension == "csv"){
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        utils::write.table(dat, name, sep = ";", row.names = FALSE)
      }
      if (extension == "txt"){
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        utils::write.table(dat, name, sep = "\t", row.names = FALSE, quote = FALSE)
      }
      if (extension == "xlsx"){
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        name <- paste(getwd(), name, sep = "/")
        writexl::write_xlsx(dat, name)
      }
      if (extension == "latex"){
        trt  <- rownames(x[[2]][[cont + 1]])
        ntrt <- length(trt)
        dat  <- data.frame(trt, x[[2]][[cont + 1]])
        rownames(dat) <- 1:ntrt
        cat("\n\nTable in latex of results of the SNKM test\n\n")
        test.SNKM <- xtable::xtable(dat)
        print(test.SNKM, include.rownames = FALSE)
      }
    }
    if (any(MCP == "TM")){
      name <- "groupTM"
      name <- paste(name, extension, sep = ".")
      cont <- nmcps <= 3
      cont <- length(cont[cont == TRUE])
      if(extension == "csv"){
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        utils::write.table(dat, name, sep = ";", row.names = FALSE)
      }
      if (extension == "txt"){
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        utils::write.table(dat, name, sep = "\t", row.names = FALSE, quote = FALSE)
      }
      if (extension == "xlsx"){
        trt <- rownames(x[[2]][[cont + 1]])
        dat <- data.frame(trt, x[[2]][[cont + 1]])
        name <- paste(getwd(), name, sep = "/")
        writexl::write_xlsx(dat, name)
      }
      if (extension == "latex"){
        trt  <- rownames(x[[2]][[cont + 1]])
        ntrt <- length(trt)
        dat  <- data.frame(trt, x[[2]][[cont + 1]])
        rownames(dat) <- 1:ntrt
        cat("\n\nTable in latex of results of the TM test\n\n")
        test.TM <- xtable::xtable(dat)
        print(test.TM, include.rownames = FALSE)
      }
    }
  }
  # Write the results for option dataMR = "summary"
  if (any(dataMR == "summary")) {
    name <- "Summary"
    name <- paste(name, extension, sep = ".")
    if(extension == "csv"){
      trt <- rownames(x[[1]])
      dat <- data.frame(trt, x[[1]])
      utils::write.table(dat, name, sep = ";", row.names = FALSE)
    }
    if (extension == "txt"){
      trt <- rownames(x[[1]])
      dat <- data.frame(trt, x[[1]])
      utils::write.table(dat, name, sep = "\t", row.names = FALSE, quote = FALSE)
    }
    if (extension == "xlsx"){
      trt <- rownames(x[[1]])
      dat <- data.frame(trt, x[[1]])
      name <- paste(getwd(), name, sep = "/")
      writexl::write_xlsx(dat, name)
    }
    if (extension == "latex"){
      trt  <- rownames(x[[1]])
      ntrt <- length(trt)
      dat  <- data.frame(trt, x[[1]])
      rownames(dat) <- 1:ntrt
      # cat("\n\nTable in latex of results of descriptive statistics\n\n")
      # print(xtable::xtable(dat), include.rownames = FALSE)
      statdesc <- xtable::xtable(dat)
    }
  }
  if (extension == "latex"){
    cat("\nSee yours tables in Console\n", "Format:", extension, "\n")
  }
  if (extension != "latex"){
    cat("See your files in Directory: ", getwd(), "\n", "Format:", extension, "\n")
  }

  #All groups
  group.tests <- list(group.MGM  = test.MGM,
                      group.MGR  = test.MGR,
                      group.SNKM = test.SNKM,
                      group.TM   = test.TM)
  ################
  # Output results
  ################

  alltest    <- 1:4
  nas        <- nas[order(nas, na.last = NA)]
  ntest      <- alltest[-nas]

  if (length(nas) == 4) {
    grouptest  <- group.tests
  } else {
    grouptest  <- group.tests[-ntest]
  }
  if (extension != "latex") {
    output <- gettext("See your files in Directory", domain = "R-MCPtests")
  } else {
    output <- list(
      "Result of tests" = grouptest,
      "Descriptive Statistics" = statdesc
    )
  }

  return(output)


}


