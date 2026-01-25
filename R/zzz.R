# Package initialization
# This file is executed when the package is loaded

.onLoad <- function(libname, pkgname) {
  # Package initialization code goes here
  # Currently no initialization needed, but this file is here for future use
  invisible()
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {

     #Startup msgs
      #Version and URL for website
          packageStartupMessage ("Attached: 'statuser' (Version: ",utils::packageVersion('statuser'),  ")") 
          
      #While developing:
         packageStartupMessage ("#######################################################\n",
                              "This Version 2026 01 25 - 10.11AM\n" )


  invisible()
}

