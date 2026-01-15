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
          packageStartupMessage ("Attached: 'sohn' (Version: ",utils::packageVersion('sohn'),  ")") 
          
      #While developing:
         packageStartupMessage ("#######################################################\n",
                              "This Version 2026 01 15 - 10.21AM\n" )


  invisible()
}

