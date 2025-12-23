# Package initialization
# This file is executed when the package is loaded

.onLoad <- function(libname, pkgname) {
  # Package initialization code goes here
  # Currently no initialization needed, but this file is here for future use
  invisible()
}

.onAttach <- function(libname, pkgname) {

     #Startup msgs
      #Version and URL for website
          packageStartupMessage ("Attached: 'sohn' (Version: ",packageVersion('sohn'),  ")") 
          
      #While developing:
         packageStartupMessage ("#######################################################\n",
                              "This Version 2025 12 23 - 06.40AM\n" )


  invisible()
}

