.First.lib <- function(libname, pkgname, where) {
    library.dynam("multtest", pkgname, libname)
    require(ctest) || stop("Needs package ctest")
}

