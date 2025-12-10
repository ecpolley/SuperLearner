.onAttach <- function(libname, pkgname) {
  v <- utils::packageVersion(pkgname)
  b <- utils::packageDate(pkgname)

  foo <- sprintf("%s\nVersion: %s\nPackage created on: %s",
                 pkgname,
                 v,
                 if (anyNA(b)) "unknown" else format(b, "%F"))
  packageStartupMessage(foo)
}
