.onLoad <- function(libname, pkgname) {
  assign("oldCartesian", getOption("datatable.allow.cartesian"),
        envir=.BaseNamespaceEnv)
  options("datatable.allow.cartesian" = TRUE)
}

.onUnload <- function(libname, pkgname) {
  options("datatable.allow.cartesian" = get("oldCartesian",
        envir=.BaseNamespaceEnv))
}
