# R/zzz.R
#
# Package-level mutable state. Held in a private environment so we don't
# leak settings into the global `options()` namespace.

.brfss_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Initialize with an empty named list of pools, keyed by dataset.
  # Each pool is itself a named list mapping year (chr) -> file path.
  if (!exists("pools", envir = .brfss_env, inherits = FALSE)) {
    assign("pools", list(), envir = .brfss_env)
  }
}

# Internal accessor: get a value from the package env, with default fallback.
.brfss_get <- function(key, default = NULL) {
  if (exists(key, envir = .brfss_env, inherits = FALSE)) {
    get(key, envir = .brfss_env, inherits = FALSE)
  } else {
    default
  }
}

# Internal mutator: set a value in the package env.
.brfss_set <- function(key, value) {
  assign(key, value, envir = .brfss_env)
  invisible(value)
}

# Internal: get the pool index for a given dataset, NULL if not set.
.brfss_get_pool <- function(dataset) {
  pools <- .brfss_get("pools", list())
  pools[[dataset]]
}

# Internal: set/replace the pool index for a given dataset.
.brfss_set_pool <- function(dataset, index) {
  pools <- .brfss_get("pools", list())
  pools[[dataset]] <- index
  .brfss_set("pools", pools)
}

# Internal: list which datasets currently have a pool registered.
.brfss_list_pools <- function() {
  names(.brfss_get("pools", list()))
}
