# R/zzz.R
#
# Package-level mutable state. Held in a private environment so we don't
# leak settings into the global `options()` namespace.
#
# As of v0.2.0, each registered pool is a list with fields:
#   data_path      chr   the registered data directory
#   codebook_path  chr   resolved codebook directory (default: <data_path>/documentation)
#   crosswalk      chr   resolved crosswalk file path (default: <codebook_path>/<dataset>_crosswalk.csv)
#   files          named list   year (chr) -> file path
# Pools are keyed by dataset name in `.brfss_env$pools`.

.brfss_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
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

# Internal: get the registered pool config for a dataset; NULL if not set.
.brfss_get_pool <- function(dataset) {
  pools <- .brfss_get("pools", list())
  pools[[dataset]]
}

# Internal: set/replace a dataset's pool config.
.brfss_set_pool_internal <- function(dataset, config) {
  pools <- .brfss_get("pools", list())
  pools[[dataset]] <- config
  .brfss_set("pools", pools)
}

# Internal: list which datasets currently have a pool registered.
.brfss_list_pools <- function() {
  names(.brfss_get("pools", list()))
}

# Internal: drop a registered pool (or all of them if dataset is NULL).
.brfss_clear_pool <- function(dataset = NULL) {
  if (is.null(dataset)) {
    .brfss_set("pools", list())
  } else {
    pools <- .brfss_get("pools", list())
    pools[[dataset]] <- NULL
    .brfss_set("pools", pools)
  }
  invisible(NULL)
}
