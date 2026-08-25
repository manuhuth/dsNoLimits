#' Report this server's NoLimits readiness
#'
#' Touches no data. This is the data owner's and the analyst's provisioning
#' diagnostic, so it never fails on a server without Julia: it reports
#' `julia.ok = FALSE` instead.
#'
#' @return A named list with `julia.ok`, `julia.version`, `nolimits.version`,
#'   `dsnolimits.version`, `nolimitsr.version` and `model.dir.set`.
#' @export
nolimitsStatusDS <- function() {
  out <- list(julia.ok = FALSE,
              julia.version = NA_character_,
              nolimits.version = NA_character_,
              dsnolimits.version = as.character(utils::packageVersion("dsNoLimits")),
              nolimitsr.version = NA_character_,
              model.dir.set = nzchar(as.character(
                .nlds_opt("dsNoLimits.modelDir", ""))[1L]))

  if (!requireNamespace("NoLimitsR", quietly = TRUE) ||
      !requireNamespace("JuliaConnectoR", quietly = TRUE)) {
    return(out)
  }
  out$nolimitsr.version <- tryCatch(
    as.character(utils::packageVersion("NoLimitsR")),
    error = function(e) NA_character_)

  st <- tryCatch(NoLimitsR::nolimits_status(), error = function(e) NULL)
  if (is.null(st) || !isTRUE(st$julia_setup_ok) || !isTRUE(st$started) ||
      !is.null(st$error)) {
    return(out)
  }
  out$julia.ok <- TRUE
  out$julia.version <- as.character(st$julia_version)
  out$nolimits.version <- as.character(st$nolimits_version)
  out
}
#AGGREGATE FUNCTION
