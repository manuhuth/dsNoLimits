#' Read back the SAEM coordinator closed-form update
#'
#' Aggregate method, run on the COORDINATOR site only, immediately after
#' [nolimitsSaemMstepDS()] has written the update into the cache. Returns the
#' closed-form-eligible parameter names and their TRANSFORMED-scale values so the
#' client can write them into its parameter vector. This is the read half of the
#' assign-updates-state / aggregate-reads-result pattern the stateful closed-form
#' M-step needs. Everything returned is model-derived; nothing is per-subject.
#'
#' Requires NoLimits >= 0.2.7 (the SAEM dev_api primitives).
#'
#' @param cache.name name of the symbol holding a cache that [nolimitsSaemMstepDS()]
#'   has just updated this outer iteration.
#' @return A named list with `names` (character) and `values` (numeric,
#'   transformed scale), of equal length.
#' @export
nolimitsSaemUpdatesDS <- function(cache.name) {
  .dsenv <- parent.frame()
  prep <- .ds_get(cache.name, .dsenv, .nlds_is_prep, "NoLimits fit cache")

  .nlds_em_guard()
  u <- prep[["saem.update"]]
  if (is.null(u)) {
    stop("FAILED: no closed-form update is cached; run nolimitsSaemMstepDS first",
         call. = FALSE)
  }

  names <- tryCatch(as.character(.nlds_fun("nlds_saem_names")(u)),
                    error = function(e) {
                      stop("FAILED: the closed-form update could not be read on this server: ",
                           .nlds_brief(e), call. = FALSE)
                    })
  values <- tryCatch(as.numeric(.nlds_fun("nlds_saem_values")(u)),
                     error = function(e) {
                       stop("FAILED: the closed-form update could not be read on this server: ",
                            .nlds_brief(e), call. = FALSE)
                     })
  if (length(names) != length(values)) {
    stop("FAILED: the closed-form update is inconsistent on this server",
         call. = FALSE)
  }

  list(names = names, values = values)
}
#AGGREGATE FUNCTION
