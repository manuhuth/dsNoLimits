#' SAEM per-subject-additive sufficient statistics
#'
#' Aggregate method, the SAEM stats round. At the FIXED draws cached by
#' [nolimitsEmEStepDS()], returns this server's DE-NORMALIZED per-subject-additive
#' SAEM sufficient statistics as one flat numeric vector (RE moments as
#' `Sum-x = mean*n` and `Sum-xx = second*n` plus `n`; outcome and HMM fields are
#' plain sums). The layout is model-derived and identical on every site, so the
#' client sums the vectors coordinate-wise and the coordinator re-normalizes. The
#' length depends on the model, never on the number of subjects.
#'
#' Requires NoLimits >= 0.2.7 (the SAEM dev_api primitives).
#'
#' @param cache.name name of the symbol holding a cache that [nolimitsEmEStepDS()]
#'   has run this outer iteration.
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @param round the federated round number, 1 to the `dsNoLimits.maxRounds`
#'   option.
#' @return A named list with `stats` (a fixed-length finite numeric vector) and
#'   `n.subjects` (integer).
#' @export
nolimitsSaemStatsDS <- function(cache.name, theta, round = 1L) {
  .dsenv <- parent.frame()
  prep <- .ds_get(cache.name, .dsenv, .nlds_is_prep, "NoLimits fit cache")
  p <- as.integer(prep$p)

  if (!is.character(theta) || length(theta) != 1L || is.na(theta)) {
    stop("FAILED: theta must be a single character string", call. = FALSE)
  }
  theta.vec <- suppressWarnings(.ds_num_decode(theta))
  if (length(theta.vec) != p || anyNA(theta.vec)) {
    stop("FAILED: theta must decode to ", p, " numeric values", call. = FALSE)
  }
  max.rounds <- suppressWarnings(as.integer(.nlds_opt("dsNoLimits.maxRounds", 500L)))
  if (length(max.rounds) != 1L || is.na(max.rounds)) max.rounds <- 500L
  if (!.nlds_is_whole(round) || round < 1 || round > max.rounds) {
    stop("FAILED: round must be an integer between 1 and ", max.rounds,
         call. = FALSE)
  }

  .nlds_em_guard()
  # `[[` not `$`: `$em` partial-matches `em.available` / `em.outer`.
  if (is.null(prep[["em"]])) {
    stop("FAILED: no E-step draws are cached; run nolimitsEmEStepDS first",
         call. = FALSE)
  }

  flat <- tryCatch(
    as.numeric(.nlds_fun("nlds_saem_stats_flat")(prep[["dm"]], theta.vec, prep[["em"]])),
    error = function(e) {
      stop("FAILED: the sufficient statistics could not be computed on this server: ",
           .nlds_brief(e), call. = FALSE)
    })
  if (length(flat) < 1L || anyNA(flat) || !all(is.finite(flat))) {
    stop("FAILED: the sufficient statistics could not be computed on this server: ",
         "non-finite result", call. = FALSE)
  }

  list(stats = flat,
       n.subjects = as.integer(prep$n.subjects))
}
#AGGREGATE FUNCTION
