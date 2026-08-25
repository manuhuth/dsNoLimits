#' Differentially private SAEM sufficient statistics
#'
#' Aggregate method, the differentially private counterpart of
#' [nolimitsSaemStatsDS()]. At the FIXED draws cached by [nolimitsEmEStepDS()],
#' returns the sum over this server's subjects of their DE-NORMALIZED
#' per-subject-additive SAEM sufficient statistics, each L2-clipped as one block
#' to `dp.clip`, plus Gaussian noise of standard deviation `sigma * dp.clip`.
#' Adding or removing one subject moves the un-noised sum by at most `dp.clip`, so
#' this is the Gaussian mechanism at noise multiplier `sigma`.
#'
#' The coordinator's closed-form M-step ([nolimitsSaemMstepDS()]) runs on these
#' NOISED summed stats and is therefore post-processing: it releases nothing and
#' charges no budget. Every mechanism parameter is owner-set through the
#' `dsNoLimits.dp.*` options, and each release is charged write-ahead to the
#' persistent ledger and refused once the owner's epsilon budget is reached.
#'
#' Requires NoLimits >= 0.2.7 (the SAEM dev_api primitives).
#'
#' @param cache.name name of the symbol holding a cache that [nolimitsEmEStepDS()]
#'   has run this outer iteration.
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @param round the release index within one outer iteration, used for
#'   validation only. The whole-fit privacy cap is enforced by the persistent
#'   epsilon ledger, which accumulates across every release regardless of this
#'   value; it is not a fit-wide counter.
#' @return A named list with `stats` (the clipped, noised sum), `releases`,
#'   `epsilon`, `remaining.budget`, `delta` and `n.subjects`.
#' @export
nolimitsSaemStatsDpDS <- function(cache.name, theta, round = 1L) {
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

  cfg <- .dp_config()
  if (!.nlds_is_whole(round) || round < 1 || round > cfg$max.t) {
    stop("FAILED: round must be an integer between 1 and ",
         as.integer(cfg$max.t), " (the dsNoLimits.dp.maxT option)",
         call. = FALSE)
  }
  fingerprint <- prep$fingerprint
  if (!is.character(fingerprint) || length(fingerprint) != 1L ||
      is.na(fingerprint) || !nzchar(fingerprint)) {
    stop("FAILED: this fit cache carries no data fingerprint, so a privacy ",
         "budget cannot be accounted for it; prepare the model again",
         call. = FALSE)
  }

  .nlds_em_guard()
  # `[[` not `$`: `$em` partial-matches `em.available` / `em.outer`.
  if (is.null(prep[["em"]])) {
    stop("FAILED: no E-step draws are cached; run nolimitsEmEStepDS first",
         call. = FALSE)
  }

  flat <- tryCatch(
    as.numeric(.nlds_fun("nlds_saem_stats_dp_rows")(prep[["dm"]], theta.vec, prep[["em"]])),
    error = function(e) {
      stop("FAILED: the sufficient statistics could not be computed on this ",
           "server: ", .nlds_brief(e), call. = FALSE)
    })
  rows <- .dp_flat_matrix(flat, "the sufficient statistics")
  # A per-subject sensitivity bound only exists when the clipping unit IS the
  # subject; refuse rather than release something the accounting does not cover.
  if (nrow(rows) != as.integer(prep$n.subjects)) {
    stop("FAILED: differentially private analysis needs one independence unit ",
         "per subject, but this model groups subjects together; no per-subject ",
         "sensitivity bound exists for it", call. = FALSE)
  }

  rel <- .dp_em_release(cfg, fingerprint, rows)
  list(stats = rel$value,
       releases = rel$releases,
       epsilon = rel$epsilon,
       remaining.budget = rel$remaining.budget,
       delta = rel$delta,
       n.subjects = as.integer(prep$n.subjects))
}
#AGGREGATE FUNCTION
