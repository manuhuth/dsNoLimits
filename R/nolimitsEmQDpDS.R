#' Differentially private MCEM/SAEM M-step part gradient
#'
#' Aggregate method, the differentially private counterpart of [nolimitsEmQDS()]
#' for the DP-Adam M-step. At the FIXED draws cached by [nolimitsEmEStepDS()],
#' returns the sum over this server's subjects of their per-subject gradient over
#' one parameter part (`"q1"` or `"q2"`, optionally restricted to a subset of free
#' names for the SAEM numerical M-step), each scaled by the preconditioning the
#' client's Adam optimiser steps in and then L2-clipped as one block to `dp.clip`,
#' plus Gaussian noise of standard deviation `sigma * dp.clip`. Adding or removing
#' one subject moves the un-noised sum by at most `dp.clip`, so this is the
#' Gaussian mechanism at noise multiplier `sigma`.
#'
#' NO Q value is released: a value would be a second, separately accountable
#' function of the data (matching [nolimitsObjGradDpDS()]). Every mechanism
#' parameter is owner-set through the `dsNoLimits.dp.*` options, and each release
#' is charged write-ahead to the persistent ledger and refused once the owner's
#' epsilon budget is reached.
#'
#' Requires NoLimits >= 0.2.7 (the MCEM dev_api primitives).
#'
#' @param cache.name name of the symbol holding a cache that [nolimitsEmEStepDS()]
#'   has run at least once this outer iteration.
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @param part `"q1"` or `"q2"`.
#' @param round the Adam step index within one M-step call (1 to `dp.rounds`),
#'   used for validation only. The whole-fit privacy cap is enforced by the
#'   persistent epsilon ledger, which accumulates across every release
#'   regardless of this value; it is not a fit-wide counter.
#' @param free.names optional comma-joined subset of the part's free names, in the
#'   order the gradient should be returned; `""` (the default) means the whole
#'   part.
#' @return A named list with `grad` (the clipped, noised sum), `releases`,
#'   `epsilon`, `remaining.budget`, `delta` and `n.subjects`.
#' @export
nolimitsEmQDpDS <- function(cache.name, theta, part, round = 1L, free.names = "") {
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
  if (!is.character(part) || length(part) != 1L || is.na(part) ||
      !(part %in% c("q1", "q2"))) {
    stop("FAILED: part must be one of q1, q2", call. = FALSE)
  }
  if (!is.character(free.names) || length(free.names) != 1L || is.na(free.names)) {
    stop("FAILED: free.names must be a single character string", call. = FALSE)
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
  full <- if (part == "q1") prep[["q1.names"]] else prep[["q2.names"]]
  if (nzchar(free.names)) {
    free <- unlist(strsplit(free.names, ",", fixed = TRUE))
    if (!all(free %in% full)) {
      stop("FAILED: free.names must be a subset of the part free names",
           call. = FALSE)
    }
  } else {
    free <- full
  }
  k <- length(free)
  # The preconditioning s of exactly these free names, in the returned order.
  ss <- as.numeric(prep$scale)[match(free, as.character(prep$names))]

  flat <- tryCatch(
    as.numeric(.nlds_fun("nlds_mcem_dp_rows")(prep[["dm"]], theta.vec, prep[["em"]], part, free)),
    error = function(e) {
      stop("FAILED: the per-subject gradients could not be evaluated on this ",
           "server: ", .nlds_brief(e), call. = FALSE)
    })
  rows <- .dp_flat_matrix(flat, "the per-subject gradients")
  if (ncol(rows) != k) {
    stop("FAILED: the per-subject gradients could not be evaluated on this ",
         "server: unexpected result length", call. = FALSE)
  }
  # A per-subject sensitivity bound only exists when the clipping unit IS the
  # subject; refuse rather than release something the accounting does not cover.
  if (nrow(rows) != as.integer(prep$n.subjects)) {
    stop("FAILED: differentially private analysis needs one independence unit ",
         "per subject, but this model groups subjects together; no per-subject ",
         "sensitivity bound exists for it", call. = FALSE)
  }
  # Precondition BEFORE clipping: clip in the coordinates the client's Adam steps.
  rows <- rows * rep(ss, each = nrow(rows))

  rel <- .dp_em_release(cfg, fingerprint, rows)
  list(grad = rel$value,
       releases = rel$releases,
       epsilon = rel$epsilon,
       remaining.budget = rel$remaining.budget,
       delta = rel$delta,
       n.subjects = as.integer(prep$n.subjects))
}
#AGGREGATE FUNCTION
