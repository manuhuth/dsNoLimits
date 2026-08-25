#' Differentially private gradient of this server's data at one parameter vector
#'
#' Aggregate method. The differentially private counterpart of
#' [nolimitsObjGradDS()], and a separately permittable method: a data owner who
#' does not want DP analysis simply leaves it out of the profile, and one who
#' wants ONLY DP analysis leaves `nolimitsObjGradDS` out instead.
#'
#' The release is the sum over this server's subjects of their per-subject
#' gradient, each L2-clipped in the preconditioned transformed coordinates the
#' client optimises in, plus Gaussian noise of standard deviation
#' `sigma * C_total`. Adding or removing one subject moves the un-noised sum by
#' at most `C_total`, so this is the Gaussian mechanism at noise multiplier
#' `sigma`. NO objective value is released: the value would be a second,
#' separately accountable function of the data.
#'
#' Every parameter of the mechanism is set by the DATA OWNER through the
#' `dsNoLimits.dp.*` options. The analyst sets none of them and cannot see them
#' except through the epsilon this function reports.
#'
#' Each release is charged to an append-only ledger keyed by a fingerprint of
#' the data and the model, BEFORE the value is returned, and is refused once the
#' owner's cumulative epsilon budget is reached. The ledger lives on disk, so
#' logging out and back in does not reset it.
#'
#' @param prep.name name of the symbol holding a cache built by
#'   [nolimitsPrepDS()] or [nolimitsPrepStringDS()].
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @param estimator one of `"laplace"`, `"focei"`, `"ghq"` or `"mle"`.
#'   `"pooled"` and `"map"` are refused: the naive-pooled objective calibrates
#'   its plug-in random effects on the whole data set and has no per-subject
#'   term, and the MAP log-prior is not a per-subject term either, so neither
#'   has a bounded per-subject sensitivity.
#' @param ghq.level Gauss-Hermite quadrature level, 1 to 9. Ignored unless
#'   `estimator` is `"ghq"`.
#' @param round the fit's round number, 1 to the `dsNoLimits.dp.maxT` option.
#' @return A named list with `grad` (the clipped, noised sum), `releases` (how
#'   many releases this data set has been charged, ever), `epsilon`,
#'   `remaining.budget`, `delta` and `n.subjects`.
#' @export
nolimitsObjGradDpDS <- function(prep.name, theta, estimator, ghq.level = 1L,
                                round = 1L) {
  .dsenv <- parent.frame()
  prep <- .ds_get(prep.name, .dsenv, .nlds_is_prep, "NoLimits fit cache")
  p <- as.integer(prep$p)

  if (!is.character(theta) || length(theta) != 1L || is.na(theta)) {
    stop("FAILED: theta must be a single character string", call. = FALSE)
  }
  theta.vec <- suppressWarnings(.ds_num_decode(theta))
  if (length(theta.vec) != p || anyNA(theta.vec)) {
    stop("FAILED: theta must decode to ", p, " numeric values", call. = FALSE)
  }

  if (!is.character(estimator) || length(estimator) != 1L || is.na(estimator)) {
    stop("FAILED: estimator must be a single character string", call. = FALSE)
  }
  est <- switch(estimator,
                laplace = , focei = , ghq = , mle = estimator,
                pooled = , map = stop(
                  "FAILED: estimator '", estimator, "' has no per-subject term ",
                  "and therefore no bounded sensitivity, so it cannot be used ",
                  "with differential privacy; use laplace, focei, ghq or mle",
                  call. = FALSE),
                stop("FAILED: estimator must be one of laplace, focei, ghq, mle",
                     call. = FALSE))

  if (!.nlds_is_whole(ghq.level) || ghq.level < 1 || ghq.level > 9) {
    stop("FAILED: ghq.level must be an integer between 1 and 9", call. = FALSE)
  }
  level <- as.integer(ghq.level)

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

  .nlds_require_julia()
  .nlds_helpers()

  spec <- .dp_clip_spec(as.character(prep$names), cfg$clip, cfg$clip.mode)
  g <- .dp_subject_gradients(prep, theta.vec, est, level)
  clipped <- .dp_clip_sum(g, spec$ids, spec$clips)

  # Write-ahead: charge the budget before the release exists, so a response
  # lost in transit is still counted as spent.
  charge <- .dp_ledger_charge(cfg, fingerprint, spec$total)

  list(grad = clipped + .dp_noise(p, cfg$sigma * spec$total),
       releases = as.integer(charge$releases),
       epsilon = charge$epsilon,
       remaining.budget = charge$remaining,
       delta = cfg$delta,
       n.subjects = as.integer(prep$n.subjects))
}
#AGGREGATE FUNCTION
