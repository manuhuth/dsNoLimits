#' MCEM M-step Q value and gradient over one parameter part
#'
#' Aggregate method, the hot path of the federated MCEM M-step. At the FIXED
#' draws cached by [nolimitsEmEStepDS()], returns this server contribution to the
#' Monte-Carlo Q-function and its gradient over one part of the parameter vector
#' (`"q1"`, the observation-side names, or `"q2"`, the random-effect-distribution
#' names, from [nolimitsPrepInfoDS()]). The value is on the MAXIMIZATION sense and
#' the gradient is on the transformed axes of that part free names; the client
#' sums both over sites, which reproduces the pooled Q at the same draws exactly
#' (per-subject additive). Nothing whose size depends on the number of subjects is
#' returned.
#'
#' A non-finite Q at an out-of-domain probe is a legitimate estimator result, not
#' a server failure: it is reported with `finite = FALSE` and a zero gradient so
#' the client optimiser backtracks. Only genuine errors stop.
#'
#' Requires NoLimits >= 0.2.7 (the MCEM dev_api primitives).
#'
#' @param cache.name name of the symbol holding a cache that [nolimitsEmEStepDS()]
#'   has run at least once this outer iteration.
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @param part `"q1"` or `"q2"`.
#' @param round the federated round number, 1 to the `dsNoLimits.maxRounds`
#'   option.
#' @param free.names optional comma-joined subset of the part's free names, in the
#'   order the gradient should be returned. The SAEM numerical M-step uses it to
#'   restrict the Q to the non-closed-form parameters of the part; `""` (the
#'   default, used by MCEM) means the whole part.
#' @return A named list with `value` (double or NA), `grad` (numeric, length of
#'   the requested free names, zeros if non-finite), `finite` (logical) and
#'   `n.subjects` (integer).
#' @export
nolimitsEmQDS <- function(cache.name, theta, part, round = 1L, free.names = "") {
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
  full <- if (part == "q1") prep[["q1.names"]] else prep[["q2.names"]]
  # SAEM restricts the part to its numerical free names; MCEM sends "" (whole part).
  if (!is.character(free.names) || length(free.names) != 1L || is.na(free.names)) {
    stop("FAILED: free.names must be a single character string", call. = FALSE)
  }
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

  flat <- tryCatch(
    as.numeric(.nlds_fun("nlds_mcem_q")(prep[["dm"]], theta.vec, prep[["em"]], part, free)),
    error = function(e) {
      stop("FAILED: the Q-function could not be evaluated on this server: ",
           .nlds_brief(e), call. = FALSE)
    })
  if (length(flat) != 1L + k) {
    stop("FAILED: the Q-function could not be evaluated on this server: ",
         "unexpected result length", call. = FALSE)
  }

  value <- flat[1L]
  grad <- if (k > 0L) flat[1L + seq_len(k)] else numeric(0)
  finite <- is.finite(value) && all(is.finite(grad))
  if (!finite) {
    value <- NA_real_
    grad <- rep(0, k)
  }

  list(value = value,
       grad = grad,
       finite = finite,
       n.subjects = as.integer(prep$n.subjects))
}
#AGGREGATE FUNCTION
