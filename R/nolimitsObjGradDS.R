#' Marginal log-likelihood and gradient of this server's data at one parameter vector
#'
#' Aggregate method, and the hot path of the federated fit. The released
#' quantities per round are one scalar log-likelihood, one p-vector gradient, one
#' p-vector of natural-scale parameters (a deterministic model transform of the
#' argument, carrying no data), one flag and one subject count. Nothing whose
#' size depends on the number of observations or subjects is returned.
#'
#' A non-finite objective is a legitimate estimator result at an out-of-domain
#' probe point, not a server failure: it is reported with `finite = FALSE` so the
#' client's line search can back off. Only genuine errors stop.
#'
#' @param prep.name name of the symbol holding a cache built by
#'   [nolimitsPrepDS()] or [nolimitsPrepStringDS()].
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' For `estimator = "map"` the returned `value` and `grad` carry the
#' LIKELIHOOD-ONLY part of the objective, and the log-prior is returned
#' separately as `prior.value` and `prior.grad`. The log-prior is data-free and
#' identical at every site, so a plain sum over sites would count it once per
#' site; the client adds the prior block exactly once. The prior block is a
#' function of the declared priors and the parameter transform alone and carries
#' no information about this server's data.
#'
#' `"mle"` and `"map"` require a model with NO random effects. An analyst who
#' prepares a random-effects model and then asks for them gets the NoLimits
#' precondition error relayed as a FAILED message.
#'
#' @param estimator one of `"laplace"`, `"focei"`, `"ghq"`, `"pooled"`,
#'   `"mle"`, `"map"`.
#' @param ghq.level Gauss-Hermite quadrature level, 1 to 9. Ignored unless
#'   `estimator` is `"ghq"`.
#' @param round the federated round number, 1 to the `dsNoLimits.maxRounds`
#'   option.
#' @return A named list with `value`, `grad`, `natural`, `finite` and
#'   `n.subjects`, plus `prior.value` and `prior.grad` when
#'   `estimator = "map"`.
#' @export
nolimitsObjGradDS <- function(prep.name, theta, estimator, ghq.level = 1L,
                              round = 1L) {
  .dsenv <- parent.frame()
  prep <- .ds_get(prep.name, .dsenv, .nlds_is_prep, "NoLimits fit cache")
  p <- as.integer(prep$p)

  # theta is exempt from nfilter.string by design: a %a-encoded p-vector exceeds
  # 80 characters at p >= 4. It is validated by decode and length instead.
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
                laplace = , focei = , ghq = , pooled = , mle = , map = estimator,
                stop("FAILED: estimator must be one of laplace, focei, ghq, ",
                     "pooled, mle, map", call. = FALSE))

  # Integer literals do not cross the Opal grammar, so whole-numbered doubles
  # arrive here; test for whole-numberedness, never for is.integer().
  if (!.nlds_is_whole(ghq.level) || ghq.level < 1 || ghq.level > 9) {
    stop("FAILED: ghq.level must be an integer between 1 and 9", call. = FALSE)
  }
  level <- as.integer(ghq.level)
  max.rounds <- suppressWarnings(as.integer(.nlds_opt("dsNoLimits.maxRounds", 500L)))
  if (length(max.rounds) != 1L || is.na(max.rounds)) max.rounds <- 500L
  if (!.nlds_is_whole(round) || round < 1 || round > max.rounds) {
    stop("FAILED: round must be an integer between 1 and ", max.rounds,
         call. = FALSE)
  }

  .nlds_require_julia()
  .nlds_helpers()
  flat <- tryCatch(
    as.numeric(.nlds_fun("nlds_objgrad")(prep$dm, theta.vec,
                                         .nlds_fun("nlds_method")(est, level))),
    error = function(e) {
      stop("FAILED: the objective could not be evaluated on this server: ",
           .nlds_brief(e), call. = FALSE)
    })
  if (length(flat) != 1L + 2L * p) {
    stop("FAILED: the objective could not be evaluated on this server: ",
         "unexpected result length", call. = FALSE)
  }

  value <- flat[1L]
  grad <- flat[1L + seq_len(p)]
  natural <- flat[1L + p + seq_len(p)]

  # The MAP log-prior, released once per round as its own block so the client
  # can add it once rather than once per site. Model-derived, not data-derived.
  prior <- NULL
  if (est == "map") {
    prior <- tryCatch(
      as.numeric(.nlds_fun("nlds_prior")(prep$dm, theta.vec)),
      error = function(e) {
        stop("FAILED: the log-prior could not be evaluated on this server: ",
             .nlds_brief(e), call. = FALSE)
      })
    if (length(prior) != 1L + p) {
      stop("FAILED: the log-prior could not be evaluated on this server: ",
           "unexpected result length", call. = FALSE)
    }
  }

  finite <- is.finite(value) && all(is.finite(grad)) &&
    (is.null(prior) || all(is.finite(prior)))
  if (!finite) {
    value <- NA_real_
    grad <- rep(0, p)
    if (!is.null(prior)) prior <- c(NA_real_, rep(0, p))
  }
  if (!all(is.finite(natural))) natural <- rep(0, p)

  out <- list(value = value,
              grad = grad,
              natural = natural,
              finite = finite,
              n.subjects = as.integer(prep$n.subjects))
  if (!is.null(prior)) {
    out$prior.value <- prior[1L]
    out$prior.grad <- prior[-1L]
  }
  out
}
#AGGREGATE FUNCTION
