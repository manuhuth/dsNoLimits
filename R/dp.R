# Differential privacy: the accountant, the clipping, the noise source and the
# persistent budget ledger. Internal; only nolimitsObjGradDpDS calls any of it.
#
# The mechanism, per site: every SUBJECT's gradient is L2-clipped in the
# preconditioned transformed coordinates the client's optimiser steps in, the
# clipped terms are summed, and Gaussian noise of standard deviation
# sigma * C_total is added to the sum. Adding or removing one subject moves the
# un-noised sum by at most C_total, so the release is the Gaussian mechanism at
# noise multiplier sigma.
#
# Unlike the secure-aggregation setting this design is ported from, DataSHIELD
# has no secure aggregation and the aggregator IS the analyst: every site's
# release is individually visible, so each site adds the FULL noise (never
# sigma * C / sqrt(S)) and accounts its own epsilon.

# Renyi-DP order grid: fine below 10, where the optimum sits for any usable
# sigma, and integral above it.
.DP_ALPHAS <- sort(unique(c(seq(1.01, 10.0, length.out = 900), 10:512)))

#' Cumulative (epsilon, delta) for a list of Gaussian releases.
#'
#' One release of the Gaussian mechanism at sensitivity C and noise standard
#' deviation sigma * C is (alpha, alpha / (2 sigma^2))-RDP for every alpha > 1.
#' RDP composes by ADDITION, so a heterogeneous history of releases at
#' sigma_1 .. sigma_T is (alpha, alpha * sum_i 1/(2 sigma_i^2))-RDP. Converting
#' with the standard tail bound and minimising over alpha gives epsilon. The
#' heterogeneous form is what lets a budget compose across fits that the data
#' owner ran at different noise multipliers over time.
#'
#' @param sigmas numeric vector of per-release noise multipliers.
#' @param delta the target delta, strictly between 0 and 1.
#' @return the epsilon, or 0 for an empty history.
#' @keywords internal
#' @noRd
.dp_epsilon <- function(sigmas, delta) {
  sigmas <- as.numeric(sigmas)
  if (!length(sigmas)) return(0)
  if (any(!is.finite(sigmas)) || any(sigmas <= 0)) {
    stop("FAILED: the privacy ledger holds an invalid noise multiplier",
         call. = FALSE)
  }
  if (length(delta) != 1L || !is.finite(delta) || delta <= 0 || delta >= 1) {
    stop("FAILED: dp.delta must be strictly between 0 and 1", call. = FALSE)
  }
  k <- sum(1 / (2 * sigmas^2))
  min(.DP_ALPHAS * k + log(1 / delta) / (.DP_ALPHAS - 1))
}

# Substrings marking a parameter as a random-effect scale, covariance or the
# residual. Matched token-wise on the lower-cased name.
.DP_VARIANCE_MARKERS <- c("omega", "sigma", "tau", "corr", "cov", "sd", "var",
                          "rho")

#' The DP clipping group of each parameter: "variance" or "location".
#'
#' Joint clipping bounds each subject's whole gradient in one L2 ball, so an
#' outlier subject in the location coordinates has its variance-component
#' gradient scaled down too, which biases the variance estimates towards zero.
#' Splitting the coordinates and clipping each block on its own is the fix.
#' Group membership never affects the (epsilon, delta) bound.
#' @param names character vector of parameter names.
#' @return integer vector of group indices, 1-based, with a `groups` attribute.
#' @keywords internal
#' @noRd
.dp_groups <- function(names) {
  label <- vapply(names, function(n) {
    tokens <- strsplit(tolower(n), "[^a-z0-9]+")[[1L]]
    tokens <- tokens[nzchar(tokens)]
    hit <- any(vapply(tokens, function(t) {
      any(startsWith(t, .DP_VARIANCE_MARKERS))
    }, logical(1)))
    if (hit) "variance" else "location"
  }, character(1), USE.NAMES = FALSE)
  groups <- unique(label)
  out <- match(label, groups)
  attr(out, "groups") <- groups
  out
}

#' Per-subject, per-group L2 clipping, then the site sum.
#'
#' Clipping subject i's group-g sub-vector to C_g bounds the L2 norm of its
#' whole concatenated contribution by sqrt(sum_g C_g^2) = C_total, so C_total is
#' the sensitivity of the release and ISOTROPIC noise at sigma * C_total makes
#' it exactly the Gaussian mechanism at multiplier sigma. Per-group clipping is
#' therefore exactly as private as joint clipping at C_total, and the accountant
#' needs no per-group special-casing.
#'
#' @param g numeric matrix, one row per subject.
#' @param group.ids integer vector of length ncol(g).
#' @param clips numeric vector of per-group clips.
#' @return the clipped column sums.
#' @keywords internal
#' @noRd
.dp_clip_sum <- function(g, group.ids, clips) {
  g <- as.matrix(g)
  out <- g
  for (k in seq_along(clips)) {
    cols <- which(group.ids == k)
    if (!length(cols)) next
    block <- g[, cols, drop = FALSE]
    norms <- sqrt(rowSums(block^2))
    factors <- ifelse(norms > clips[k], clips[k] / pmax(norms, 1e-300), 1)
    out[, cols] <- block * factors
  }
  colSums(out)
}

#' C_total = sqrt(sum_g C_g^2): the L2 sensitivity of the clipped site sum.
#' @keywords internal
#' @noRd
.dp_clip_total <- function(clips) sqrt(sum(clips^2))

#' Uniform deviates from a cryptographic entropy source (OpenSSL's CSPRNG).
#'
#' Never the session's RNG stream: `datashield.seed` makes that stream
#' reproducible, and reproducible privacy noise is no privacy at all - anyone
#' holding the seed subtracts it and recovers the exact clipped sum.
#' `openssl::rand_bytes` draws from the operating system's CSPRNG on every
#' platform and leaves `.Random.seed` untouched, so a DP round cannot perturb an
#' analyst's other seeded work either.
#' @keywords internal
#' @noRd
.dp_os_uniform <- function(m) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop("FAILED: the 'openssl' package is required to draw differentially ",
         "private noise from a cryptographic entropy source", call. = FALSE)
  }
  # Four cryptographically secure bytes per deviate.
  w <- as.numeric(openssl::rand_bytes(4L * m))
  b <- matrix(w, nrow = 4L)
  # One 32-bit value per deviate; the +0.5 keeps it strictly inside (0, 1) so
  # the log in Box-Muller is finite. Doubles throughout to avoid integer
  # overflow at 255 * 2^24.
  ints <- b[1L, ] * 16777216 + b[2L, ] * 65536 + b[3L, ] * 256 + b[4L, ]
  (ints + 0.5) / 4294967296
}

#' Gaussian noise from OS entropy, by Box-Muller.
#' @param n how many deviates.
#' @param sd the standard deviation.
#' @keywords internal
#' @noRd
.dp_noise <- function(n, sd) {
  if (n <= 0L) return(numeric(0))
  m <- 2L * ceiling(n / 2)
  u <- .dp_os_uniform(m)
  r <- sqrt(-2 * log(u[seq(1L, m, by = 2L)]))
  a <- 2 * pi * u[seq(2L, m, by = 2L)]
  (sd * c(rbind(r * cos(a), r * sin(a))))[seq_len(n)]
}

#' The DP configuration the DATA OWNER set. The analyst sets none of it.
#' @keywords internal
#' @noRd
.dp_config <- function() {
  num <- function(name, default) {
    v <- suppressWarnings(as.numeric(.nlds_opt(name, default)))
    if (length(v) != 1L || !is.finite(v)) {
      stop("FAILED: option ", name, " is not a single finite number",
           call. = FALSE)
    }
    v
  }
  dir <- .nlds_opt("dsNoLimits.dp.ledgerDir", "")
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    stop("FAILED: this server is not provisioned for differentially private ",
         "analysis: the data owner has not set dsNoLimits.dp.ledgerDir, so the ",
         "privacy budget cannot be accounted", call. = FALSE)
  }
  mode <- .nlds_opt("dsNoLimits.dp.clipMode", "per-group")
  if (!is.character(mode) || length(mode) != 1L ||
      !(mode %in% c("per-group", "joint"))) {
    stop("FAILED: option dsNoLimits.dp.clipMode must be 'per-group' or 'joint'",
         call. = FALSE)
  }
  cfg <- list(ledger.dir = dir,
              clip = num("dsNoLimits.dp.clip", 20),
              clip.mode = mode,
              sigma = num("dsNoLimits.dp.noiseMultiplier", 1),
              budget = num("dsNoLimits.dp.epsilonBudget", 10),
              delta = num("dsNoLimits.dp.delta", 1e-5),
              max.t = num("dsNoLimits.dp.maxT", 200))
  if (cfg$clip <= 0 || cfg$sigma <= 0 || cfg$budget <= 0 || cfg$max.t < 1) {
    stop("FAILED: the differential-privacy options are invalid: ",
         "dp.clip, dp.noiseMultiplier and dp.epsilonBudget must be positive ",
         "and dp.maxT at least 1", call. = FALSE)
  }
  if (cfg$delta <= 0 || cfg$delta >= 1) {
    stop("FAILED: option dsNoLimits.dp.delta must be strictly between 0 and 1",
         call. = FALSE)
  }
  cfg
}

#' Fingerprint of an analysis: the data contents plus the model source.
#'
#' Digesting the canonical CONTENTS, not the symbol name or the layout, is what
#' stops a fresh budget being minted by reordering columns or rows, renaming a
#' symbol, or cosmetically editing the model. Columns are ordered by name and
#' rows by a deterministic content key before hashing, so a permutation of either
#' collapses to the same key. Genuinely different DATA still legitimately gets a
#' new budget - that is correct behaviour, not a loophole - and it is stated in
#' DISCLOSURE.md section 9.
#' @keywords internal
#' @noRd
.dp_fingerprint <- function(df, model.source) {
  df <- df[order(names(df))]
  df <- df[do.call(order, unname(as.list(df))), , drop = FALSE]
  digest::digest(list(names(df), unname(lapply(df, function(x) x)),
                      .nlds_canon_model(model.source)),
                 algo = "sha256")
}

#' @keywords internal
#' @noRd
.dp_ledger_path <- function(dir, fingerprint) {
  file.path(dir, paste0("dsnolimits-", fingerprint, ".csv"))
}

#' The noise multipliers of every release already charged to this analysis.
#' @keywords internal
#' @noRd
.dp_ledger_sigmas <- function(path) {
  if (!file.exists(path)) return(numeric(0))
  rec <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!nrow(rec) || is.null(rec$sigma)) return(numeric(0))
  as.numeric(rec$sigma)
}

#' Check the budget and charge one release, under a file lock.
#'
#' WRITE-AHEAD: the record is appended BEFORE the caller is allowed to build the
#' release, so a response lost in transit is still counted as spent. That is the
#' conservative direction - the alternative loses budget accounting on every
#' dropped connection.
#'
#' The lock makes concurrent sessions serialise on the read-check-append, which
#' is the only sequence that has to be atomic; two analysts logging in at once
#' therefore cannot both pass a check against the same remaining budget.
#'
#' @return a list with `releases`, `epsilon` (after this release) and
#'   `remaining`.
#' @keywords internal
#' @noRd
.dp_ledger_charge <- function(cfg, fingerprint, clip.total) {
  if (!dir.exists(cfg$ledger.dir) &&
      !dir.create(cfg$ledger.dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("FAILED: this server's differential-privacy ledger directory does ",
         "not exist and could not be created", call. = FALSE)
  }
  path <- .dp_ledger_path(cfg$ledger.dir, fingerprint)
  lock <- filelock::lock(paste0(path, ".lock"), timeout = 30000)
  if (is.null(lock)) {
    stop("FAILED: the differential-privacy ledger is locked by another ",
         "session; try again", call. = FALSE)
  }
  on.exit(filelock::unlock(lock), add = TRUE)

  sigmas <- .dp_ledger_sigmas(path)
  spent <- .dp_epsilon(sigmas, cfg$delta)
  after <- .dp_epsilon(c(sigmas, cfg$sigma), cfg$delta)
  # Gate on the POST-release epsilon, not on what is spent so far: refuse any
  # release that WOULD push cumulative epsilon over the cap, so realized spend
  # never exceeds the budget. The pre-release gate granted the overshooting
  # release and only refused the one after it.
  if (after > cfg$budget) {
    stop("FAILED: the differentially private budget for this data set is ",
         "exhausted (epsilon spent ", signif(spent, 4), " of ",
         signif(cfg$budget, 4), "; this release would reach ", signif(after, 4),
         " at delta ", signif(cfg$delta, 3),
         "); no further releases are possible", call. = FALSE)
  }
  new <- data.frame(time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
                    sigma = cfg$sigma, delta = cfg$delta,
                    clip = clip.total, mode = cfg$clip.mode,
                    stringsAsFactors = FALSE)
  utils::write.table(new, path, sep = ",", row.names = FALSE,
                     col.names = !file.exists(path), append = file.exists(path),
                     qmethod = "double")
  list(releases = length(sigmas) + 1L,
       epsilon = after,
       remaining = max(0, cfg$budget - after))
}

#' The per-subject gradient rows for one DP round, in preconditioned
#' transformed coordinates.
#'
#' @param prep the fit cache.
#' @param theta.vec the transformed-scale parameter vector.
#' @param est the estimator, one of laplace / focei / ghq / mle.
#' @param level the quadrature level.
#' @return a numeric matrix with one row per subject.
#' @keywords internal
#' @noRd
.dp_subject_gradients <- function(prep, theta.vec, est, level) {
  p <- as.integer(prep$p)
  flat <- tryCatch({
    if (est == "mle") {
      as.numeric(.nlds_fun("nlds_dp_individuals")(prep$dm, theta.vec))
    } else {
      as.numeric(.nlds_fun("nlds_dp_batches")(
        prep$dm, theta.vec, .nlds_fun("nlds_method")(est, level)))
    }
  }, error = function(e) {
    stop("FAILED: the per-subject gradients could not be evaluated on this ",
         "server: ", .nlds_brief(e), call. = FALSE)
  })
  if (length(flat) < 2L) {
    stop("FAILED: the per-subject gradients could not be evaluated on this ",
         "server: unexpected result length", call. = FALSE)
  }
  nb <- as.integer(flat[1L])
  max.ids <- as.integer(flat[2L])
  if (is.na(nb) || nb < 1L || length(flat) != 2L + nb * p) {
    stop("FAILED: the per-subject gradients could not be evaluated on this ",
         "server: unexpected result length", call. = FALSE)
  }
  # A per-subject sensitivity bound only exists when the clipping unit IS the
  # subject. If a random-effect level groups several subjects into one batch,
  # add/remove-one-subject is NOT bounded by the clip, so refuse rather than
  # release something the accounting does not cover.
  if (max.ids != 1L || nb != as.integer(prep$n.subjects)) {
    stop("FAILED: differentially private analysis needs one independence unit ",
         "per subject, but this model groups subjects together; no per-subject ",
         "sensitivity bound exists for it", call. = FALSE)
  }
  m <- matrix(flat[-(1:2)], nrow = nb, ncol = p)
  # A non-finite per-subject contribution is treated as a clipped zero-gradient
  # row: it would be clipped to norm C anyway and zero is inside the ball, so the
  # release proceeds, noise is added and the ledger is charged normally. A free,
  # uncharged error here would let an analyst probe numerical instability without
  # spending budget - in DP mode every well-formed query must cost budget.
  bad <- !is.finite(rowSums(m))
  if (any(bad)) m[bad, ] <- 0
  m
}

#' The clipped, UN-NOISED site sum. Test-only; never reachable from the client.
#'
#' Exposed so the test suite can verify the two properties the released value
#' inherits: that the sum is the preconditioned gradient when the clip does not
#' bind, and that dropping one subject moves it by at most C_total.
#' @keywords internal
#' @noRd
.dp_clipped_sum <- function(prep, theta.vec, est, level, clip, clip.mode) {
  g <- .dp_subject_gradients(prep, theta.vec, est, level)
  spec <- .dp_clip_spec(as.character(prep$names), clip, clip.mode)
  .dp_clip_sum(g, spec$ids, spec$clips)
}

#' Decode a `[n; ncol; vec(matrix)]` flat Julia vector into an n x ncol matrix.
#'
#' The EM DP Julia helpers (nlds_mcem_dp_rows, nlds_saem_stats_dp_rows) return one
#' flat column-major vector per this layout, mirroring nlds_dp_batches.
#' @keywords internal
#' @noRd
.dp_flat_matrix <- function(flat, what) {
  flat <- as.numeric(flat)
  if (length(flat) < 2L) {
    stop("FAILED: ", what, " could not be evaluated on this server: ",
         "unexpected result length", call. = FALSE)
  }
  n <- as.integer(flat[1L])
  k <- as.integer(flat[2L])
  if (is.na(n) || is.na(k) || n < 1L || k < 1L || length(flat) != 2L + n * k) {
    stop("FAILED: ", what, " could not be evaluated on this server: ",
         "unexpected result length", call. = FALSE)
  }
  matrix(flat[-(1:2)], nrow = n, ncol = k)
}

#' Clip (JOINT, per-subject), charge the ledger, and noise one EM DP release.
#'
#' Shared by nolimitsEmQDpDS and nolimitsSaemStatsDpDS. The EM releases clip each
#' subject's whole row in one L2 ball of radius `dp.clip` (joint clip: the RE-
#' moment, outcome and count fields differ in magnitude, so per-group splitting is
#' not meaningful for the stats vector, and the M-step gradient part is one block
#' anyway), so the site sum has sensitivity `dp.clip` and isotropic noise at
#' `sigma * dp.clip` is the Gaussian mechanism at multiplier `sigma`. A non-finite
#' subject row is treated as a clipped zero row: the query still costs budget, as
#' every well-formed DP query must. Charges write-ahead, before the value exists.
#' @param rows numeric matrix, one row per subject.
#' @return a list with the noised sum in `value` plus the ledger fields.
#' @keywords internal
#' @noRd
.dp_em_release <- function(cfg, fingerprint, rows) {
  rows <- as.matrix(rows)
  k <- ncol(rows)
  bad <- !is.finite(rowSums(rows))
  if (any(bad)) rows[bad, ] <- 0
  clipped <- .dp_clip_sum(rows, rep(1L, k), cfg$clip)
  charge <- .dp_ledger_charge(cfg, fingerprint, cfg$clip)
  list(value = clipped + .dp_noise(k, cfg$sigma * cfg$clip),
       releases = as.integer(charge$releases),
       epsilon = charge$epsilon,
       remaining.budget = charge$remaining,
       delta = cfg$delta)
}

#' Resolve the coordinate-to-group split and the per-group clips.
#' @keywords internal
#' @noRd
.dp_clip_spec <- function(names, clip, clip.mode) {
  if (identical(clip.mode, "joint")) {
    return(list(ids = rep(1L, length(names)), clips = clip,
                total = clip, groups = "joint"))
  }
  ids <- .dp_groups(names)
  groups <- attr(ids, "groups")
  clips <- rep(clip, length(groups))
  list(ids = as.integer(ids), clips = clips, total = .dp_clip_total(clips),
       groups = groups)
}
