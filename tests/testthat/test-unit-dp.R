# The accountant, the clipping, the noise source and the ledger. The accountant
# assertions are ported verbatim from the reference implementation this design
# comes from, which tested them against the closed forms below.

test_that("the accountant matches the analytic Gaussian mechanism at T = 1", {
  eps <- .dp_epsilon(1, 1e-5)
  expect_equal(eps, 5.2985, tolerance = 1e-3)
  classical <- sqrt(2 * log(1.25 / 1e-5))
  expect_lt(classical, eps)
  expect_lt(eps, 1.15 * classical)
  # Hand-computed optimum of a/2 + ln(1e5)/(a-1) at a = 1 + sqrt(2 ln(1e5)).
  a <- 1 + sqrt(2 * log(1e5))
  expect_equal(eps, a / 2 + log(1e5) / (a - 1), tolerance = 1e-3)
})

test_that("the accountant composes over rounds and falls with sigma", {
  cases <- list(list(1, 1.0, 1e-5, 5.2985),
                list(50, 1.0, 1e-5, 58.9308),
                list(50, 4.0, 1e-5, 10.0452),
                list(1, 4.0, 1e-6, 1.3454))
  worst <- 0
  for (cs in cases) {
    rounds <- cs[[1L]]; sigma <- cs[[2L]]; delta <- cs[[3L]]; expected <- cs[[4L]]
    got <- .dp_epsilon(rep(sigma, rounds), delta)
    expect_equal(got, expected, tolerance = 1e-3)
    # Closed form of the same minimisation, as an independent check of the grid.
    cc <- rounds / (2 * sigma^2)
    a <- 1 + sqrt(log(1 / delta) / cc)
    closed <- cc * a + log(1 / delta) / (a - 1)
    expect_equal(got, closed, tolerance = 2e-3)
    worst <- max(worst, abs(got - expected) / expected,
                 abs(got - closed) / closed)
  }
  cat(sprintf("\n[dp] worst accountant-vs-closed-form relative difference = %.3e\n",
              worst))
})

test_that("the accountant composes a heterogeneous history and is monotone", {
  delta <- 1e-5
  expect_equal(.dp_epsilon(numeric(0), delta), 0)
  # A heterogeneous history is exactly the homogeneous one when the sigmas agree.
  expect_equal(.dp_epsilon(c(2, 2, 2), delta), .dp_epsilon(rep(2, 3), delta))
  # Two releases at different noise multipliers cost strictly more than either
  # one alone, and strictly less than two at the noisier setting.
  mixed <- .dp_epsilon(c(1, 4), delta)
  expect_gt(mixed, .dp_epsilon(1, delta))
  expect_gt(mixed, .dp_epsilon(4, delta))
  expect_lt(mixed, .dp_epsilon(c(1, 1), delta))
  # Monotone in the number of releases, whatever the mix.
  hist <- c(1, 3, 0.7, 2, 5)
  eps <- vapply(seq_along(hist), function(k) .dp_epsilon(hist[seq_len(k)], delta),
                numeric(1))
  expect_true(all(diff(eps) > 0))
  # ... and falling in delta at fixed history.
  expect_gt(.dp_epsilon(hist, 1e-9), .dp_epsilon(hist, 1e-3))
})

test_that("the accountant rejects an unusable ledger or delta", {
  expect_error(.dp_epsilon(c(1, -1), 1e-5), "FAILED: the privacy ledger")
  expect_error(.dp_epsilon(c(1, NA), 1e-5), "FAILED: the privacy ledger")
  expect_error(.dp_epsilon(1, 0), "FAILED: dp.delta")
  expect_error(.dp_epsilon(1, 1), "FAILED: dp.delta")
})

test_that("the group heuristic separates variance from location parameters", {
  ids <- .dp_groups(c("cl", "v", "omega", "sigma"))
  expect_equal(attr(ids, "groups"), c("location", "variance"))
  expect_equal(as.integer(ids), c(1L, 1L, 2L, 2L))
  ids2 <- .dp_groups(c("ka", "cov_ka_cl", "sd_v", "rho", "tau2"))
  expect_equal(as.integer(ids2), c(1L, 2L, 2L, 2L, 2L))
  # Every parameter lands in exactly one group, always.
  expect_length(attr(.dp_groups("cl"), "groups"), 1L)
})

test_that("clipping bounds the sum and the add/remove-one-subject delta", {
  g <- rbind(c(3, 4), c(0.6, 0.8), c(0, 0))
  total <- .dp_clip_sum(g, c(1L, 1L), 2)
  expect_equal(total, c(1.2 + 0.6, 1.6 + 0.8))
  set.seed(11)
  for (clip in c(0.1, 1, 7)) {
    rows <- matrix(stats::rnorm(44, 0, 50), nrow = 11)
    ids <- rep(1L, 4)
    expect_lte(sqrt(sum(.dp_clip_sum(rows, ids, clip)^2)), 11 * clip + 1e-9)
    expect_lte(sqrt(sum((.dp_clip_sum(rows, ids, clip) -
                           .dp_clip_sum(rows[-11, , drop = FALSE], ids, clip))^2)),
               clip + 1e-9)
  }
})

test_that("per-group clipping bounds each block and the total sensitivity", {
  ids <- c(1L, 1L, 2L, 2L)
  clips <- c(1, 3)
  c.total <- .dp_clip_total(clips)
  expect_equal(c.total, sqrt(1 + 9))
  set.seed(12)
  rows <- matrix(stats::rnorm(40, 0, 20), nrow = 10)
  one <- .dp_clip_sum(rows[1L, , drop = FALSE], ids, clips)
  expect_lte(sqrt(sum(one[1:2]^2)), clips[1L] + 1e-9)
  expect_lte(sqrt(sum(one[3:4]^2)), clips[2L] + 1e-9)
  dropped <- .dp_clip_sum(rows[-10, , drop = FALSE], ids, clips)
  expect_lte(sqrt(sum((.dp_clip_sum(rows, ids, clips) - dropped)^2)),
             c.total + 1e-9)
})

test_that("the noise is fresh and never touches the session RNG stream", {
  first <- .dp_noise(4000, 1)
  second <- .dp_noise(4000, 1)
  expect_false(identical(first, second))
  expect_equal(stats::sd(first), 1, tolerance = 0.1)
  expect_equal(mean(first), 0, tolerance = 0.1)
  expect_equal(stats::sd(.dp_noise(4000, 2.5)), 2.5, tolerance = 0.1)
  expect_length(.dp_noise(7, 1), 7L)
  expect_length(.dp_noise(0, 1), 0L)

  # The session's own RNG state must be bit-identical before and after: a DP
  # release may not perturb an analyst's seeded work, and may not consume it.
  set.seed(99)
  invisible(stats::runif(1))
  before <- get(".Random.seed", envir = .GlobalEnv)
  invisible(.dp_noise(1000, 3))
  expect_identical(get(".Random.seed", envir = .GlobalEnv), before)
  # Two identical seeded draws still agree across a DP release in between.
  set.seed(5); a <- stats::rnorm(3)
  set.seed(5); invisible(.dp_noise(10, 1)); b <- stats::rnorm(3)
  expect_identical(a, b)
})

test_that("the DP configuration is owner-set and refuses to run unprovisioned", {
  withr::with_options(list(dsNoLimits.dp.ledgerDir = NULL,
                           default.dsNoLimits.dp.ledgerDir = ""), {
    expect_error(.dp_config(), "not provisioned for differentially private")
  })
  dir <- withr::local_tempdir()
  withr::with_options(list(default.dsNoLimits.dp.ledgerDir = dir), {
    cfg <- .dp_config()
    expect_equal(cfg$clip, 20)
    expect_equal(cfg$clip.mode, "per-group")
    expect_equal(cfg$sigma, 1)
    expect_equal(cfg$delta, 1e-5)
  })
  withr::with_options(list(default.dsNoLimits.dp.ledgerDir = dir,
                           dsNoLimits.dp.clipMode = "elementwise"), {
    expect_error(.dp_config(), "clipMode must be")
  })
  withr::with_options(list(default.dsNoLimits.dp.ledgerDir = dir,
                           dsNoLimits.dp.clip = 0), {
    expect_error(.dp_config(), "options are invalid")
  })
  withr::with_options(list(default.dsNoLimits.dp.ledgerDir = dir,
                           dsNoLimits.dp.delta = 1), {
    expect_error(.dp_config(), "delta must be strictly between")
  })
  # The live name wins over the default.-prefixed one, as everywhere else.
  withr::with_options(list(default.dsNoLimits.dp.ledgerDir = dir,
                           default.dsNoLimits.dp.clip = 5,
                           dsNoLimits.dp.clip = 9), {
    expect_equal(.dp_config()$clip, 9)
  })
})

test_that("the fingerprint is canonical: layout-invariant, content-sensitive", {
  d1 <- data.frame(ID = c(1, 1, 2), t = c(0, 1, 0), y = c(0.5, 0.4, 0.3))
  model <- "conc ~ Normal(Dose/v, sigma)"
  fp <- .dp_fingerprint(d1, model)

  # Re-uploading the same table under a new symbol is the same analysis.
  expect_identical(fp, .dp_fingerprint(d1, model))
  # Reordering COLUMNS (by name) collapses to the same key.
  expect_identical(fp, .dp_fingerprint(d1[, c("y", "ID", "t")], model))
  # Reordering ROWS collapses to the same key.
  expect_identical(fp, .dp_fingerprint(d1[3:1, ], model))
  # A cosmetic model edit - an added comment, reflowed whitespace - is the same.
  expect_identical(fp, .dp_fingerprint(d1, "conc ~ Normal(Dose/v, sigma) # note"))
  expect_identical(fp, .dp_fingerprint(d1, "conc  ~\n  Normal(Dose/v,   sigma)\n"))

  # Genuinely different DATA legitimately gets a fresh budget: one altered cell.
  d3 <- d1; d3$y[1L] <- 0.500001
  expect_false(identical(fp, .dp_fingerprint(d3, model)))
  # Renaming a column moves the value-to-name mapping: a real content change.
  d4 <- d1; names(d4)[3L] <- "conc"
  expect_false(identical(fp, .dp_fingerprint(d4, model)))
  # A genuinely different model is different.
  expect_false(identical(fp, .dp_fingerprint(d1, "conc ~ Normal(Dose/cl, sigma)")))
})

test_that("the budget gates on POST-release epsilon and never overshoots", {
  dir <- withr::local_tempdir()
  # A budget that admits exactly k = 2 releases at sigma 1: strictly above the
  # cost of two, strictly below the cost of three.
  e2 <- .dp_epsilon(rep(1, 2), 1e-5)
  e3 <- .dp_epsilon(rep(1, 3), 1e-5)
  budget <- (e2 + e3) / 2
  cfg <- list(ledger.dir = dir, sigma = 1, delta = 1e-5, budget = budget,
              clip.mode = "joint")
  fp <- "post-release-gate"

  expect_equal(.dp_ledger_charge(cfg, fp, 20)$releases, 1L)
  expect_equal(.dp_ledger_charge(cfg, fp, 20)$releases, 2L)
  # The (k+1)th release is refused - a pre-release gate would have granted it and
  # only refused the one after, overshooting the cap by one release.
  expect_error(.dp_ledger_charge(cfg, fp, 20),
               "budget for this data set is exhausted")
  # Realized cumulative epsilon never exceeded the cap.
  realized <- .dp_epsilon(.dp_ledger_sigmas(.dp_ledger_path(dir, fp)), 1e-5)
  expect_lte(realized, budget)
  expect_equal(realized, e2)
})

test_that("the ledger charges write-ahead, persists and refuses when exhausted", {
  dir <- withr::local_tempdir()
  cfg <- list(ledger.dir = dir, sigma = 1, delta = 1e-5, budget = 1e6,
              clip.mode = "joint")
  fp <- "fingerprint-a"
  first <- .dp_ledger_charge(cfg, fp, 20)
  expect_equal(first$releases, 1L)
  expect_equal(first$epsilon, .dp_epsilon(1, 1e-5))
  expect_equal(first$remaining, 1e6 - first$epsilon)
  path <- .dp_ledger_path(dir, fp)
  expect_true(file.exists(path))
  expect_length(.dp_ledger_sigmas(path), 1L)

  # WRITE-AHEAD: a release that dies after the charge is still spent. The charge
  # returns before the caller builds anything, so simulating the failure is just
  # discarding the result - and the next charge still sees the record.
  invisible(tryCatch({
    .dp_ledger_charge(cfg, fp, 20)
    stop("the release failed after the ledger was written")
  }, error = function(e) NULL))
  expect_equal(.dp_ledger_charge(cfg, fp, 20)$releases, 3L)

  # A different data set has a separate, untouched budget.
  expect_equal(.dp_ledger_charge(cfg, "fingerprint-b", 20)$releases, 1L)

  # The budget is enforced against the ledger, not against any session counter,
  # so a fresh "session" reading the same directory sees the same spend.
  tight <- utils::modifyList(cfg, list(budget = 1.1 * .dp_epsilon(1, 1e-5)))
  expect_error(.dp_ledger_charge(tight, fp, 20), "budget for this data set is exhausted")
  expect_equal(length(.dp_ledger_sigmas(path)), 3L)  # a refusal charges nothing

  # A heterogeneous history composes: the same ledger at another sigma.
  hetero <- utils::modifyList(cfg, list(sigma = 4))
  got <- .dp_ledger_charge(hetero, fp, 20)
  expect_equal(got$epsilon, .dp_epsilon(c(1, 1, 1, 4), 1e-5))
})

test_that("the ledger serialises concurrent sessions under a file lock", {
  skip_on_cran()
  skip_if_not_installed("callr")
  dir <- withr::local_tempdir()
  # The workers use the INSTALLED package, not a source load: under R CMD check
  # the test files run from a copy inside the .Rcheck directory, where no
  # package source is reachable by a relative path.
  worker <- function(dir, n) {
    charge <- utils::getFromNamespace(".dp_ledger_charge", "dsNoLimits")
    cfg <- list(ledger.dir = dir, sigma = 1, delta = 1e-5, budget = 1e9,
                clip.mode = "joint")
    for (i in seq_len(n)) {
      charge(cfg, "shared", 20)
      Sys.sleep(0.005)
    }
    TRUE
  }
  a <- callr::r_bg(worker, list(dir = dir, n = 15))
  b <- callr::r_bg(worker, list(dir = dir, n = 15))
  a$wait(120000); b$wait(120000)
  expect_equal(a$get_exit_status(), 0L)
  expect_equal(b$get_exit_status(), 0L)
  # Every one of the 30 charges landed, and the file is still parseable: an
  # interleaved append under no lock loses or corrupts records.
  expect_equal(length(.dp_ledger_sigmas(.dp_ledger_path(dir, "shared"))), 30L)
})
