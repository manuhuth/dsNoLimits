test_that("nolimitsObjGradDS validates every argument before evaluating", {
  withr::local_options(nlds_fit_options())
  prep <- list(dm = 1, theta0 = rep(0, 4), names = letters[1:4], scale = rep(1, 4),
               p = 4L, n.subjects = 6L, n.obs = 36L, model.hash = "x",
               versions = list())
  th <- .ds_num_encode(rep(0, 4))

  expect_error(nolimitsObjGradDS("prep", 1, "laplace", 1L, 1L),
               "theta must be a single character string")
  expect_error(nolimitsObjGradDS("prep", .ds_num_encode(rep(0, 3)), "laplace", 1L, 1L),
               "must decode to 4 numeric values")
  expect_error(nolimitsObjGradDS("prep", "not,a,number,here", "laplace", 1L, 1L),
               "must decode to 4 numeric values")
  expect_error(nolimitsObjGradDS("prep", th, "saem", 1L, 1L),
               "must be one of laplace, focei, ghq, pooled, mle, map")
  expect_error(nolimitsObjGradDS("prep", th, 3, 1L, 1L),
               "estimator must be a single character string")
  expect_error(nolimitsObjGradDS("prep", th, "laplace", 0L, 1L),
               "ghq.level must be an integer between 1 and 9")
  expect_error(nolimitsObjGradDS("prep", th, "laplace", 10L, 1L),
               "ghq.level must be an integer between 1 and 9")
  expect_error(nolimitsObjGradDS("prep", th, "laplace", 1L, 0L),
               "round must be an integer between 1 and 500")

  expect_error(nolimitsObjGradDS("prep", th, "laplace", 1.5, 1),
               "ghq.level must be an integer between 1 and 9")
  expect_error(nolimitsObjGradDS("prep", th, "laplace", 1, "2"),
               "round must be an integer between 1 and 500")

  withr::local_options(list(dsNoLimits.maxRounds = 5L))
  expect_error(nolimitsObjGradDS("prep", th, "laplace", 1L, 6L),
               "round must be an integer between 1 and 5")
})

test_that("whole-numbered doubles are accepted for ghq.level and round", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()
  th <- .ds_num_encode(prep$theta0)

  # The client cannot emit integer literals through the Opal grammar.
  expect_equal(nolimitsObjGradDS("prep", th, "ghq", 1, 3)$value,
               nolimitsObjGradDS("prep", th, "ghq", 1L, 3L)$value)
})

test_that("nolimitsObjGradDS splits the flat Julia result and releases nothing per-subject", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()

  res <- nolimitsObjGradDS("prep", .ds_num_encode(prep$theta0), "laplace", 1L, 1L)

  expect_named(res, c("value", "grad", "natural", "finite", "n.subjects"))
  expect_true(res$finite)
  expect_true(is.finite(res$value))
  expect_length(res$grad, 4L)
  expect_true(all(is.finite(res$grad)))
  # natural is a deterministic model transform of the argument, not a data value.
  expect_equal(res$natural, c(1, 10, 0.3, 0.5), tolerance = 1e-10)
  expect_equal(res$n.subjects, 6L)
  expect_true(all(lengths(res) <= 4L))
  expect_identical(unserialize(serialize(res, NULL)), res)
})

test_that("nolimitsObjGradDS serves all four estimators", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()
  th <- .ds_num_encode(prep$theta0)

  for (est in c("laplace", "focei", "ghq", "pooled")) {
    res <- nolimitsObjGradDS("prep", th, est, 1L, 1L)
    expect_true(res$finite, info = est)
    expect_true(is.finite(res$value), info = est)
  }
  # The estimators are genuinely different objectives.
  expect_false(isTRUE(all.equal(
    nolimitsObjGradDS("prep", th, "laplace", 1L, 1L)$value,
    nolimitsObjGradDS("prep", th, "pooled", 1L, 1L)$value)))
})

test_that("a non-finite objective is flagged, never an error, and carries no payload", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()

  # An absurd transformed omega drives the Laplace expansion out of its domain.
  bad <- prep$theta0
  bad[3L] <- 50
  res <- suppressWarnings(
    nolimitsObjGradDS("prep", .ds_num_encode(bad), "laplace", 1L, 1L))

  expect_false(res$finite)
  expect_true(is.na(res$value))
  expect_equal(res$grad, rep(0, 4))
  expect_true(all(is.finite(res$natural)))
})

test_that("a thrown Julia error becomes a FAILED message", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  broken <- nlds_get_prep()
  broken$dm <- 42

  expect_error(
    nolimitsObjGradDS("broken", .ds_num_encode(broken$theta0), "laplace", 1L, 1L),
    "FAILED: the objective could not be evaluated on this server")
})

## ------------------------------------------------------------- mle / map ----
## The no-random-effects estimators. `map` is the only one whose objective is not
## purely additive over servers, so the server splits it: value/grad carry the
## likelihood only and the log-prior travels as its own block.

# The declared priors of NLDS_TEST_MODEL_NORE, evaluated in R at the
# natural-scale parameters. Independent of NoLimits' own logprior.
nlds_r_logprior <- function(nat) {
  sum(stats::dlnorm(nat[1L], log(1), 0.5, log = TRUE),
      stats::dlnorm(nat[2L], log(10), 0.5, log = TRUE),
      stats::dlnorm(nat[3L], log(0.5), 0.5, log = TRUE))
}

test_that("mle and map evaluate on a no-random-effects model", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep_nore()
  th <- .ds_num_encode(prep$theta0)

  expect_equal(prep$p, 3L)
  expect_equal(prep$names, c("cl", "v", "sigma"))

  mle <- nolimitsObjGradDS("prep", th, "mle", 1L, 1L)
  expect_named(mle, c("value", "grad", "natural", "finite", "n.subjects"))
  expect_true(mle$finite)
  expect_true(is.finite(mle$value))
  expect_length(mle$grad, 3L)

  map <- nolimitsObjGradDS("prep", th, "map", 1L, 1L)
  expect_named(map, c("value", "grad", "natural", "finite", "n.subjects",
                      "prior.value", "prior.grad"))
  expect_length(map$prior.grad, 3L)
  expect_true(all(is.finite(map$prior.grad)))
  # Nothing per-subject: every element is at most p long.
  expect_true(all(lengths(map) <= 3L))
  expect_identical(unserialize(serialize(map, NULL)), map)

  # value/grad are the LIKELIHOOD-ONLY part: identical to the mle arm.
  expect_equal(map$value, mle$value)
  expect_equal(map$grad, mle$grad)
})

test_that("the prior block is the log-prior at the natural theta", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep_nore()

  for (th.t in list(prep$theta0, prep$theta0 + c(0.2, -0.5, 0.3))) {
    res <- nolimitsObjGradDS("prep", .ds_num_encode(th.t), "map", 1L, 1L)
    expect_equal(res$prior.value, nlds_r_logprior(res$natural), tolerance = 1e-10)

    # Its transformed-scale gradient, by central differences of that same R
    # computation through the server's own natural transform.
    # Every *DS call stays in THIS frame, where `prep` lives: a *DS function
    # resolves its argument from parent.frame() with inherits = FALSE, so a
    # vapply() callback frame would not see the cache.
    h <- 1e-5
    fd <- numeric(3L)
    for (j in seq_len(3L)) {
      zp <- th.t
      zp[j] <- zp[j] + h
      zm <- th.t
      zm[j] <- zm[j] - h
      up <- nolimitsObjGradDS("prep", .ds_num_encode(zp), "map", 1L, 1L)$natural
      dn <- nolimitsObjGradDS("prep", .ds_num_encode(zm), "map", 1L, 1L)$natural
      fd[j] <- (nlds_r_logprior(up) - nlds_r_logprior(dn)) / (2 * h)
    }
    expect_equal(res$prior.grad, fd, tolerance = 1e-6)
  }
})

test_that("the prior block is data-free: the same model on less data gives the same block", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  writeLines(NLDS_TEST_MODEL_NORE,
             file.path(tempdir(), "nlds-registry", "onecomp_nore.jl"))
  withr::local_options(list(dsNoLimits.modelDir =
                              file.path(tempdir(), "nlds-registry")))

  D <- nlds_test_data()
  prep <- nolimitsPrepDS("D", "onecomp_nore", "ID", "t")
  D <- D[D$ID %in% utils::head(unique(D$ID), 3L), , drop = FALSE]
  prep.small <- nolimitsPrepDS("D", "onecomp_nore", "ID", "t")

  th <- .ds_num_encode(prep$theta0)
  a <- nolimitsObjGradDS("prep", th, "map", 1L, 1L)
  b <- nolimitsObjGradDS("prep.small", th, "map", 1L, 1L)

  expect_identical(a$prior.value, b$prior.value)
  expect_identical(a$prior.grad, b$prior.grad)
  # ... while the likelihood part is not the same, so the two really are
  # different amounts of data.
  expect_false(isTRUE(all.equal(a$value, b$value)))
})

test_that("mle and map on a random-effects model fail with a legible message", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  prep <- nlds_get_prep()          # the random-effects fixture
  th <- .ds_num_encode(prep$theta0)

  for (est in c("mle", "map")) {
    e <- tryCatch(nolimitsObjGradDS("prep", th, est, 1L, 1L),
                  error = function(e) e)
    expect_s3_class(e, "error")
    expect_match(conditionMessage(e),
                 "FAILED: the objective could not be evaluated on this server")
    # The Julia reason is no longer relayed verbatim (it could embed a
    # data-derived value); a fixed non-disclosive stand-in is returned instead.
    expect_match(conditionMessage(e), "see server logs")
    expect_false(grepl("random effects", conditionMessage(e)))
    # The trap fires with a message and nothing else.
    expect_named(e, c("message", "call"))
  }
})

test_that("nolimitsNaturalDS is the model transform of its argument alone", {
  nlds_skip_no_julia()
  withr::local_options(nlds_fit_options())
  P <- nlds_get_prep()
  theta <- P$theta0
  res <- nolimitsNaturalDS("P", .ds_num_encode(theta))
  expect_named(res, "natural")
  expect_length(res$natural, as.integer(P$p))
  # The SAME vector the non-private round already returns inside `natural`.
  expect_identical(res$natural,
                   nolimitsObjGradDS("P", .ds_num_encode(theta), "laplace", 1, 1)$natural)
  expect_identical(unserialize(serialize(res, NULL)), res)
  # Deterministic in its argument, and moving the argument moves the answer.
  expect_identical(res, nolimitsNaturalDS("P", .ds_num_encode(theta)))
  moved <- theta; moved[1L] <- moved[1L] + 0.25
  expect_false(identical(res$natural, nolimitsNaturalDS("P", .ds_num_encode(moved))$natural))
  expect_error(nolimitsNaturalDS("P", .ds_num_encode(c(1, 2))),
               "must decode to 4 numeric values")
  expect_error(nolimitsNaturalDS("P", 3), "single character string")
})
