test_that(".ds_get resolves symbols and $-paths and rejects everything else", {
  withr::local_options(ds_test_options())
  e <- new.env(parent = emptyenv())
  assign("D", data.frame(a = 1:3), envir = e)
  assign("L", list(fit = list(coefficients = c(1, 2))), envir = e)

  expect_equal(.ds_get("D", e, is.data.frame, "data frame")$a, 1:3)
  expect_equal(.ds_get("D$a", e), 1:3)
  expect_equal(.ds_get("L$fit$coefficients", e, is.numeric), c(1, 2))

  # A base R function must never masquerade as data (inherits = FALSE).
  expect_error(.ds_get("mean", e), "does not exist on this server")
  expect_error(.ds_get("D[['a']]", e), "not a plain symbol")
  expect_error(.ds_get("system('ls')", e), "not a plain symbol")
  expect_error(.ds_get("D$a; D", e), "not a plain symbol")
  expect_error(.ds_get(c("D", "L"), e), "single character string")
  expect_error(.ds_get(NA_character_, e), "single character string")
  expect_error(.ds_get("D$nope", e), "has no element")
  expect_error(.ds_get("D", e, is.numeric), "wrong type")
  expect_error(.ds_get(strrep("a", 21), e), "nfilter.stringShort")
})

test_that(".ds_get honours a raised nfilter.stringShort from the live option", {
  withr::local_options(ds_test_options(nfilter.stringShort = 40))
  e <- new.env(parent = emptyenv())
  nm <- strrep("a", 30)
  assign(nm, 1, envir = e)
  expect_equal(.ds_get(nm, e, is.numeric), 1)
})

test_that("the numeric codec round-trips nasty doubles exactly", {
  x <- c(0, -0, 1, -1, pi, .Machine$double.eps, .Machine$double.xmax,
         .Machine$double.xmin, 1e-300, -1e300, 1 / 3, 1e-8, 123456789.123456789)
  expect_identical(.ds_num_decode(.ds_num_encode(x)), x)
  expect_identical(.ds_num_decode(.ds_num_encode(1 / (1:500))), 1 / (1:500))

  # Non-finite values survive; NA re-parses with a coercion warning.
  expect_identical(.ds_num_decode(.ds_num_encode(c(Inf, -Inf, NaN))),
                   c(Inf, -Inf, NaN))
  expect_true(is.na(suppressWarnings(.ds_num_decode(.ds_num_encode(NA_real_)))))

  # The wire alphabet must be legal in both grammars.
  expect_false(grepl("[^0-9a-fpx+,.NaInf-]", .ds_num_encode(x)))
})

test_that("the base64url codec round-trips a model string in a legal alphabet", {
  # Well over 64 bytes: jsonlite::base64_enc line-wraps beyond that, and an
  # unstripped newline is a lexer error on Opal.
  expect_gt(nchar(NLDS_TEST_MODEL), 64L)
  enc <- .ds_encode(NLDS_TEST_MODEL)
  expect_match(enc, "^[A-Za-z0-9_-]+$")
  expect_false(grepl("[[:space:]]", enc))
  expect_identical(.ds_decode(enc), NLDS_TEST_MODEL)

  # Short and awkward payloads, and every length modulo 4.
  for (n in 1:12) {
    s <- strrep("y ~ x + (1|id)", n)
    expect_identical(.ds_decode(.ds_encode(s)), s)
  }
  # The decoder tolerates whitespace a transport may have reintroduced.
  expect_identical(.ds_decode(paste0(substr(enc, 1L, 10L), "\n",
                                     substr(enc, 11L, nchar(enc)))),
                   NLDS_TEST_MODEL)
  # An empty payload would be a lexer error on Opal, so it never gets encoded.
  expect_null(.ds_encode(""))
})

test_that(".nlds_is_whole accepts whole doubles as well as integers", {
  expect_true(.nlds_is_whole(5L))
  expect_true(.nlds_is_whole(5))
  expect_true(.nlds_is_whole(-1))
  expect_false(.nlds_is_whole(5.5))
  expect_false(.nlds_is_whole(NA_real_))
  expect_false(.nlds_is_whole(Inf))
  expect_false(.nlds_is_whole(c(1, 2)))
  expect_false(.nlds_is_whole("5"))
})

test_that(".nf reads the live threshold first and diagnoses an unset one", {
  withr::local_options(ds_test_options(nfilter.tab = 7))
  expect_equal(.nf("nfilter.tab"), 7)

  withr::local_options(list(nfilter.tab = NULL, default.nfilter.tab = 11))
  expect_equal(.nf("nfilter.tab"), 11)

  withr::local_options(list(nfilter.tab = NULL, default.nfilter.tab = NULL))
  expect_error(.nf("nfilter.tab"), "is not configured on this server")
})

test_that(".nlds_opt cascades live name then default. prefix", {
  withr::local_options(list(dsNoLimits.modelDir = "/live",
                            default.dsNoLimits.modelDir = "/def"))
  expect_equal(.nlds_opt("dsNoLimits.modelDir"), "/live")

  withr::local_options(list(dsNoLimits.modelDir = NULL))
  expect_equal(.nlds_opt("dsNoLimits.modelDir"), "/def")

  withr::local_options(list(default.dsNoLimits.modelDir = NULL))
  expect_equal(.nlds_opt("dsNoLimits.modelDir", "fallback"), "fallback")
})

test_that(".nlds_check_name accepts plain names only", {
  withr::local_options(ds_test_options())
  expect_silent(.nlds_check_name("ID", "id.col"))
  expect_error(.nlds_check_name("../../etc/passwd", "model.name"), "not a plain name")
  expect_error(.nlds_check_name("a$b", "model.name"), "not a plain name")
  expect_error(.nlds_check_name("", "model.name"), "single character string")
  expect_error(.nlds_check_name(strrep("m", 21), "model.name"), "nfilter.stringShort")
})

test_that(".nlds_is_prep only accepts a full cache", {
  expect_false(.nlds_is_prep(list(dm = 1)))
  expect_false(.nlds_is_prep(data.frame(a = 1)))
  expect_false(.nlds_is_prep(list(dm = 1, theta0 = 1, names = "a", scale = 1,
                                  p = 1, n.subjects = 1, n.obs = 1,
                                  versions = list())))
  expect_true(.nlds_is_prep(list(dm = 1, theta0 = 1, names = "a", scale = 1,
                                 p = 1, n.subjects = 1, n.obs = 1,
                                 model.hash = "x", versions = list())))
})

test_that(".nlds_brief never relays Julia message content to the client", {
  # A Julia DomainError/ingestion error can embed a data-derived value and a
  # server path in its printed form; .nlds_brief must relay none of it.
  sentinel <- "SECRET_DATA_9973.1428"
  server_path <- "/opt/pkg/NoLimits/src/foo.jl:42"
  e <- simpleError(paste0(
    "Error: DomainError with ", sentinel, ":\n",
    "Exp will only return a complex result if called with a complex argument.\n",
    "Stacktrace:\n [1] ", server_path))
  brief <- suppressMessages(.nlds_brief(e))
  expect_type(brief, "character")
  expect_length(brief, 1L)
  expect_false(grepl(sentinel, brief, fixed = TRUE))
  expect_false(grepl("DomainError", brief, fixed = TRUE))
  expect_false(grepl(server_path, brief, fixed = TRUE))
  # An injected value in the message is likewise not echoed.
  e2 <- simpleError("could not read column conc: value 424242.777 out of domain")
  expect_false(grepl("424242.777", suppressMessages(.nlds_brief(e2)), fixed = TRUE))
})

test_that(".nlds_model_hash is canonical and matches the DP fingerprint canonicalizer", {
  base <- .nlds_model_hash(NLDS_TEST_MODEL)
  expect_match(base, "^[0-9a-f]{64}$")

  # Comment-only and whitespace-only edits do not change the hash.
  expect_identical(base, .nlds_model_hash(paste0(NLDS_TEST_MODEL, "\n# a note\n")))
  reflowed <- gsub("\n", "\n   ", NLDS_TEST_MODEL, fixed = TRUE)
  expect_identical(base, .nlds_model_hash(reflowed))

  # A genuine content change does change it.
  expect_false(identical(
    base, .nlds_model_hash(sub("Dose / v", "Dose / v + 0.0", NLDS_TEST_MODEL,
                               fixed = TRUE))))

  # The ledger fingerprint and the agreement hash share one canonicalization.
  expect_identical(.nlds_canon_model(paste0(NLDS_TEST_MODEL, " # x")),
                   .nlds_canon_model(NLDS_TEST_MODEL))
})
