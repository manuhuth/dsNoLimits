test_that("nolimitsModelsDS lists the registry and reports an unset one", {
  withr::local_options(ds_test_options(dsNoLimits.modelDir = NULL,
                                       default.dsNoLimits.modelDir = ""))
  res <- nolimitsModelsDS()
  expect_named(res, c("models", "hashes", "model.dir.set"))
  expect_identical(res$models, character(0L))
  expect_identical(res$hashes, character(0L))
  expect_false(res$model.dir.set)

  dir <- nlds_model_dir()
  withr::local_options(ds_test_options(dsNoLimits.modelDir = dir))
  res <- nolimitsModelsDS()
  expect_identical(res$models, "onecomp_iv")
  expect_identical(res$hashes, .nlds_model_hash(NLDS_TEST_MODEL))
  expect_true(res$model.dir.set)
})

test_that("nolimitsModelsDS hashes are canonical: content changes them, cosmetics do not", {
  dir <- nlds_model_dir()
  withr::local_options(ds_test_options(dsNoLimits.modelDir = dir))
  base <- nolimitsModelsDS()$hashes

  # A comment-only / whitespace-only edit is the SAME registered model.
  writeLines(paste0(NLDS_TEST_MODEL, "\n# an owner comment\n"),
             file.path(dir, "onecomp_iv.jl"))
  expect_identical(nolimitsModelsDS()$hashes, base)

  # A byte-different-but-same-name model is a DIFFERENT model.
  writeLines(sub("Dose / v", "Dose / v + 0.0", NLDS_TEST_MODEL, fixed = TRUE),
             file.path(dir, "onecomp_iv.jl"))
  expect_false(identical(nolimitsModelsDS()$hashes, base))

  # models and hashes stay aligned when several are registered.
  writeLines(NLDS_TEST_MODEL, file.path(dir, "onecomp_iv.jl"))
  writeLines(NLDS_TEST_MODEL_NORE, file.path(dir, "onecomp_nore.jl"))
  res <- nolimitsModelsDS()
  expect_identical(res$models, c("onecomp_iv", "onecomp_nore"))
  expect_identical(res$hashes,
                   c(.nlds_model_hash(NLDS_TEST_MODEL),
                     .nlds_model_hash(NLDS_TEST_MODEL_NORE)))
})

test_that("nolimitsModelsDS ignores files that are not plain model names", {
  dir <- nlds_model_dir()
  writeLines("x", file.path(dir, "notes.txt"))
  writeLines("x", file.path(dir, "_hidden.jl"))
  withr::local_options(ds_test_options(dsNoLimits.modelDir = dir))
  expect_identical(nolimitsModelsDS()$models, "onecomp_iv")
})

test_that("nolimitsModelsDS falls back to the default.-prefixed option", {
  dir <- nlds_model_dir()
  withr::local_options(ds_test_options(dsNoLimits.modelDir = NULL,
                                       default.dsNoLimits.modelDir = dir))
  expect_identical(nolimitsModelsDS()$models, "onecomp_iv")
})
