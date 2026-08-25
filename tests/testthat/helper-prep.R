# The fit cache is expensive to build (Julia code generation for the model, then
# a warm-up objective evaluation), so every Julia-touching test shares one.
#
# `D` is defined in THIS function's frame, which is exactly the frame
# nolimitsPrepDS captures as parent.frame(). Wrapping a *DS call in a helper is
# only wrong when the data lives in a different frame from the call.
# The caller must have the fit options in scope.
.nlds_prep_cache <- new.env(parent = emptyenv())

nlds_get_prep <- function() {
  nlds_skip_no_julia()
  if (!is.null(.nlds_prep_cache$prep)) return(.nlds_prep_cache$prep)
  dir <- file.path(tempdir(), "nlds-registry")
  dir.create(dir, showWarnings = FALSE)
  writeLines(NLDS_TEST_MODEL, file.path(dir, "onecomp_iv.jl"))
  old <- options(dsNoLimits.modelDir = dir)
  on.exit(options(old), add = TRUE)
  D <- nlds_test_data()
  .nlds_prep_cache$prep <- nolimitsPrepDS("D", "onecomp_iv", "ID", "t")
  .nlds_prep_cache$prep
}

# The no-random-effects, priored fixture, for the mle/map arms. p = 3 over 6
# subjects, so nfilter.glm >= 0.5 is enough; nlds_fit_options() uses 1.
nlds_get_prep_nore <- function() {
  nlds_skip_no_julia()
  if (!is.null(.nlds_prep_cache$nore)) return(.nlds_prep_cache$nore)
  dir <- file.path(tempdir(), "nlds-registry")
  dir.create(dir, showWarnings = FALSE)
  writeLines(NLDS_TEST_MODEL_NORE, file.path(dir, "onecomp_nore.jl"))
  old <- options(dsNoLimits.modelDir = dir)
  on.exit(options(old), add = TRUE)
  D <- nlds_test_data()
  .nlds_prep_cache$nore <- nolimitsPrepDS("D", "onecomp_nore", "ID", "t")
  .nlds_prep_cache$nore
}
