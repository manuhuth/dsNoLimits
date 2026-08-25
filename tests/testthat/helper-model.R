# One model fixture reused by every Julia-touching test: Julia code generation is
# the cost, and every distinct @Model pays it again. Closed-form one-compartment
# IV with a log-normal random effect on clearance - no ODE solver runs at all.
NLDS_TEST_MODEL <- '
@fixedEffects begin
    cl    = RealNumber(1.0)
    v     = RealNumber(10.0)
    omega = RealNumber(0.3, scale=:log)
    sigma = RealNumber(0.5, scale=:log)
end

@covariates begin
    t    = Covariate()
    Dose = ConstantCovariate(constant_on=:ID)
end

@randomEffects begin
    eta_cl = RandomEffect(LogNormal(0.0, omega); column=:ID)
end

@formulas begin
    cli = cl * eta_cl
    conc ~ Normal(Dose / v * exp(-(cli / v) * t), sigma)
end
'

# The second fixture, for the mle/map estimators, which require a model with NO
# random effects. Same closed-form one-compartment IV structure, no eta, and a
# weakly informative prior on every parameter so the MAP objective differs from
# the MLE one. Fitting it to the RE-simulated fixture data is a deliberate
# misspecified fit: the oracle is the same fit pooled, so equivalence still holds
# exactly.
NLDS_TEST_MODEL_NORE <- '
@fixedEffects begin
    cl    = RealNumber(1.0,  prior=LogNormal(log(1.0), 0.5))
    v     = RealNumber(10.0, prior=LogNormal(log(10.0), 0.5))
    sigma = RealNumber(0.5, scale=:log, prior=LogNormal(log(0.5), 0.5))
end

@covariates begin
    t    = Covariate()
    Dose = ConstantCovariate(constant_on=:ID)
end

@formulas begin
    conc ~ Normal(Dose / v * exp(-(cl / v) * t), sigma)
end
'

# 6 subjects x 6 observations, synthetic.
nlds_test_data <- function() {
  utils::read.csv(test_path("fixtures", "site_a.csv"), stringsAsFactors = FALSE)
}

# Both spellings of every threshold, scoped by withr so a disc test that lowers
# one cannot poison the next file. Named overrides in `...` replace BOTH
# spellings, which the plain testing.md snippet does not do; the saturation gate
# (p <= nfilter.glm * n.subjects) needs nfilter.glm raised for a 6-subject,
# 4-parameter fixture, and raising only the live name would hide a cascade bug.
ds_test_options <- function(control = "permissive", ...) {
  base <- list(
    nfilter.tab = 3, nfilter.subset = 3, nfilter.glm = 0.33, nfilter.string = 80,
    nfilter.stringShort = 20, nfilter.kNN = 3, nfilter.levels.density = 0.33,
    nfilter.levels.max = 40, nfilter.noise = 0.25, nfilter.privacy.old = 5
  )
  over <- list(...)
  hit <- intersect(names(over), names(base))
  base[hit] <- over[hit]
  over <- over[setdiff(names(over), hit)]
  opts <- c(base, stats::setNames(base, paste0("default.", names(base))))
  opts$datashield.privacyLevel <- 5
  opts$datashield.privacyControlLevel <- control
  opts$default.datashield.privacyControlLevel <- control
  c(opts, over)
}

# The fixture needs nfilter.glm raised: 4 parameters over 6 subjects.
nlds_fit_options <- function(...) ds_test_options(nfilter.glm = 1, ...)

# TRUE only if NoLimits actually boots: Julia present AND NoLimits.jl installed
# and loadable. Cached so booting is attempted once. On CI, where no Julia /
# NoLimits environment is provisioned, this is FALSE and the Julia tiers skip
# instead of erroring.
.nlds_ready <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- isTRUE(tryCatch(
        requireNamespace("NoLimitsR", quietly = TRUE) &&
          requireNamespace("JuliaConnectoR", quietly = TRUE) &&
          isTRUE(JuliaConnectoR::juliaSetupOk()) &&
          !is.null(NoLimitsR::nolimits()),
        error = function(e) FALSE))
    }
    cached
  }
})

nlds_skip_no_julia <- function() {
  skip_on_cran()
  skip_if_not(.nlds_ready(), "Julia / NoLimits not available")
}

# A registry directory holding the fixture model under a registered name.
nlds_model_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  writeLines(NLDS_TEST_MODEL, file.path(dir, "onecomp_iv.jl"))
  dir
}
