#' Run one MCEM E-step and cache the draws in the session
#'
#' Assign method. At the broadcast parameter vector each site draws its own
#' subjects posterior random effects (via NoLimits `mcem_e_step`), threading the
#' warm-start state from the previous outer iteration, and stores the FIXED draws
#' plus the new state inside the fit cache. The draws never cross the wire and
#' nothing whose size depends on the number of subjects is returned: this method
#' only mutates session state, so it is an assign, and the engine binds the
#' returned cache back to the analyst symbol.
#'
#' On `outer.iter == 1` the E-step seeds from the prior mean (state = nothing),
#' which also resets the sampler when a second fit reuses the same cache symbol.
#' The RNG is seeded by `base.seed + site.id`, so with each site given a stable
#' id the draws are a pure function of the seed, the site and the subject, and the
#' federated fit reproduces the pooled one up to Monte-Carlo noise.
#'
#' Requires NoLimits >= 0.2.7 (the MCEM dev_api primitives); an older server
#' fails with a legible message.
#'
#' @param cache.name name of the symbol holding a cache built by
#'   [nolimitsPrepDS()] or [nolimitsPrepStringDS()]; the E-step result is bound
#'   back to it.
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @param outer.iter the outer EM iteration, a whole number starting at 1.
#' @param base.seed the base E-step RNG seed, a whole number.
#' @param site.id this site stable integer id, a whole number; added to
#'   `base.seed` to seed the RNG.
#' @param sample.schedule the number of posterior draws per subject per E-step, a
#'   whole number (the demo default is 100).
#' @return the fit cache, with the E-step draws and warm-start state added, bound
#'   to `cache.name` by the engine.
#' @export
nolimitsEmEStepDS <- function(cache.name, theta, outer.iter, base.seed, site.id,
                              sample.schedule = 100) {
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
  if (!.nlds_is_whole(outer.iter) || outer.iter < 1) {
    stop("FAILED: outer.iter must be a whole number >= 1", call. = FALSE)
  }
  if (!.nlds_is_whole(base.seed)) {
    stop("FAILED: base.seed must be a whole number", call. = FALSE)
  }
  if (!.nlds_is_whole(site.id) || site.id < 0) {
    stop("FAILED: site.id must be a whole number >= 0", call. = FALSE)
  }
  if (!.nlds_is_whole(sample.schedule) || sample.schedule < 1) {
    stop("FAILED: sample.schedule must be a whole number >= 1", call. = FALSE)
  }

  .nlds_em_guard()
  # `[[` not `$`: `$em` partial-matches `em.available` / `em.outer`.
  if (!isTRUE(prep[["em.available"]])) {
    stop("FAILED: this fit cache was prepared without the MCEM partition; ",
         "re-prepare the model on a server with NoLimits >= 0.2.7", call. = FALSE)
  }

  seed <- as.numeric(base.seed) + as.numeric(site.id)
  # Fresh seeding on the first outer iteration; thread the previous state after.
  prev <- if (outer.iter == 1) NULL else prep[["em"]]

  em <- tryCatch(
    .nlds_fun("nlds_mcem_estep")(prep[["dm"]], theta.vec, as.numeric(sample.schedule),
                                 seed, prev),
    error = function(e) {
      stop("FAILED: the E-step could not be run on this server: ",
           .nlds_brief(e), call. = FALSE)
    })

  prep$em <- em
  prep$em.outer <- as.integer(outer.iter)
  prep
}
#ASSIGN FUNCTION
