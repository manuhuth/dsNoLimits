#' SAEM coordinator closed-form M-step (stateful)
#'
#' Assign method, run on the COORDINATOR site only. Takes the client's summed flat
#' sufficient statistics, reconstructs the aggregated stats from a local template
#' (built from this site's own cached draws), stochastic-approximation-smooths them
#' against the cache's carried `smoothed_state` at this outer iteration's gamma, and
#' closed-form updates the eligible parameters (via NoLimits `saem_closed_form_mstep`,
#' bit-identical to the SAEM fit). The new smoothed_state and the transformed-scale
#' parameter updates are written into the cache; [nolimitsSaemUpdatesDS()] reads the
#' updates back out (the assign-updates-state / aggregate-reads-result pattern, since
#' the closed-form M-step is stateful). On `outer.iter == 1` the SA state initializes
#' from the current stats (`smoothed_state = nothing`).
#'
#' Requires NoLimits >= 0.2.7 (the SAEM dev_api primitives).
#'
#' @param cache.name name of the symbol holding a cache that [nolimitsEmEStepDS()]
#'   has run this outer iteration; the update is bound back to it.
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @param summed.stats the client's coordinate-wise sum of the per-site flat
#'   sufficient statistics, `sprintf("%a", .)`-encoded and comma-joined.
#' @param outer.iter the outer EM iteration, a whole number starting at 1; feeds
#'   the SA gamma schedule.
#' @param maxiters the fixed outer budget, a whole number; parameterizes the SAEM
#'   method (its gamma schedule phase boundaries), matching the pooled fit.
#' @return the fit cache, with the closed-form update and the smoothed_state added,
#'   bound to `cache.name` by the engine.
#' @export
nolimitsSaemMstepDS <- function(cache.name, theta, summed.stats, outer.iter,
                                maxiters) {
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
  if (!is.character(summed.stats) || length(summed.stats) != 1L || is.na(summed.stats)) {
    stop("FAILED: summed.stats must be a single character string", call. = FALSE)
  }
  summed.vec <- suppressWarnings(.ds_num_decode(summed.stats))
  if (length(summed.vec) < 1L || anyNA(summed.vec)) {
    stop("FAILED: summed.stats must decode to numeric values", call. = FALSE)
  }
  if (!.nlds_is_whole(outer.iter) || outer.iter < 1) {
    stop("FAILED: outer.iter must be a whole number >= 1", call. = FALSE)
  }
  if (!.nlds_is_whole(maxiters) || maxiters < 1) {
    stop("FAILED: maxiters must be a whole number >= 1", call. = FALSE)
  }

  .nlds_em_guard()
  # `[[` not `$`: `$em` partial-matches `em.available` / `em.outer`.
  if (is.null(prep[["em"]])) {
    stop("FAILED: no E-step draws are cached; run nolimitsEmEStepDS first",
         call. = FALSE)
  }

  # smoothed_state = nothing on the first outer iteration; otherwise thread the
  # carried update proxy (which holds the previous smoothed_state).
  prev <- if (outer.iter == 1) NULL else prep[["saem.update"]]

  u <- tryCatch(
    .nlds_fun("nlds_saem_mstep")(prep[["dm"]], theta.vec, prep[["em"]], summed.vec,
                                 prev, as.numeric(outer.iter), as.numeric(maxiters)),
    error = function(e) {
      stop("FAILED: the closed-form M-step could not be run on this server: ",
           .nlds_brief(e), call. = FALSE)
    })

  prep$saem.update <- u
  prep
}
#ASSIGN FUNCTION
