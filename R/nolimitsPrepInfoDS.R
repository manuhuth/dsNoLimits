#' Report the model-derived metadata of a fit cache
#'
#' Aggregate method. Returns everything in the cache except the Julia handle.
#' Every element is model-derived or a plain count; nothing is per-subject.
#'
#' @param prep.name name of the symbol holding a cache built by
#'   [nolimitsPrepDS()] or [nolimitsPrepStringDS()].
#' @return A named list with `theta0`, `names`, `scale`, `p`, `n.subjects`,
#'   `n.obs`, `model.hash` and `versions`. `model.hash` is the SHA-256 of the
#'   canonical model source, which the client compares across sites to prove
#'   every site prepared the byte-identical registered model.
#' @export
nolimitsPrepInfoDS <- function(prep.name) {
  .dsenv <- parent.frame()
  prep <- .ds_get(prep.name, .dsenv, .nlds_is_prep, "NoLimits fit cache")
  list(theta0 = as.numeric(prep$theta0),
       names = as.character(prep$names),
       scale = as.numeric(prep$scale),
       p = as.integer(prep$p),
       n.subjects = as.integer(prep$n.subjects),
       n.obs = as.integer(prep$n.obs),
       model.hash = as.character(prep$model.hash),
       # MCEM M-step partition, so the client can route q1/q2 parameters. Plain
       # character vectors (or NULL when the server lacks the EM primitives);
       # model-derived, so the client compares them across sites for agreement.
       q1.names = if (is.null(prep$q1.names)) NULL else as.character(prep$q1.names),
       q2.names = if (is.null(prep$q2.names)) NULL else as.character(prep$q2.names),
       # SAEM hybrid M-step routing (closed-form vs numerical free names); also
       # model-derived, so the client compares them across sites for agreement.
       cf.names = if (is.null(prep$cf.names)) NULL else as.character(prep$cf.names),
       num.names = if (is.null(prep$num.names)) NULL else as.character(prep$num.names),
       em.available = isTRUE(prep$em.available),
       versions = prep$versions)
}
#AGGREGATE FUNCTION
