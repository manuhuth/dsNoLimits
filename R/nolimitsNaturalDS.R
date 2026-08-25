#' Natural-scale parameters for one transformed-scale parameter vector
#'
#' Aggregate method. Applies the model's own inverse parameter transform to the
#' vector the client supplied and returns the result. It is a deterministic
#' function of its argument and the model, and it reads no data at all: the
#' returned value is the same on a server holding a million subjects and on one
#' holding none. It exists because the client has no Julia, and because the
#' differentially private fit path deliberately never asks for a value or
#' gradient at the final parameter vector.
#'
#' This is the same release the non-private fit path already makes on every
#' round, where it travels inside [nolimitsObjGradDS()]'s `natural` element.
#'
#' @param prep.name name of the symbol holding a cache built by
#'   [nolimitsPrepDS()] or [nolimitsPrepStringDS()].
#' @param theta the transformed-scale parameter vector, `sprintf("%a", .)`-encoded
#'   and comma-joined.
#' @return A named list with `natural`, a numeric vector of length `p`.
#' @export
nolimitsNaturalDS <- function(prep.name, theta) {
  .dsenv <- parent.frame()
  prep <- .ds_get(prep.name, .dsenv, .nlds_is_prep, "NoLimits fit cache")
  p <- as.integer(prep$p)

  if (!is.character(theta) || length(theta) != 1L || is.na(theta)) {
    stop("FAILED: theta must be a single character string", call. = FALSE)
  }
  theta.vec <- suppressWarnings(.ds_num_decode(theta))
  if (length(theta.vec) != p || anyNA(theta.vec)) {
    stop("FAILED: theta must decode to ", p, " numeric values", call. = FALSE)
  }

  .nlds_require_julia()
  .nlds_helpers()
  natural <- tryCatch(
    as.numeric(.nlds_fun("nlds_natural_vec")(prep$dm, theta.vec)),
    error = function(e) {
      stop("FAILED: the parameter transform could not be applied on this ",
           "server: ", .nlds_brief(e), call. = FALSE)
    })
  if (length(natural) != p) {
    stop("FAILED: the parameter transform could not be applied on this ",
         "server: unexpected result length", call. = FALSE)
  }
  list(natural = natural)
}
#AGGREGATE FUNCTION
