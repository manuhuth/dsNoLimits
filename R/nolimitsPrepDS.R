#' Build the session-resident fit cache from a registered model (strict mode)
#'
#' Assign method. The model is chosen by name from this server's own registry,
#' so no analyst-supplied Julia code is executed.
#'
#' @param df.name name of the data frame in the session environment.
#' @param model.name name of a model registered on this server, as listed by
#'   [nolimitsModelsDS()].
#' @param id.col name of the subject identifier column.
#' @param time.col name of the time column.
#' @return The fit cache, which the DataSHIELD engine binds to the analyst's
#'   symbol. It holds a Julia handle plus model-derived metadata; no per-subject
#'   quantity is ever computed into a return value.
#' @export
nolimitsPrepDS <- function(df.name, model.name, id.col, time.col) {
  .dsenv <- parent.frame()

  .nlds_check_name(model.name, "model.name")
  dir <- as.character(.nlds_opt("dsNoLimits.modelDir", ""))[1L]
  if (is.na(dir) || !nzchar(dir)) {
    stop("FAILED: this server has no model registry configured ",
         "(option dsNoLimits.modelDir)", call. = FALSE)
  }
  path <- file.path(dir, paste0(model.name, ".jl"))
  if (!file.exists(path)) {
    stop("FAILED: '", model.name, "' is not a model registered on this server",
         call. = FALSE)
  }
  model.source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  .nlds_prep(df.name, model.source, id.col, time.col, .dsenv)
}
#ASSIGN FUNCTION
