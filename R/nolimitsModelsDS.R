#' List the models this server has registered for strict mode
#'
#' Touches no data. The registry is the directory named by the
#' `dsNoLimits.modelDir` option (live name first, then `default.`-prefixed).
#' Only files whose names are plain model names are listed.
#'
#' @return A named list with `models` (character vector of model names, without
#'   the `.jl` extension), `hashes` (the SHA-256 of each model's canonical
#'   source, aligned to `models`) and `model.dir.set` (logical). Names and hashes
#'   are model-derived only; no data is touched. An auditor compares the hashes
#'   across nodes to see exactly which model each site has registered.
#' @export
nolimitsModelsDS <- function() {
  dir <- as.character(.nlds_opt("dsNoLimits.modelDir", ""))[1L]
  if (is.na(dir) || !nzchar(dir) || !dir.exists(dir)) {
    return(list(models = character(0L), hashes = character(0L),
                model.dir.set = FALSE))
  }
  files <- list.files(dir, pattern = "\\.jl$")
  files <- files[grepl("^[A-Za-z][A-Za-z0-9._-]*\\.jl$", files)]
  files <- files[order(sub("\\.jl$", "", files))]
  models <- sub("\\.jl$", "", files)
  hashes <- vapply(files, function(f) {
    src <- paste(readLines(file.path(dir, f), warn = FALSE), collapse = "\n")
    .nlds_model_hash(src)
  }, character(1L), USE.NAMES = FALSE)
  list(models = models, hashes = hashes, model.dir.set = TRUE)
}
#AGGREGATE FUNCTION
