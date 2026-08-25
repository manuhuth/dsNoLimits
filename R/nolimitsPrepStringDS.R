#' Build the session-resident fit cache from an analyst-supplied model (permissive mode)
#'
#' Assign method. The decoded model string is Julia source that this server
#' executes. It is therefore arbitrary code execution on the node, gated on the
#' `permissive` privacy control level and shipped as a separately permittable
#' method so a data owner can disable permissive mode by removing this one
#' method from the profile. See `DISCLOSURE.md`.
#'
#' @param df.name name of the data frame in the session environment.
#' @param model.b64 base64url-encoded Julia `@Model` source.
#' @param id.col name of the subject identifier column.
#' @param time.col name of the time column.
#' @return The fit cache, which the DataSHIELD engine binds to the analyst's
#'   symbol.
#' @export
nolimitsPrepStringDS <- function(df.name, model.b64, id.col, time.col) {
  dsBase::checkPermissivePrivacyControlLevel(c("permissive"))
  .dsenv <- parent.frame()

  if (!is.character(model.b64) || length(model.b64) != 1L || is.na(model.b64) ||
      !nzchar(model.b64)) {
    stop("FAILED: model.b64 must be a single non-empty character string",
         call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_-]+$", model.b64)) {
    stop("FAILED: model.b64 is not base64url-encoded", call. = FALSE)
  }
  model.source <- tryCatch(.ds_decode(model.b64), error = function(e) {
    stop("FAILED: model.b64 could not be decoded", call. = FALSE)
  })

  .nlds_prep(df.name, model.source, id.col, time.col, .dsenv)
}
#ASSIGN FUNCTION
