# Data-owner setup for the dsNoLimits Opal profile. Run once against a live Opal
# (the docker-compose stack in this directory), as the Opal administrator.
#   Rscript docker/setup_opal.R
#
# It installs/publishes dsNoLimits, creates and enables the profile, sets the
# server-side options (model registry + DP ledger + DP budget), grants use
# permission, uploads the three warfarin study tables, and verifies.

library(opalr)

o <- opal.login("administrator", "password", url = "http://localhost:8080")

PROFILE <- "nolimits"   # MUST equal ROCK_CLUSTER in docker-compose.yml, not ROCK_ID.

# --- install ---------------------------------------------------------------
# In production dsNoLimits is baked into the Rock image, so this is usually a
# no-op. If you install into a live profile instead, note: install auto-publishes
# ONLY for packages absent before, so an UPGRADE needs an explicit publish (below).
# dsadmin.install_github_package(o, "dsNoLimits", username = "manuhuth",
#                                ref = "v0.0.3", profile = PROFILE)

# --- profile ---------------------------------------------------------------
if (!dsadmin.profile_exists(o, PROFILE)) {
  dsadmin.profile_create(o, name = PROFILE, cluster = PROFILE)
}
# profile_init is DESTRUCTIVE: it wipes every method and option and re-derives
# them from the installed packages' inst/DATASHIELD. Always run it BEFORE setting
# options, never after, or the options below are lost.
dsadmin.profile_init(o, name = PROFILE)
# Explicit re-publish so an upgraded method list is picked up (init already
# publishes; this is the line to repeat after any later bare upgrade).
dsadmin.publish_package(o, "dsNoLimits", profile = PROFILE)

# --- options ---------------------------------------------------------------
# The package ships default.dsNoLimits.* defaults; the data owner overrides the
# un-prefixed (live) names. modelDir has no sensible default; dp.ledgerDir MUST
# point at the mounted persistent volume from docker-compose.yml or DP mode
# refuses to run.
dsadmin.set_option(o, "dsNoLimits.modelDir", "/var/lib/rock/nolimits-models", profile = PROFILE)
dsadmin.set_option(o, "dsNoLimits.dp.ledgerDir", "/var/lib/rock/dp-ledger", profile = PROFILE)
# DP budget knobs. The ANALYST sets none of these; the owner does.
dsadmin.set_option(o, "dsNoLimits.dp.clip", "20", profile = PROFILE)
dsadmin.set_option(o, "dsNoLimits.dp.clipMode", "per-group", profile = PROFILE)
dsadmin.set_option(o, "dsNoLimits.dp.noiseMultiplier", "1", profile = PROFILE)
dsadmin.set_option(o, "dsNoLimits.dp.epsilonBudget", "10", profile = PROFILE)
dsadmin.set_option(o, "dsNoLimits.dp.delta", "1e-5", profile = PROFILE)
dsadmin.set_option(o, "dsNoLimits.dp.maxT", "200", profile = PROFILE)

# --- enable + permit -------------------------------------------------------
dsadmin.profile_enable(o, name = PROFILE)
dsadmin.profile_perm_add(o, PROFILE, "analysts", type = "group", permission = "use")

# --- data ------------------------------------------------------------------
# Upload the three warfarin study tables into an Opal project. Alternatively load
# them through the Opal UI (Project > Tables > Import). Each analyst site in a
# real federation holds ONE of these; here all three live in one Opal for a smoke
# test. Columns: ID, t, Dose, conc (see example_analysis.R for the derivation).
w <- nlmixr2data::warfarin
dose <- stats::setNames(w$amt[w$evid == 1], as.character(w$id[w$evid == 1]))
obs <- w[w$dvid == "cp" & w$evid == 0, ]
d <- data.frame(ID = as.character(obs$id), t = obs$time,
                Dose = unname(dose[as.character(obs$id)]), conc = obs$dv)
ids <- unique(d$ID)
if (!opal.project(o, "warfarin")$name %in% "warfarin") opal.project_create(o, "warfarin", database = TRUE)
for (k in 0:2) {
  tab <- d[d$ID %in% ids[seq_along(ids) %% 3 == k], ]
  opal.table_save(o, tab, project = "warfarin", table = paste0("site", k + 1),
                  overwrite = TRUE, force = TRUE, id.name = "ID")
}

# --- verify ----------------------------------------------------------------
print(dsadmin.get_methods(o, type = "aggregate", profile = PROFILE))
print(dsadmin.get_methods(o, type = "assign", profile = PROFILE))
print(dsadmin.get_options(o, profile = PROFILE))

opal.logout(o)
