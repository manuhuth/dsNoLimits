# Deploying dsNoLimits to a real Opal + Rock federation

This is the data owner's guide to standing up dsNoLimits behind Opal. dsNoLimits
runs its compute kernel in Julia through JuliaConnectoR, so the data node needs
Julia installed. That is a genuine deployment prerequisite, not something the
package can bootstrap. Everything you need is under [`docker/`](docker/): a
Julia-baked Rock profile image, a compose stack, an admin setup script, an
analyst example, and the two example model files.

Live Opal verification of this layer is **PENDING and not yet run** - see
[Status](#status). What follows is the intended, self-contained procedure.

## Prerequisites, honestly

Installing Julia on the node is a real change to the container. Before you
approve it, know what it costs (from the JuliaConnectoR deployment contract):

1. **~1 GB of Julia runtime** plus a precompiled depot baked into the profile
   image.
2. **One localhost TCP socket inside the analysis container.** JuliaConnectoR
   starts a Julia process listening on `localhost:<port>`. Rserve itself is
   configured `remote disable`; this loopback-only socket is a real addition to
   the container's posture and your security review should note it.
3. **One Julia process per live analyst session**, ~150-300 MB RSS each,
   possibly not reaped until the session ends. This is a capacity question for a
   many-analyst node.
4. **No change to the disclosure model.** DataSHIELD restricts which *function
   names* a client may call; it does not inspect what a function does
   internally. A Julia-backed method is declared and gated exactly like a pure-R
   one, and the dsNoLimits disclosure checks still run in R before anything is
   returned. Your audit surface is this package's source, as it already is - see
   [DISCLOSURE.md](DISCLOSURE.md).
5. **Debuggability is worse.** A Julia error crosses a socket and a
   `try()`-wrapped serialisation before it reaches the analyst as an opaque,
   possibly truncated R error. dsNoLimits catches Julia errors and returns
   structured, scrubbed status where it can, but a Julia stack trace is harder
   to chase than an R one.

There is **no pure-R fallback**: NoLimits NLME has no R reimplementation, so
Julia is the method, not an optimisation. A node without a working Julia reports
`julia.ok = FALSE` from `ds.nolimitsStatus()` and fails prep with a clear
provisioning message rather than degrading silently.

## arm64 / amd64

Rock images are **amd64**. Build and run this on an **x86_64 host**. On Apple
silicon the stack runs under emulation where Julia is slow and untested; do not
rely on it for anything but a syntax smoke test.

## Build and bring up

```bash
# 1. Build the Julia-baked Rock image (x86_64 host). Bump versions via build ARGs.
docker build -t dsnolimits/rock-nolimits:0.0.3 docker/

# 2. Bring up mongo + rock + opal.
docker compose -f docker/docker-compose.yml up -d

# 3. Once Opal is healthy, run the owner setup (profile, options, tables).
Rscript docker/setup_opal.R
```

The image build runs three guards ([`docker/Dockerfile`](docker/Dockerfile)):
dsNoLimits installed, its `inst/DATASHIELD` present (the usual cause of
"installed but invisible"), and Julia boots through the bridge. A green build
means the profile will publish methods.

Confirm the methods are visible the way Opal itself checks, from the Rock node:

```bash
curl -u manager:password http://localhost:8085/rserver/packages/_datashield
# the nolimits* methods must appear here
```

## Registering a model: strict vs permissive

dsNoLimits ships two model paths, gated separately in the profile.

- **Strict (production default).** The owner pre-registers model files in a
  directory and the analyst names one. Set the option
  `dsNoLimits.modelDir` to a directory holding `*.jl` files - the image bakes
  [`docker/models/`](docker/models) into `/var/lib/rock/nolimits-models` and
  `setup_opal.R` points the option there. The analyst calls
  `ds.nolimitsFit(model = "warfarin_pk", model.mode = "registered", ...)`.
- **Permissive (opt-in).** The analyst supplies the model as Julia source, which
  is arbitrary code executed on the node. This is the separate assign method
  `nolimitsPrepStringDS`; disable it by removing that one method from the
  profile, or gate it on the `permissive` privacy control level. Do not enable
  it on a node whose analysts you would not trust to run code.

## Provisioning and mounting the DP ledger

Differential-privacy mode enforces an epsilon budget through an append-only
ledger on disk. Two things are mandatory:

1. **Set `dsNoLimits.dp.ledgerDir`** to a directory the Rock user can write.
   Unset, DP mode refuses with a provisioning message. `setup_opal.R` sets it to
   `/var/lib/rock/dp-ledger`.
2. **Mount that directory as a persistent volume.** The compose file mounts the
   `dp_ledger` volume at `/var/lib/rock/dp-ledger`. If the ledger lives on
   ephemeral container storage, a container rebuild resets the spent budget to
   zero - the exact leak the ledger exists to prevent. Do not prune this volume.

The DP budget knobs (`dp.clip`, `dp.clipMode`, `dp.noiseMultiplier`,
`dp.epsilonBudget`, `dp.delta`, `dp.maxT`) are **owner-set only**; the analyst
sets none of them. `setup_opal.R` shows every one.

## Upgrading

Installing a package through Opal auto-publishes only packages that were absent
before, so an **upgrade never re-publishes its method list**. After any bare
upgrade, run `dsadmin.publish_package(o, "dsNoLimits", profile = "nolimits")` or
`dsadmin.profile_init()`. Note `profile_init` is destructive - it wipes options
- so re-set the options after it. The production path is to bake a new image and
redeploy, which sidesteps this entirely.

## Status

**Live Opal verification of this deployment layer has not been run.** It is
gated on an x86_64 host and the user's go-ahead. The artifacts under `docker/`
are written and internally consistent, but no `ds.nolimitsFit` has yet been run
against Opal-backed sites. A future x86_64 run must still confirm: the real
serialization boundary on the return path, the assign-fetch of the Julia proxy
across a real Rock session, and the studysideMessage/error-text reshaping on
real Opal (all clean under DSLite, all unexercised against Opal). Until then,
treat the DSLite equivalence results as the acceptance evidence and this guide
as the plan for the Opal step.
