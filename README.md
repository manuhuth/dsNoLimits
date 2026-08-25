# dsNoLimits

Server-side DataSHIELD package for federated nonlinear mixed-effects (NLME)
analysis of longitudinal data. It exposes the marginal log-likelihood and its
gradient of a NLME model, per site, so that the companion client package
`dsNoLimitsClient` can drive a federated optimiser and obtain estimates
identical to a pooled analysis without any individual-level data leaving the
node.

This is the package a data custodian installs and permits. Analysts do not use
it directly; they use the client package
[`dsNoLimitsClient`](https://github.com/manuhuth/dsNoLimitsClient).

## What it does

Each `nolimitsObjGradDS` call returns one scalar log-likelihood and one gradient
vector for the current parameter value; the client sums these across sites and
steps an optimiser. Supported estimators are `laplace`, `focei`, `ghq`,
`pooled`, `mle` and `map`, plus the sampling-based EM estimators `mcem` and
`saem`, which run a local E-step each iteration (`nolimitsEmEStepDS`) and a
federated M-step (`nolimitsEmQDS`, and `nolimitsSaemStatsDS` with a coordinator
step for SAEM); the per-subject random-effect draws never leave the node. For
the deterministic estimators, Wald standard errors are obtained by finite
differences of the same federated gradient.

Two model profiles are provided as separately permittable methods:

- strict: the analyst names a model from a server-side registry the data owner
  curates (`nolimitsPrepDS`). This is the production default.
- permissive: the analyst supplies the model as a source string
  (`nolimitsPrepStringDS`), which is arbitrary Julia code executed on the node.
  It is gated on the `permissive` privacy control level and can be withheld by
  removing the single assign method from the profile.

An optional differentially private mode clips per-subject gradients and adds
Gaussian noise under a persistent, owner-set budget ledger that accounts a
cumulative (epsilon, delta) across releases. It covers the gradient estimators
(`nolimitsObjGradDpDS`) and the EM estimators (`nolimitsEmQDpDS`,
`nolimitsSaemStatsDpDS`). All privacy parameters are set by the data owner; the
analyst sets none of them.

## Disclosure posture

The per-round release is one log-likelihood scalar plus one gradient vector of
length equal to the number of parameters, the same class of quantity as the
per-iteration score vector released by `ds.glm`. Nothing whose size depends on
the number of subjects or observations is ever returned. Per-site minima are
enforced server-side: at least `nfilter.tab` subjects, and at most
`nfilter.glm * n_subjects` parameters.

The exact-gradient release means a determined analyst can, in principle, probe
the objective by finite differencing; the differentially private mode exists for
deployments that want that exposure bounded. The permissive profile executes
analyst-supplied code and should only be enabled deliberately. Both points, and
the full eight-question audit, are documented in
[DISCLOSURE.md](DISCLOSURE.md).

## Deployment

Julia is a hard prerequisite. NoLimits NLME has no pure-R reimplementation, so a
node without Julia cannot serve these methods (`nolimitsStatusDS` reports this
rather than failing silently). A node needs:

- Julia, with the registered NoLimits release (>= 0.2.7; the EM estimators use
  primitives added in 0.2.7);
- the `NoLimitsR` R package, which bridges to it;
- this package, installed and its methods permitted through `inst/DATASHIELD`.

The supported deployment route is a container image built on a DataSHIELD Rock
profile that bundles Julia, `NoLimitsR` and this package with the depot
precompiled. Which methods are permitted, and the strict vs permissive and
differentially private profiles, are configured per node.

## Federated equals pooled

For the deterministic estimators, a federated fit reproduces a pooled NoLimits
fit to better than 1e-9 relative on the objective; the warfarin ODE end-to-end
example agrees to 1.7e-9. This holds because the marginal log-likelihood is a
sum of independent per-subject terms, so summing site contributions is exact
rather than approximate. The sampling-based MCEM and SAEM estimators agree with
their pooled counterparts up to Monte-Carlo error.

`dsNoLimits` and `dsNoLimitsClient` are versioned in lockstep; install matching
versions on the server and the client.

## Status

Development version, not yet released. It is currently tested against pooled
reference fits under DSLite (in-process); testing and deployment on a real
Opal/Rock server will follow. See [NEWS.md](NEWS.md).

## License

MIT. See [LICENSE](LICENSE).
