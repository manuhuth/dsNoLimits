# dsNoLimits 0.0.5.9000

- Sampling-based EM estimators MCEM and SAEM. The E-step (`nolimitsEmEStepDS`)
  is an assign that resamples the per-subject random-effect draws and threads the
  sampler's warm-state into the session cache; the draws never leave the site.
  The M-step is federated: `nolimitsEmQDS` returns the Monte-Carlo Q value and
  gradient at the fixed draws (MCEM, and SAEM's numerical parameters), while
  `nolimitsSaemStatsDS` returns per-subject-additive de-normalized sufficient
  statistics whose coordinate-wise sum drives a stateful closed-form M-step on a
  coordinator site (`nolimitsSaemMstepDS` / `nolimitsSaemUpdatesDS`). Requires the
  NoLimits EM dev-API primitives (NoLimits >= 0.2.7); the functions guard on
  their presence and report clearly otherwise.
- Differential privacy for MCEM and SAEM: `nolimitsEmQDpDS` and
  `nolimitsSaemStatsDpDS` release per-subject-clipped, noised M-step gradients and
  sufficient statistics, charging the same write-ahead budget ledger and RDP
  accountant as the other estimators. The coordinator's closed-form update runs
  on the noised summed statistics as post-processing (no extra release).

# dsNoLimits 0.0.4.9000

- Cross-site model-agreement check: a strict-mode fit verifies every site
  resolved the byte-identical registered model. `nolimitsPrepInfoDS` now returns
  a canonical `model.hash`, and `nolimitsModelsDS` returns a `hashes` vector so
  an auditor can compare registered models across nodes. The hash shares its
  canonicalization with the differential-privacy ledger fingerprint.

# dsNoLimits 0.0.3.9000

Initial development version. Not yet released.

- Server-side federated NLME analysis: per-site marginal log-likelihood and
  gradient oracle for a client-driven optimiser (`nolimitsObjGradDS`).
- Estimators: `laplace`, `focei`, `ghq`, `pooled`, `mle` and `map`.
- Wald standard errors via finite differences of the federated gradient.
- Two model profiles as separately permittable methods: strict server-side
  model registry (`nolimitsPrepDS`) and permissive analyst-supplied model
  string (`nolimitsPrepStringDS`, gated on the `permissive` privacy level).
- Optional differentially private mode (`nolimitsObjGradDpDS`) with per-subject
  gradient clipping, Gaussian noise, and a persistent owner-set budget ledger.
- Disclosure controls enforced server-side: `nfilter.tab` minimum subjects and
  `nfilter.glm * n_subjects` parameter saturation gate.
- `nolimitsStatusDS` reports Julia and package provisioning without failing.

Julia and the registered NoLimits release are a deployment prerequisite; there
is no pure-R fallback.
