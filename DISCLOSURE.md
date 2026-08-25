# dsNoLimits disclosure statement

The headings are the DataSHIELD disclosure-audit checklist. The answers describe
the package as it stands and are kept current with the code.

## 1. What the package is for, and which existing R packages it builds on

dsNoLimits is the server side of a federated fit for nonlinear mixed-effects
(NLME) models of longitudinal data: population pharmacokinetics and any other
repeated-measures model with subject-level random effects. An analyst using the
companion client package `dsNoLimitsClient` runs a single optimisation whose
objective and gradient are the plain unweighted sums of the per-server values, so
the estimates are those of a pooled analysis without any individual-level record
leaving a server.

The statistics are not new. The marginal log-likelihood of the supported
estimators (Laplace, FOCEI, Gauss-Hermite quadrature, naive-pooled, and - for
models without random effects - maximum likelihood and maximum a posteriori) is a
sum of independent per-subject terms, so it and its gradient are additive over
servers. That is the same sufficient-statistic argument that makes `ds.glm`
federated, and the transport here is DataSHIELD's ordinary aggregate/assign round
trip.

Maximum a posteriori is the one estimator whose objective is not purely additive:
its log-prior is a single data-free term. Each server therefore returns the
additive log-likelihood part and, separately, the log-prior block; the client adds
the prior exactly once. Splitting it this way is a correctness requirement (a
plain sum would count the prior once per server) and releases nothing further:
the prior block is a deterministic function of the model's declared priors and
parameter transform, evaluated at the parameter vector the client itself sent.

Dependencies: `dsBase` (disclosure settings and the privacy-control gate);
`jsonlite` (base64url transport codec); and, at run time on the data node,
`NoLimitsR` and `JuliaConnectoR`, which drive the Julia package NoLimits.jl. The
numerical work is NoLimits.jl's; dsNoLimits adds the transport and the disclosure
layer and nothing statistical.

**Deviation from the usual expectation of a pure-R fallback, stated plainly:**
there is none. NoLimits.jl's NLME estimators have no R reimplementation, so Julia
is a deployment prerequisite, not an optimisation. A server without it answers
`nolimitsStatusDS()` with `julia.ok = FALSE` and fails every prep call with
`FAILED: this server is not provisioned for NoLimits (Julia): ...`. It never
falls back to a different, unvalidated estimator.

## 2. What results the package returns to the user

Eight methods. None of them returns anything whose size depends on the number of
observations or the number of subjects.

| method | kind | returns |
|---|---|---|
| `nolimitsStatusDS` | aggregate | `julia.ok`, four version strings, `model.dir.set`. Touches no data. |
| `nolimitsModelsDS` | aggregate | the names and canonical source hashes of the models registered on this server, and whether a registry is configured. Touches no data. |
| `nolimitsPrepDS` | assign | the session-resident fit cache, bound to the analyst's symbol. Never transmitted. |
| `nolimitsPrepStringDS` | assign | the same cache, from an analyst-supplied model (see 5). |
| `nolimitsPrepInfoDS` | aggregate | `theta0`, `names`, `scale`, `p`, `n.subjects`, `n.obs`, `versions`. |
| `nolimitsObjGradDS` | aggregate | `value` (one scalar log-likelihood), `grad` (p numbers), `natural` (p numbers), `finite` (one flag), `n.subjects` (one count); for `estimator = "map"` also `prior.value` (one scalar) and `prior.grad` (p numbers). |
| `nolimitsObjGradDpDS` | aggregate | `grad` (p numbers, per-subject clipped and Gaussian-noised), `releases`, `epsilon`, `remaining.budget`, `delta`, `n.subjects`. No objective value. See 9. |
| `nolimitsNaturalDS` | aggregate | `natural` (p numbers): the model's inverse parameter transform of the client's own vector. Reads no data. |

Notes on the two that carry numbers:

- `theta0`, `names` and `scale` are derived from the **model definition** only,
  never from the data. That is a deliberate invariant: it is what allows the
  client to require exact equality of these across servers as a
  same-model check, and it must not be weakened to a data-dependent
  initialisation.
- `natural` is the model's own deterministic inverse transform applied to the
  parameter vector the client sent. It is a function of the argument alone and
  carries no information about the data. `nolimitsNaturalDS` returns exactly that
  and nothing else; it exists because the client has no Julia and because the
  differentially private path deliberately never asks for a value or a gradient
  at the final parameter vector. Its answer is the same on a server holding a
  million subjects and on one holding none.
- `value` is the **log**-likelihood (to be maximised). No sign is flipped on the
  server; all negation lives in the client's optimiser.
- `prior.value` and `prior.grad` appear only for `estimator = "map"`, and only
  then does `value` carry the likelihood-only part of the objective. Both are
  computed from the model's declared priors and its parameter transform,
  differentiated at the client's own parameter vector. They are the same at every
  server that prepared the same model - the client checks that by exact equality
  each round - which is precisely the statement that they carry no data.
- The per-round release is one scalar and one p-vector of the same class as the
  per-iteration score vector `ds.glm` releases on every IRLS iteration, and the
  client additionally reports each server's log-likelihood contribution in the
  fitted object, which is the same class as the per-study deviance `ds.glmSLMA`
  releases. Both are deliberate and are named here rather than hidden.
- The Julia handle inside the fit cache is a session-resident proxy. It is
  created by an assign method, lives only inside the analyst's symbol, and is
  never placed in any aggregate's return value.

Empirical Bayes estimates, per-subject random effects, residuals, predictions and
any other per-individual quantity are never computed into a return value by any
method in this package.

## 3. How existing disclosure settings are used; any new settings and what they do

Existing `dsBase` settings, read at run time through
`dsBase::listDisclosureSettingsDS()` with the live-name-first,
`default.`-prefixed-fallback cascade. Nothing is hard-coded.

- `nfilter.tab` - the minimum number of subjects a server may contribute. A
  server with fewer refuses to prepare a fit at all.
- `nfilter.glm` - the parameters-to-units saturation ratio. Enforced as
  `p <= nfilter.glm * n.subjects`. Note **subjects**, not observations: for a
  hierarchical model the independent units are the random-effect groups, so
  counting observations would overstate the information available and is the less
  conservative choice.
- `nfilter.stringShort` - the length cap on every name-shaped argument (data
  frame name, model name, id and time column, fit cache name).
- `datashield.privacyControlLevel` - `nolimitsPrepStringDS` runs
  `dsBase::checkPermissivePrivacyControlLevel(c("permissive"))` as its literal
  first statement.

Two new options, both declared in `inst/DATASHIELD`:

- `dsNoLimits.modelDir` - the directory holding this server's registered model
  files. Empty by default, which disables strict mode until the data owner
  registers models.
- `dsNoLimits.maxRounds` - the largest federated round number the server will
  accept, default 500. This is a **courtesy cap, not a query budget**: DataSHIELD
  has no query budget anywhere, the round number is supplied by the client, and a
  determined analyst can reset it by starting a new session. It bounds accidental
  runaway loops; it is not a differential-privacy control and is not represented
  as one.

Seven further options configure the optional differentially private method, and
every one of them is set by the **data owner**; section 9 lists them and states
what the analyst can and cannot influence.

One argument is deliberately exempt from `nfilter.string`: the `theta` argument of
`nolimitsObjGradDS`, which is a `sprintf("%a", .)`-encoded parameter vector and
exceeds 80 characters at four parameters already. It is validated instead by
decoding it and requiring exactly `p` non-`NA` numbers. It is never `eval`ed, and
no name-shaped argument anywhere in the package is `eval(parse())`ed: names are
resolved through a regex-validated `get(..., inherits = FALSE)` helper.

## 4. Precautions for results on small numbers of individuals

- Both gates in section 3 are enforced **server-side, at prep time, before any
  Julia work touches the data**, and a server that fails either one never builds
  a fit cache. Every later round reads its subject count from that cache, so a
  server that could not pass the gates cannot contribute to any round.
- A fired gate `stop()`s. It never returns a sentinel or a partial result
  alongside a warning, and the error condition carries the message and nothing
  else: no data value, no count, no server path. This is unit-tested by
  inspecting the condition object, not only by expecting an error.
- Each gate is tested with a matched negative control exactly at the threshold,
  so a guard that fired unconditionally would not pass the suite.
- Error text is identical across servers for every failure that is not a
  provisioning failure, so the client cannot infer a server's data from which
  message it received. Julia error text is truncated to its reason and stripped
  of its stack trace before it is reported, so server file paths do not travel.
- Round-level results are the same size whatever the data: one scalar, two
  p-vectors, one flag, one count, plus one scalar and one p-vector for `map`.
- A non-finite objective at an out-of-domain probe point is reported as
  `finite = FALSE` with `value = NA` and a zero gradient rather than as an error,
  because that is a legitimate estimator result the client's line search must be
  able to back away from. The flag reveals only that the estimator's expansion
  failed at a point the client itself chose.

**Residual risk, stated honestly.** A federated likelihood surface is not a
summary statistic in the way a mean is: an analyst who is allowed unlimited
evaluations of a log-likelihood and its gradient at parameter vectors of their
choosing is probing a function of the server's data, and with a sufficiently
contrived model definition (permissive mode) and enough rounds, that is a channel.
This is the same residual risk that iterative federated methods such as `ds.glm`
and `ds.glmSLMA` carry, and DataSHIELD's threat model addresses it through the
per-server subject-count floor, the saturation gate, the restriction of model
definitions to a server-side registry in the production profile, and the
professional-trust framework rather than through a formal privacy budget. No
differential-privacy noise is added by `nolimitsObjGradDS`.

**The differencing exposure, named explicitly.** The non-DP path releases the
**exact** additive per-round gradient. Because it is a plain sum over subjects,
an analyst who can obtain two prep caches over datasets differing by exactly one
subject can subtract the two releases and recover that one subject's exact
per-subject gradient at any parameter vector they choose; over enough
chosen-theta queries they can reconstruct that subject's per-subject likelihood
surface. This is the classic DataSHIELD differencing / repeated-query
reconstruction exposure (skill `disclosure-control.md` sections 5.1 and 5.2), and
it is the **same exposure class as any exact federated gradient method** - it is
intrinsic to releasing an exact additive gradient, not specific to this package,
and it cannot be closed by a code guard inside `nolimitsObjGradDS` without
ceasing to release an exact gradient at all. It is stated here so a data
custodian decides with eyes open. The mitigations are, honestly, governance
rather than cryptography:

1. **The trust boundary itself.** The differencing attack presumes an analyst who
   can arrange two datasets differing by one subject on the same node. That is a
   governance question about who is permitted to analyse the data at all.
2. **Not co-permitting arbitrary row-subsetting/reordering verbs.** The attack
   needs a way to form the "one subject fewer" dataset on the node. A data owner
   who does **not** permit general row-subsetting or row-reordering assign methods
   (`ds.dataFrameSubset` and the like) alongside dsNoLimits on the same profile
   denies the analyst the tool that constructs the neighbouring dataset. dsNoLimits
   itself creates no subset and reshapes no data.
3. **DP mode.** `nolimitsObjGradDpDS` (section 9) replaces the exact gradient with
   a clipped, noised one under a formal, budgeted `(epsilon, delta)` guarantee,
   which is the only one of the three that is a mathematical rather than a
   governance mitigation.

## 5. Functions that manipulate the data rather than return summary results

`nolimitsPrepDS` and `nolimitsPrepStringDS` are assign methods. They do not
transform, subset or reshape the analyst's data frame: they read it, count its
subjects, hand a copy to Julia, and bind a cache to the analyst's symbol. No new
data-shaped object is created in the session environment.

`nolimitsPrepStringDS` needs naming bluntly. Its `model.b64` argument decodes to
Julia source that this server then executes. **That is arbitrary code execution
on the data node.** Three things bound it, and a data owner should understand all
three:

1. It is a separately permittable method. Removing `nolimitsPrepStringDS` from
   the profile's `AssignMethods` disables analyst-supplied models entirely and
   leaves strict mode fully functional. This is why the two prep paths are two
   methods rather than one method with a switch.
2. It refuses to run unless the server's `datashield.privacyControlLevel` is
   `permissive`. The shipped DataSHIELD default is not.
3. `nolimitsPrepDS`, the production path, takes only a **name** and reads the
   model source from the server's own registry directory. Nothing an analyst
   sends is executed. The name is validated against a plain-name regex and the
   length cap before it is used, and only files matching a model-name pattern are
   listed or resolved.

Data owners running production deployments should permit `nolimitsPrepDS` and not
`nolimitsPrepStringDS`.

Registration is how a data owner agrees to a model. To make that agreement
auditable across a federation, each prepared model carries a canonical source
hash (SHA-256 of the model source after comments and whitespace are normalised),
returned by `nolimitsPrepInfoDS` and `nolimitsModelsDS`. The client
(`ds.nolimitsClient`) enforces byte-identical model agreement across sites: a
strict-mode fit aborts unless every site's hash matches, so a fit provably ran
the identical registered model each owner approved. The hashes are model-derived
only and disclose no data.

## 6. If repackaging an existing R package, what has been suppressed or manipulated before returning

dsNoLimits does not repackage an R function's output. It calls two NoLimits.jl
entry points (`objective_and_gradient` and the model/parameter accessors) whose
results are, by construction, one scalar and one p-vector; the Julia helper
returns exactly `[value, gradient, natural theta]` as a flat numeric vector, the
log-prior helper returns exactly `[value, gradient]`, and nothing else is
materialised into R. In particular the `FitContext` that the
prep call builds - which does hold per-subject working state - stays inside the
Julia session behind a proxy, and no accessor for it is exposed to any aggregate
method.

## 7. Provision for future maintenance of the package and its dependencies

Maintained by the author of NoLimits.jl and `NoLimitsR` (Manuel Huth, Universität
Bonn) alongside those packages, so the server package and the numerical engine it
depends on are maintained together. `dsNoLimits` and `dsNoLimitsClient` are kept
at identical version numbers and released in lockstep, following the DataSHIELD
recommendation to avoid breaking changes that would force every data owner to
reinstall. The test suite proves the federated result equals the pooled one for
every supported estimator, so an upstream change that breaks the additivity
argument fails CI rather than silently biasing a fit.

Deployment prerequisites (a pinned Julia, `JuliaConnectoR`, `NoLimitsR` and a
precompiled NoLimits.jl depot readable by the R server's user) are part of the
server image and documented with it.

## 8. Which GitHub repository holds the package

Not yet published. The package is developed in a local repository named
`dsNoLimits`, with its client in `dsNoLimitsClient`. This section is completed
with the repository URLs when the packages are published, which will not happen
before the disclosure statement above has been reviewed.

## 9. Differential privacy (`nolimitsObjGradDpDS`)

This section describes an **optional** method. A data owner who does not want
differentially private analysis simply leaves `nolimitsObjGradDpDS` out of the
profile's permitted list; a data owner who wants **only** differentially private
analysis leaves `nolimitsObjGradDS` out instead. Neither choice needs an option.

### The mechanism

Each round the server computes one gradient **per subject**, in the
preconditioned transformed coordinates the client's optimiser steps in, clips
each subject's vector to an L2 ball, sums the clipped vectors and adds Gaussian
noise of standard deviation `sigma * C_total` to the sum. Adding or removing one
subject moves the un-noised sum by at most `C_total`, so the release is the
Gaussian mechanism at noise multiplier `sigma` under add/remove-one-subject
adjacency, and the cost of a fit composes as Renyi differential privacy.

Two deviations from the secure-aggregation setting this design is ported from,
both in the conservative direction, because DataSHIELD has no secure aggregation
and the aggregator IS the analyst:

- every server adds the **full** noise `sigma * C_total`, never
  `sigma * C_total / sqrt(number of servers)`, because each server's release is
  individually visible to the analyst;
- epsilon is accounted **per server**, against that server's own ledger.

Clipping is per parameter group by default (`dp.clipMode = "per-group"`, groups
resolved from the parameter names into location and variance blocks), because
joint clipping scales an outlying subject's variance-component gradient down
along with its location gradient and biases the variance estimates towards zero.
Per-group clipping at `C_g` per group is exactly as private as joint clipping at
`C_total = sqrt(sum_g C_g^2)`, which is why the accountant needs no per-group
case: epsilon depends on `sigma` and the release count alone.

The noise is drawn from the operating system's entropy pool (`/dev/urandom`),
never from R's session RNG: `datashield.seed` makes that stream reproducible, and
reproducible privacy noise is no privacy at all - anyone holding the seed
subtracts it and recovers the exact clipped sum. The draw leaves `.Random.seed`
untouched, so it neither perturbs nor consumes an analyst's seeded work.

Only four estimators are accepted: `laplace`, `focei`, `ghq` and `mle`.
`pooled` and `map` are refused, because neither has a per-subject term with a
bounded sensitivity. Before any release the server checks that the number of
independence units equals the number of subjects and refuses otherwise: with a
random-effect level that groups several subjects together, add/remove-one-subject
is not bounded by the clip.

### What is released, and what is not

`grad`, `releases`, `epsilon`, `remaining.budget`, `delta`, `n.subjects`. There
is **no objective value**: the value would be a second, separately accountable
function of the data, and is not released at all under this method. `epsilon` and
`remaining.budget` are scalars derived from the data owner's own settings and the
release count; they describe the mechanism, not the data.

Every **well-formed** query charges the budget regardless of its numerical
outcome. A parameter vector at which some subject's per-subject gradient is not
finite does not error out for free: that subject's contribution is treated as a
clipped zero-gradient (it would be clipped to the ball anyway, and zero is inside
it), the release proceeds with noise, and the ledger is charged normally, so an
analyst learns nothing free by probing numerically unstable points. Only
structural failures that genuinely cannot yield a release - an unprovisioned
server, a model that groups subjects into batches larger than one, a
wrong-length parameter vector - stop before charging, because they are not
data-probes.

### The persistent budget ledger

DataSHIELD has no query budget anywhere, and any in-session counter resets on
re-login, so the budget lives **outside** the R session: an append-only file per
analysis, in a directory the data owner configures. Until
`dsNoLimits.dp.ledgerDir` is set, the method refuses to run at all.

- The file is keyed by a SHA-256 fingerprint of the analysis data's **contents**
  and the model source, computed on a **canonical** form: columns are ordered by
  name, rows are ordered by a deterministic content key, and the model source is
  stripped of comments with its whitespace collapsed before hashing. So none of
  renaming a symbol, re-uploading the same table, reordering its columns or rows,
  or cosmetically editing the model (adding a comment, reflowing whitespace)
  mints a fresh budget. Genuinely different **data** does legitimately get a
  fresh budget - that is correct, a property of the add/remove-one-subject
  adjacency relation, not a loophole.
- Each release is recorded **before** the noised value is returned. A response
  lost in transit is therefore still counted as spent, which is the conservative
  direction.
- The read-check-append runs under a file lock, so two sessions cannot both pass
  a check against the same remaining budget.
- Once the cumulative epsilon recomputed from the ledger reaches
  `dsNoLimits.dp.epsilonBudget`, every further release is refused. The
  accountant composes a **heterogeneous** history, so budgets stay correct across
  fits the data owner ran at different noise multipliers over time.
- In a container, the ledger directory must be a persistent volume. A rebuild
  that discards it resets the budget; that is a stated deployment failure mode,
  not a silent one.

### What the analyst can never set

`dp.clip`, `dp.clipMode`, `dp.noiseMultiplier`, `dp.epsilonBudget`, `dp.delta`,
`dp.maxT` and `dp.ledgerDir` are **server options**. No client argument reaches
any of them, and no client argument can raise a budget, lower a noise
multiplier, weaken a clip or choose a sensitivity bound. The analyst chooses only
how many rounds to ask for, and asking for more than `dp.maxT` is refused. This
is the opposite of the analyst-supplied-epsilon design found elsewhere in the
DataSHIELD ecosystem, and it is deliberate.

### The honesty boundary

The ledger is tamper-proof against the **DataSHIELD API surface**: no method in
this package reads, writes or reveals the ledger except through the charge above,
and no client argument names a ledger path. It is not tamper-proof against a
compromise of the server host itself - anyone with a shell on the node can delete
the file - and server compromise is outside DataSHIELD's threat model for every
package in the ecosystem, this one included.

Finally, the guarantee is over the DP method's own releases. A profile that
permits `nolimitsObjGradDpDS` **and** `nolimitsObjGradDS` lets an analyst obtain
un-noised gradients through the second method; the epsilon reported by the first
then describes only what the first released. Permitting both is a legitimate
configuration - it is the same trust position as this package without DP at all -
but a data owner who wants the DP guarantee to describe the whole session must
permit the DP method alone.

## 10. The sampling-based EM estimators (MCEM, SAEM)

MCEM and SAEM are sampling-based EM algorithms and are structurally different
from the other estimators, but they release the same class of quantity and are
no more disclosive.

**Per-subject draws never leave the site.** The E-step (`nolimitsEmEStepDS`) is an
assign method: it samples each subject's random-effect draws inside the session
and writes them, with the sampler's warm-state, into the object bound to the
analyst's symbol. It returns nothing to the client. The M-step aggregates read
those draws only to return fixed-size summaries - a Monte-Carlo Q value and its
gradient over the free parameters (`nolimitsEmQDS`), or the model's sufficient
statistics (`nolimitsSaemStatsDS`). Nothing whose size scales with the number of
subjects or observations is ever returned.

**The SAEM stats round releases each site's own sufficient statistics.** Because
`nolimitsSaemStatsDS` is an aggregate, the analyst's session receives one result
per site and sums them itself. Each site's result is that site's block of
de-normalized sufficient statistics - the random-effect mean vector and
covariance, plus the residual and any HMM emission sums - taken over that site's
subjects. This is the same class of release as `dsBase`'s `varDS`/`corDS`, which
return sums and cross-products behind the `nfilter.tab` floor alone, and it is
bounded by the same floor (a site's prep refuses below `nfilter.tab` subjects).
It is more informative per call than a scalar moment, though: one call hands back
a whole group's mean vector and covariance at once. A data owner running SAEM on
small sites should raise `nfilter.tab`, or not permit `nolimitsSaemStatsDS`, if
the group-moment release at the default floor of 3 is not acceptable. The summed
statistics that then travel to the coordinator, and the coordinator's smoothed
state, are population aggregates and carry no additional per-site detail.

**Differencing exposure is the same as the other estimators.** The exact
(non-DP) EM Q and sufficient-statistics oracles are additive over subjects, so the
differencing analysis of section 4 applies unchanged - two datasets differing by
one subject reveal that subject's contribution to the summary. The mitigations are
the same: governance, not co-permitting arbitrary subsetting, and DP mode.

**Differential privacy for EM** (`nolimitsEmQDpDS`, `nolimitsSaemStatsDpDS`)
clips and noises each subject's M-step gradient or sufficient-statistics row and
charges the ledger with the same accounting as section 9. Unlike section 9's
gradient path, the EM DP methods always clip each subject's row **jointly** (one
`dp.clip` bound over the whole row) regardless of the `dsNoLimits.dp.clipMode`
option: per-group clipping has no meaning for a stacked sufficient-statistics
vector whose blocks differ in kind. This does not affect the `(epsilon, delta)`
accounting. It is the most privacy-expensive path in the package: the budget
composes over every M-step step across every outer iteration (`outer * 2 *
dp.rounds` releases for MCEM, `outer * (1 + numerical_parts * dp.rounds)` for
SAEM), so a useful private EM fit needs a correspondingly larger budget. The
coordinator's closed-form update runs on the already-noised summed statistics and
is pure post-processing - it releases nothing and charges nothing.

**A note on ordering.** The M-step and stats rounds read the draws the E-step
cached for the current outer iteration. This "fixed draws per iteration" contract
is maintained by the client, which issues the calls in order within one session;
the shipped `ds.nolimitsFit` is strictly sequential. The server does not bind a
round to an iteration, so a caller that reused the same session symbol from two
concurrent analyses could evaluate an M-step against another iteration's draws.
That is a statistical-correctness concern, not a disclosure one - the draws never
cross the wire either way - and it does not arise with the shipped client.
