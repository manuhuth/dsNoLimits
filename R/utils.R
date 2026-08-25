# Internal helpers shared by the server functions. None of these are listed in
# inst/DATASHIELD and none of them may be called from the client.

# Package-local cache. Julia must never boot at library() time.
.nlds_state <- new.env(parent = emptyenv())

#' Resolve a client-supplied object name from the DataSHIELD session environment.
#'
#' @param name character(1); a plain symbol name or a `$`-path such as `"D$age"`.
#' @param env the session environment, captured once as `parent.frame()` by the
#'   calling `*DS` function and passed in explicitly.
#' @param predicate optional function such as `is.numeric`; the resolved object
#'   must satisfy it.
#' @param what human-readable description used in error messages.
#' @return the resolved object.
#' @keywords internal
#' @noRd
.ds_get <- function(name, env, predicate = NULL, what = "object") {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop(sprintf("FAILED: %s name must be a single character string", what),
         call. = FALSE)
  }
  max_chars <- suppressWarnings(
    as.numeric(dsBase::listDisclosureSettingsDS()$nfilter.stringShort))
  # as.numeric(NULL) is numeric(0), not NA - both cases must fall back
  if (!length(max_chars) || is.na(max_chars)) max_chars <- 20
  if (nchar(name) > max_chars) {
    stop(sprintf("FAILED: %s name exceeds nfilter.stringShort (%d characters)",
                 what, as.integer(max_chars)), call. = FALSE)
  }
  # A legal symbol, or symbols joined by `$`. Rejects (), [], operators, spaces,
  # backticks, `::`, quotes and anything else the parser would honour.
  sym <- "[A-Za-z.][A-Za-z0-9._]*"
  if (!grepl(sprintf("^%s(\\$%s)*$", sym, sym), name)) {
    stop(sprintf("FAILED: %s name is not a plain symbol or $-path", what),
         call. = FALSE)
  }
  parts <- strsplit(name, "$", fixed = TRUE)[[1]]
  if (!exists(parts[1L], envir = env, inherits = FALSE)) {
    stop(sprintf("FAILED: %s '%s' does not exist on this server", what, parts[1L]),
         call. = FALSE)
  }
  out <- get(parts[1L], envir = env, inherits = FALSE)
  for (p in parts[-1L]) {
    if (!(is.list(out) || is.data.frame(out)) || !(p %in% names(out))) {
      stop(sprintf("FAILED: %s '%s' has no element '%s'", what, name, p),
           call. = FALSE)
    }
    out <- out[[p]]
  }
  if (!is.null(predicate) && !isTRUE(predicate(out))) {
    stop(sprintf("FAILED: %s '%s' is of the wrong type", what, name),
         call. = FALSE)
  }
  out
}

#' Read one validated disclosure threshold, live name first.
#' @keywords internal
#' @noRd
.nf <- function(name) {
  v <- suppressWarnings(as.numeric(dsBase::listDisclosureSettingsDS()[[name]]))
  if (length(v) != 1L || !is.finite(v)) {
    stop("FAILED: disclosure setting ", name, " is not configured on this server",
         call. = FALSE)
  }
  v
}

#' Two-level option lookup: the data owner's live name, then the package default.
#' @keywords internal
#' @noRd
.nlds_opt <- function(name, default = NULL) {
  v <- getOption(name)
  if (is.null(v)) v <- getOption(paste0("default.", name))
  if (is.null(v)) default else v
}

# Exact double marshalling. Never round for transmission.
#' @keywords internal
#' @noRd
.ds_num_encode <- function(x) paste0(sprintf("%a", x), collapse = ",")

#' @keywords internal
#' @noRd
.ds_num_decode <- function(s) as.numeric(unlist(strsplit(s, ",", fixed = TRUE)))

# base64url: the only alphabet legal in a string literal in both wire grammars.
#' @keywords internal
#' @noRd
.ds_encode <- function(x) {
  if (!nzchar(x)) return(NULL)
  # jsonlite::base64_enc line-wraps beyond 64 bytes; a newline on the wire is a
  # lexer error, so strip all whitespace before stripping the padding.
  b64 <- gsub("[[:space:]]", "", jsonlite::base64_enc(charToRaw(enc2utf8(x))))
  gsub("=", "", chartr("+/", "-_", b64), fixed = TRUE)
}

#' @keywords internal
#' @noRd
.ds_decode <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]", "", x)
  pad <- strrep("=", (4L - nchar(x) %% 4L) %% 4L)
  rawToChar(jsonlite::base64_dec(chartr("-_", "+/", paste0(x, pad))))
}

#' A whole number, whatever storage mode it arrived in.
#'
#' Integer literals are not safely representable in the Opal grammar, so the
#' client sends counts as whole-numbered doubles.
#' @keywords internal
#' @noRd
.nlds_is_whole <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == trunc(x)
}

#' Validate a plain-name argument (a column name, a registered model name).
#' @keywords internal
#' @noRd
.nlds_check_name <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("FAILED: %s must be a single character string", what),
         call. = FALSE)
  }
  max_chars <- suppressWarnings(
    as.numeric(dsBase::listDisclosureSettingsDS()$nfilter.stringShort))
  if (!length(max_chars) || is.na(max_chars)) max_chars <- 20
  if (nchar(x) > max_chars) {
    stop(sprintf("FAILED: %s exceeds nfilter.stringShort (%d characters)",
                 what, as.integer(max_chars)), call. = FALSE)
  }
  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", x)) {
    stop(sprintf("FAILED: %s is not a plain name", what), call. = FALSE)
  }
  invisible(x)
}

#' Canonical form of the model source, for hashing.
#'
#' Strips R/Julia line comments (`#` to end of line) and collapses every run of
#' whitespace to one space, so a cosmetic edit - an added comment, a reflow -
#' does not change the fingerprint. Shared by the DP ledger fingerprint
#' (`.dp_fingerprint`) and the cross-site model-agreement hash so both use one
#' canonicalization and cannot drift apart.
#' ponytail: a `#` inside a string literal is stripped too; renaming symbols and
#' reflowing already collapse to the same key, and a data owner editing string
#' contents to mint budget is not the threat. Tighten to a real tokenizer only if
#' that ever matters.
#' @keywords internal
#' @noRd
.nlds_canon_model <- function(model.source) {
  s <- paste(as.character(model.source), collapse = "\n")
  s <- gsub("#[^\n]*", "", s)
  trimws(gsub("[[:space:]]+", " ", s))
}

#' SHA-256 of the canonical model source: the auditable model fingerprint the DP
#' ledger and the client's cross-site model-agreement check both compare.
#' @keywords internal
#' @noRd
.nlds_model_hash <- function(model.source) {
  digest::digest(.nlds_canon_model(model.source), algo = "sha256")
}

# The Julia side of the protocol, evaluated once per R process into the
# NoLimitsSession module. Port of the NoLimitsFlower app's helpers without the
# Python GIL and differential-privacy parts.
JULIA_HELPERS <- '
const NLDS_CTX  = IdDict{Any, Any}()
const NLDS_AXES = IdDict{Any, Any}()

# One FitContext per site DataModel: it carries the batch infos, the constants
# cache and the evaluation cache, so a round does not rebuild them.
nlds_ctx(dm) = get!(() -> NoLimits.build_fit_context(dm), NLDS_CTX, dm)

function nlds_axes(dm)
    get!(NLDS_AXES, dm) do
        (NoLimits.ComponentArrays.getaxes(NoLimits.get_params(dm, scale = :transformed)),
         dm.model.fixed.inverse_transform)
    end
end

function nlds_natural(dm, v)
    ax, inv = nlds_axes(dm)
    inv(NoLimits.ComponentArrays.ComponentArray(collect(Float64, v), ax))
end

nlds_natural_vec(dm, v) = Vector{Float64}(nlds_natural(dm, v))

# The model own declared inits on the transformed scale: the optimizer start
# point, and never data-derived.
nlds_theta0(dm) = Vector{Float64}(NoLimits.get_params(dm, scale = :transformed))

# collect, not a tuple: a Tuple would cross the bridge as a proxy object.
nlds_names(dm) = collect(String, string.(keys(NoLimits.get_params(dm, scale = :untransformed))))

# Diagonal preconditioning scale, taken from NoLimits itself. It is internal, so
# fall back to no preconditioning rather than break if it is ever renamed.
function nlds_precondition(dm)
    theta = NoLimits.get_params(dm, scale = :transformed)
    isdefined(NoLimits, :_precondition_scale) || return ones(Float64, length(theta))
    Vector{Float64}(NoLimits._precondition_scale(
        NoLimits.get_model(dm), propertynames(theta), theta))
end

function nlds_method(estimator::AbstractString, level::Integer)
    estimator == "laplace" && return NoLimits.Laplace()
    estimator == "focei"   && return NoLimits.FOCEI()
    estimator == "ghq"     && return NoLimits.GHQuadrature(level = Int(level))
    estimator == "pooled"  && return NoLimits.Pooled()
    estimator == "mle"     && return NoLimits.MLE()
    # "map" too: the site returns only the DATA-dependent part of the MAP
    # objective, which is exactly MLE(). The log-prior is data-free and equal at
    # every site, so summing MAP() over S sites would count it S times; the
    # client adds it once, from nlds_prior.
    estimator == "map"     && return NoLimits.MLE()
    error("unknown estimator")
end

# ONE flat Float64 vector out: [value; grad(p); natural(p)]. A tuple would come
# back as proxies costing a juliaGet each.
function nlds_objgrad(dm, v, method)
    theta = nlds_natural(dm, v)
    val, grad = NoLimits.objective_and_gradient(method, nlds_ctx(dm), theta,
                                                scale = "transformed")
    vcat(Float64(val), Vector{Float64}(grad), nlds_natural_vec(dm, v))
end

# The MAP log-prior block, [value; grad(p)], in TRANSFORMED coordinates. Purely
# model-derived: it reads the declared priors and the parameter transform, never
# the data. One ForwardDiff sweep straight through inverse_transform, so no
# manual Jacobian is needed.
function nlds_prior(dm, v)
    ax, inv = nlds_axes(dm)
    fe = NoLimits.get_fixed(NoLimits.get_model(dm))
    z = collect(Float64, v)
    f = w -> NoLimits.logprior(fe, inv(NoLimits.ComponentArrays.ComponentArray(w, ax)))
    res = NoLimits.ForwardDiff.DiffResults.GradientResult(z)
    NoLimits.ForwardDiff.gradient!(res, f, z)
    vcat(Float64(NoLimits.ForwardDiff.DiffResults.value(res)),
         Vector{Float64}(NoLimits.ForwardDiff.DiffResults.gradient(res)))
end

# --- differential privacy: the per-subject material the clipping needs -------
#
# A random-effect BATCH is the independence unit of NoLimits. With one random
# effect grouped on the subject id a batch IS one subject; the caller asserts
# that (batches == subjects) before anything is released, because a per-subject
# sensitivity bound is only meaningful when the clipping unit is the subject.
#
# The gradients are returned in the coordinates the CLIENT optimises in:
# transformed axes times the preconditioning scale s. Clipping has to bound the
# norm of the vector the optimiser steps with, since that is the vector the
# noise is calibrated against.
nlds_bstars(method::Union{NoLimits.Laplace, NoLimits.FOCEI}, ctx, theta) =
    NoLimits.empirical_bayes(ctx, theta)
nlds_bstars(method, ctx, theta) = nothing

nlds_batch_og(method::Union{NoLimits.Laplace, NoLimits.FOCEI}, ctx, theta, bi, bstars) =
    NoLimits.objective_and_gradient(
        method, ctx.dm, theta, NoLimits.get_batch_infos(ctx)[bi], bstars[bi];
        const_cache = ctx.const_cache, cache = ctx.cache, scale = "transformed")

nlds_batch_og(method::NoLimits.GHQuadrature, ctx, theta, bi, bstars) =
    NoLimits.objective_and_gradient(
        method, ctx.dm, theta, NoLimits.get_batch_infos(ctx)[bi];
        const_cache = ctx.const_cache, cache = ctx.cache, scale = "transformed")

# ONE flat Float64 vector out: [nrows; maxids; vec(grads)], column-major, so the
# R side rebuilds it with matrix(x, nrow = nrows).
function nlds_dp_batches(dm, v, method)
    theta = nlds_natural(dm, v)
    s = nlds_precondition(dm)
    ctx = nlds_ctx(dm)
    infos = NoLimits.get_batch_infos(ctx)
    bstars = nlds_bstars(method, ctx, theta)
    grads = Matrix{Float64}(undef, length(infos), length(s))
    maxids = 0
    for bi in eachindex(infos)
        _, g = nlds_batch_og(method, ctx, theta, bi, bstars)
        grads[bi, :] .= s .* Vector{Float64}(g)
        maxids = max(maxids, length(infos[bi].inds))
    end
    vcat(Float64(length(infos)), Float64(maxids), vec(grads))
end

# The no-random-effects case: the clipping unit is the individual and the
# per-subject term is the conditional log-likelihood. Same layout as above.
function nlds_dp_individuals(dm, v)
    theta = nlds_natural(dm, v)
    s = nlds_precondition(dm)
    ctx = nlds_ctx(dm)
    n = length(NoLimits.get_individuals(dm))
    grads = Matrix{Float64}(undef, n, length(s))
    for i in 1:n
        _, g = NoLimits.objective_and_gradient(
            NoLimits.MLE(), ctx, theta, i; scale = "transformed")
        grads[i, :] .= s .* Vector{Float64}(g)
    end
    vcat(Float64(n), 1.0, vec(grads))
end

# --- MCEM: nested federated EM (local, stateful E-step; federated M-step) -----
#
# Port of the NoLimitsFlower feat/federated-em nlf_mcem_* helpers. The M-step
# Q(theta) at FIXED posterior draws is a per-subject sum, so its value AND
# gradient federate exactly, split into a q1 (observation-side) and q2
# (random-effect-distribution) part by mcem_q_partition. The E-step samples each
# site is subjects local posteriors and is STATEFUL: the draws and the warm-start
# state persist in the session cache across the M-step rounds of one outer
# iteration and NEVER cross the wire.
import Random

# Guard on the dev_api primitives themselves, not on a version number: NoLimits
# main carries them while still reporting 0.2.6, so a version check would fail.
nlds_em_available() =
    isdefined(NoLimits, :mcem_e_step) &&
    isdefined(NoLimits, :mcem_q_objective_and_gradient) &&
    isdefined(NoLimits, :mcem_q_partition) &&
    isdefined(NoLimits, :saem_sufficient_statistics) &&
    isdefined(NoLimits, :saem_closed_form_mstep) &&
    isdefined(NoLimits, :saem_closed_form_eligibility)

# A length-1 character vector crosses the bridge as a Julia scalar String;
# collect() would then split it into characters. Normalize to a Symbol vector.
_nlds_symvec(x::AbstractString) = [Symbol(x)]
_nlds_symvec(x) = Symbol.(collect(x))

# q1/q2 free-name partition as String vectors (collect, so a Tuple does not cross
# as a proxy), in the model parameter order.
nlds_mcem_q1(dm) = collect(String, string.(NoLimits.mcem_q_partition(dm).q1))
nlds_mcem_q2(dm) = collect(String, string.(NoLimits.mcem_q_partition(dm).q2))

# One proxy object holding the E-step draws and warm-start state, so neither is
# translated across the bridge: draws stay Julia-side, and the NamedTuple state
# is not deep-copied into R (which would break threading).
mutable struct NLDSEState
    draws
    state
end

# One LOCAL E-step at the wire theta. `prev === nothing` on outer iter 1 (fresh
# prior-mean seeding); otherwise thread `prev.state` so warm-start and per-batch
# RNGs persist. The rng is seeded reproducibly by the caller (base seed + site id),
# matching the demo; maxiters does not affect a constant sample schedule.
function nlds_mcem_estep(dm, v, sample_schedule, seed, prev)
    theta = nlds_natural(dm, v)
    method = NoLimits.MCEM(sample_schedule = Int(sample_schedule), maxiters = 100)
    rng = Random.Xoshiro(UInt64(seed))
    state = prev === nothing ? nothing : prev.state
    draws, new_state = NoLimits.mcem_e_step(dm, theta, method, state; rng = rng)
    NLDSEState(draws, new_state)
end

nlds_mcem_nbatches(em) = length(em.draws)

# M-step Q value + transformed-axes gradient over one part free names at the FIXED
# cached draws. ONE flat vector out: [Q; grad(k)], k = length(free_names). Summing
# over sites reproduces the pooled Q (per-subject additive). Maximization sense.
function nlds_mcem_q(dm, v, em, part, free_names)
    theta = nlds_natural(dm, v)
    Q, g = NoLimits.mcem_q_objective_and_gradient(
        dm, theta, em.draws; part = Symbol(part),
        free_names = _nlds_symvec(free_names), scale = "transformed")
    vcat(Float64(Q), Vector{Float64}(g))
end

# Per-subject (batch idx) form for the additivity proof: summing over idx equals
# the population nlds_mcem_q to machine precision.
function nlds_mcem_q_idx(dm, v, em, idx, part, free_names)
    theta = nlds_natural(dm, v)
    Q, g = NoLimits.mcem_q_objective_and_gradient(
        dm, theta, em.draws, Int(idx); part = Symbol(part),
        free_names = _nlds_symvec(free_names), scale = "transformed")
    vcat(Float64(Q), Vector{Float64}(g))
end

# DP M-step: ONE per-subject-gradient matrix over the part free_names at the FIXED
# draws, for the R side to clip + noise. No Q value (that would be a second,
# separately accountable release). ONE flat vector out: [n; k; vec(grads)],
# column-major, so R rebuilds it with matrix(x, nrow = n). Gradient on the
# transformed axes; the R side applies the preconditioning s before clipping.
function nlds_mcem_dp_rows(dm, v, em, part, free_names)
    theta = nlds_natural(dm, v)
    fnames = _nlds_symvec(free_names)
    n = length(em.draws)
    k = length(fnames)
    grads = Matrix{Float64}(undef, n, k)
    for i in 1:n
        _, g = NoLimits.mcem_q_objective_and_gradient(
            dm, theta, em.draws, i; part = Symbol(part),
            free_names = fnames, scale = "transformed")
        grads[i, :] .= Vector{Float64}(g)
    end
    vcat(Float64(n), Float64(k), vec(grads))
end

# --- SAEM: hybrid federated M-step (closed-form + numerical) -----------------
#
# Port of the NoLimitsFlower feat/federated-em nlf_saem_* helpers. Per outer
# iteration each site emits per-subject-additive DE-NORMALIZED SAEM sufficient
# statistics; the client sums them coordinate-wise; a COORDINATOR site runs the
# STATEFUL closed-form M-step (saem_closed_form_mstep, bit-identical to the fit)
# and the numerical (non-closed-form) params reuse the MCEM Q kernel. Reuses the
# MCEM E-step + draw cache above.

# closed-form-eligible vs numerical free names, String vectors in parameter order.
nlds_saem_cf(dm)  = collect(String, string.(NoLimits.saem_closed_form_eligibility(dm).closed_form))
nlds_saem_num(dm) = collect(String, string.(NoLimits.saem_closed_form_eligibility(dm).numerical))

_nlds_flat_push!(out, x::Number) = push!(out, Float64(x))
_nlds_flat_push!(out, x::AbstractArray) = append!(out, Float64.(vec(x)))

# Flatten (re, outcome, hmm) stats to a Float64 vector of DE-NORMALIZED additive
# quantities in the deterministic key order. re: [Sx (d), vec(Sxx) (d*d), n];
# outcome: [s1, s2, ss, n]; hmm: [sum_w..., sum_wy...]. Model-fixed layout, so the
# client sums coordinate-wise; the coordinator re-normalizes on the way back in.
function _nlds_saem_flatten(stats)
    out = Float64[]
    for re in keys(stats.re)
        s = getfield(stats.re, re)
        _nlds_flat_push!(out, s.mean .* s.n)
        _nlds_flat_push!(out, s.second .* s.n)
        _nlds_flat_push!(out, Float64(s.n))
    end
    for col in keys(stats.outcome)
        s = getfield(stats.outcome, col)
        _nlds_flat_push!(out, s.s1); _nlds_flat_push!(out, s.s2)
        _nlds_flat_push!(out, s.ss); _nlds_flat_push!(out, Float64(s.n))
    end
    for col in keys(stats.hmm)
        s = getfield(stats.hmm, col)
        _nlds_flat_push!(out, s.sum_w); _nlds_flat_push!(out, s.sum_wy)
    end
    out
end

# Rebuild aggregated stats from the summed flat vector, re-normalizing the RE
# moments (mean = Sx/n, second = Sxx/n). `template` gives the model-fixed structure.
function _nlds_saem_unflatten(template, flat)
    i = 0
    re_pairs = Pair{Symbol, Any}[]
    for re in keys(template.re)
        s = getfield(template.re, re)
        d = length(s.mean)
        sx = flat[(i + 1):(i + d)]; i += d
        sxx = reshape(flat[(i + 1):(i + d * d)], d, d); i += d * d
        n = flat[i + 1]; i += 1
        push!(re_pairs, re => (family = s.family, mean = sx ./ n, second = sxx ./ n, n = n))
    end
    out_pairs = Pair{Symbol, Any}[]
    for col in keys(template.outcome)
        s = getfield(template.outcome, col)
        st = (family = s.family, s1 = flat[i + 1], s2 = flat[i + 2], ss = flat[i + 3], n = flat[i + 4])
        i += 4
        push!(out_pairs, col => st)
    end
    hmm_pairs = Pair{Symbol, Any}[]
    for col in keys(template.hmm)
        s = getfield(template.hmm, col)
        lw = length(s.sum_w); lwy = length(s.sum_wy)
        sw = lw == 1 ? flat[i + 1] : flat[(i + 1):(i + lw)]; i += lw
        swy = lwy == 1 ? flat[i + 1] : flat[(i + 1):(i + lwy)]; i += lwy
        push!(hmm_pairs, col => (family = s.family, target = s.target, sum_w = sw, sum_wy = swy))
    end
    (re = NamedTuple(re_pairs), outcome = NamedTuple(out_pairs), hmm = NamedTuple(hmm_pairs))
end

# This site DE-NORMALIZED additive sufficient statistics over the FIXED cached
# draws (the stats round). ONE flat Float64 vector out.
nlds_saem_stats_flat(dm, v, em) =
    _nlds_saem_flatten(NoLimits.saem_sufficient_statistics(dm, nlds_natural(dm, v), em.draws))

# Per-subject (batch idx) form for the additivity proof: summing over idx equals
# the population nlds_saem_stats_flat to machine precision.
nlds_saem_stats_flat_idx(dm, v, em, idx) =
    _nlds_saem_flatten(NoLimits.saem_sufficient_statistics(dm, nlds_natural(dm, v), em.draws, Int(idx)))

# DP stats round: ONE per-subject DE-NORMALIZED flat-stats matrix over the FIXED
# draws, for the R side to clip + noise. ONE flat vector out: [n; L; vec(rows)],
# column-major, so R rebuilds it with matrix(x, nrow = n).
function nlds_saem_stats_dp_rows(dm, v, em)
    theta = nlds_natural(dm, v)
    n = length(em.draws)
    rows = [_nlds_saem_flatten(NoLimits.saem_sufficient_statistics(dm, theta, em.draws, i)) for i in 1:n]
    L = length(rows[1])
    m = Matrix{Float64}(undef, n, L)
    for i in 1:n
        m[i, :] .= rows[i]
    end
    vcat(Float64(n), Float64(L), vec(m))
end

# The demo SAEM method: no SA burn-in, a plain-maximization numerical M-step, and a
# convergence window past the outer budget so the fixed budget always runs. Shared
# by the eligibility split and the coordinator gamma schedule so both agree.
nlds_saem_method(maxiters) = NoLimits.SAEM(
    maxiters = Int(maxiters), sa_burnin_iters = 0, convergence_window = 50,
    mstep_sa_on_params = false)

# One proxy holding the coordinator closed-form update (names + TRANSFORMED-scale
# values) AND the SA smoothed_state, so the NamedTuple state is not deep-copied into
# R and can be threaded to the next outer iteration. Mirrors NLDSEState.
mutable struct NLDSSaemUpdate
    names
    values
    state
end

# One COORDINATOR-side closed-form M-step from the server summed flat stats.
# Reconstructs the aggregated stats (template from the coordinator own draws),
# computes gamma at outer iteration k, runs the STATEFUL update threading `prev.state`
# (`prev === nothing` on k == 1), and returns the eligible params on the wire scale
# plus the smoothed_state to carry forward, all inside one proxy.
function nlds_saem_mstep(dm, v, em, summed_flat, prev, k, maxiters)
    theta = nlds_natural(dm, v)
    template = NoLimits.saem_sufficient_statistics(dm, theta, em.draws)
    agg = _nlds_saem_unflatten(template, collect(Float64, summed_flat))
    method = nlds_saem_method(maxiters)
    gamma = NoLimits._saem_gamma_schedule(Int(k), method.saem)
    state = prev === nothing ? nothing : prev.state
    updates, new_state = NoLimits.saem_closed_form_mstep(
        dm, agg, state, theta, Float64(gamma); method = method)
    theta_nat = deepcopy(theta)
    for name in keys(updates)
        setproperty!(theta_nat, name, getproperty(updates, name))
    end
    theta_t = dm.model.fixed.transform(theta_nat)
    names = String[String(n) for n in keys(updates)]
    vals = Float64[Float64(getproperty(theta_t, n)) for n in keys(updates)]
    NLDSSaemUpdate(names, vals, new_state)
end

# Read the closed-form update out of the proxy (the coordinator AGGREGATE reply).
nlds_saem_names(u)  = collect(String, u.names)
nlds_saem_values(u) = Vector{Float64}(u.values)
'

#' Preflight: Julia and NoLimits must be provisioned on this server.
#'
#' There is no pure-R fallback: NoLimits NLME has no R reimplementation, so
#' Julia is a documented deployment prerequisite (see DISCLOSURE.md).
#' @keywords internal
#' @noRd
.nlds_require_julia <- function() {
  msg <- "FAILED: this server is not provisioned for NoLimits (Julia): "
  if (!requireNamespace("NoLimitsR", quietly = TRUE) ||
      !requireNamespace("JuliaConnectoR", quietly = TRUE)) {
    stop(msg, "the NoLimitsR and JuliaConnectoR R packages are not installed",
         call. = FALSE)
  }
  if (!isTRUE(tryCatch(JuliaConnectoR::juliaSetupOk(), error = function(e) FALSE))) {
    stop(msg, "no suitable Julia installation was found", call. = FALSE)
  }
  tryCatch(NoLimitsR::nolimits(),
           error = function(e) stop(msg, .nlds_brief(e), call. = FALSE))
  invisible(TRUE)
}

#' A fixed, non-disclosive stand-in for a Julia error, for the client.
#'
#' A raw JuliaConnectoR message carries a stack trace naming absolute server
#' paths AND, worse, can embed a data-derived value: a Julia `DomainError` or an
#' ingestion error prints the offending number or string. Relaying any of that
#' text to the analyst is a disclosure channel, so nothing from the exception is
#' returned. The real error is written to the server's own logs instead, where
#' the data owner can read it. Callers prefix their own context (e.g. "the model
#' could not be built on this server: ").
#' @keywords internal
#' @noRd
.nlds_brief <- function(e) {
  message("dsNoLimits Julia error: ", conditionMessage(e))
  "see server logs"
}

#' Guard the MCEM/SAEM entry points on the dev_api primitives being present.
#'
#' The primitives ship on NoLimits main, which still reports version 0.2.6, so
#' the check is on `isdefined`, never on a version number. Boots Julia and evals
#' the helpers, then asks Julia whether the primitives exist.
#' @keywords internal
#' @noRd
.nlds_em_guard <- function() {
  .nlds_require_julia()
  .nlds_helpers()
  ok <- isTRUE(tryCatch(as.logical(.nlds_fun("nlds_em_available")()),
                        error = function(e) FALSE))
  if (!ok) {
    stop("FAILED: MCEM/SAEM require NoLimits >= 0.2.7 on this server",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Evaluate the Julia helper source once per R process.
#' @keywords internal
#' @noRd
.nlds_helpers <- function() {
  if (isTRUE(.nlds_state$helpers)) return(invisible(TRUE))
  NoLimitsR::nl_eval(JULIA_HELPERS)
  .nlds_state$helpers <- TRUE
  invisible(TRUE)
}

#' A callable proxy for one helper, memoised.
#'
#' The `invokelatest` wrapper is required on Julia 1.12: resolving a binding of a
#' module created after the bridge task started otherwise warns, and printing a
#' Julia error frame can hang the socket, so errors are stringified inside Julia.
#' @keywords internal
#' @noRd
.nlds_fun <- function(name) {
  key <- paste0("fun.", name)
  if (is.null(.nlds_state[[key]])) {
    .nlds_state[[key]] <- JuliaConnectoR::juliaEval(sprintf('
      let f = NoLimitsSession.%s
          (a...; k...) -> try
                  Base.invokelatest(f, a...; k...)
              catch e
                  msg = try
                      sprint(showerror, e)
                  catch
                      string(typeof(e))
                  end
                  error(first(msg, 2000))
              end
      end', name))
  }
  .nlds_state[[key]]
}

#' Shape predicate for the object an assign method bound to the analyst's symbol.
#' @keywords internal
#' @noRd
.nlds_is_prep <- function(x) {
  is.list(x) && all(c("dm", "theta0", "names", "scale", "p", "n.subjects",
                      "n.obs", "model.hash", "versions") %in% names(x))
}

#' Build the session-resident fit cache. Shared by both prep assign methods.
#'
#' @param df.name name of the data frame in the session environment.
#' @param model.source Julia `@Model` source for the model.
#' @param id.col,time.col column names.
#' @param .dsenv the session environment, captured by the calling `*DS` function.
#' @keywords internal
#' @noRd
.nlds_prep <- function(df.name, model.source, id.col, time.col, .dsenv) {
  .nlds_check_name(id.col, "id.col")
  .nlds_check_name(time.col, "time.col")

  df <- .ds_get(df.name, .dsenv, is.data.frame, "data frame")
  if (!(id.col %in% names(df)) || !(time.col %in% names(df))) {
    stop("FAILED: id.col and time.col must both be columns of the data frame",
         call. = FALSE)
  }

  # Disclosure gates before any Julia work touches the data.
  n.subjects <- length(unique(df[[id.col]]))
  if (n.subjects < .nf("nfilter.tab")) {
    stop("FAILED: the number of subjects is less than nfilter.tab", call. = FALSE)
  }

  .nlds_require_julia()
  .nlds_helpers()

  built <- tryCatch({
    nl <- NoLimitsR::nolimits()
    model <- NoLimitsR::nl_model(model.source)
    dm <- nl$DataModel(model, NoLimitsR::nl_data(df),
                       primary_id = NoLimitsR::jl_sym(id.col),
                       time_col = NoLimitsR::jl_sym(time.col))
    list(dm = dm,
         theta0 = as.numeric(.nlds_fun("nlds_theta0")(dm)),
         names = as.character(.nlds_fun("nlds_names")(dm)),
         scale = as.numeric(.nlds_fun("nlds_precondition")(dm)))
  }, error = function(e) {
    stop("FAILED: the model could not be built on this server: ",
         .nlds_brief(e), call. = FALSE)
  })

  p <- length(built$theta0)
  # Subjects, not observations: conservative for hierarchical models.
  if (p > .nf("nfilter.glm") * n.subjects) {
    stop("FAILED: the model has too many parameters for the number of subjects ",
         "on this server (nfilter.glm)", call. = FALSE)
  }

  # Warm-up: pays the Julia compile inside the prep call and proves the model
  # actually evaluates on this server's data.
  warm <- tryCatch(
    as.numeric(.nlds_fun("nlds_objgrad")(built$dm, built$theta0,
                                         .nlds_fun("nlds_method")("laplace", 1L))),
    error = function(e) {
      stop("FAILED: the model could not be evaluated on this server: ",
           .nlds_brief(e), call. = FALSE)
    })
  if (length(warm) != 1L + 2L * p) {
    stop("FAILED: the model could not be evaluated on this server: ",
         "unexpected objective result", call. = FALSE)
  }

  # MCEM M-step partition (q1 observation-side, q2 random-effect-distribution).
  # Guarded on the dev_api primitives: on a registry NoLimits without them these
  # are NULL and em.available is FALSE, and only the EM entry points care.
  em.available <- isTRUE(tryCatch(as.logical(.nlds_fun("nlds_em_available")()),
                                  error = function(e) FALSE))
  q1.names <- NULL
  q2.names <- NULL
  # SAEM hybrid M-step routing: closed-form-eligible vs numerical free names.
  cf.names <- NULL
  num.names <- NULL
  if (em.available) {
    q1.names <- tryCatch(as.character(.nlds_fun("nlds_mcem_q1")(built$dm)),
                         error = function(e) NULL)
    q2.names <- tryCatch(as.character(.nlds_fun("nlds_mcem_q2")(built$dm)),
                         error = function(e) NULL)
    cf.names <- tryCatch(as.character(.nlds_fun("nlds_saem_cf")(built$dm)),
                         error = function(e) NULL)
    num.names <- tryCatch(as.character(.nlds_fun("nlds_saem_num")(built$dm)),
                          error = function(e) NULL)
    if (is.null(q1.names) || is.null(q2.names) ||
        is.null(cf.names) || is.null(num.names)) em.available <- FALSE
  }

  list(dm = built$dm,
       theta0 = built$theta0,
       names = built$names,
       scale = built$scale,
       p = p,
       n.subjects = n.subjects,
       n.obs = nrow(df),
       # MCEM M-step partition and its availability, read back by prepInfo so the
       # client can route q1/q2 parameters. Character vectors (or NULL).
       q1.names = q1.names,
       q2.names = q2.names,
       # SAEM hybrid M-step routing (closed-form vs numerical), read by prepInfo.
       cf.names = cf.names,
       num.names = num.names,
       em.available = em.available,
       # Canonical hash of the resolved model source. Every site's copy is
       # compared client-side so a strict-mode fit provably ran the byte-identical
       # model each data owner registered.
       model.hash = .nlds_model_hash(model.source),
       # Keyed on the data CONTENTS and the model source, so re-uploading the
       # same table under a new symbol cannot mint a fresh privacy budget.
       fingerprint = .dp_fingerprint(df, model.source),
       estimator.warm = "laplace",
       versions = .nlds_versions())
}

#' Version report, used by the status method and stored in the prep cache.
#' @keywords internal
#' @noRd
.nlds_versions <- function() {
  jl <- NA_character_
  nolimits <- NA_character_
  if (isTRUE(.nlds_state$booted) ||
      isTRUE(tryCatch(!is.null(NoLimitsR::nolimits()), error = function(e) FALSE))) {
    .nlds_state$booted <- TRUE
    jl <- tryCatch(JuliaConnectoR::juliaEval("string(VERSION)"),
                   error = function(e) NA_character_)
    nolimits <- tryCatch(JuliaConnectoR::juliaEval("string(pkgversion(NoLimits))"),
                         error = function(e) NA_character_)
  }
  list(dsnolimits = as.character(utils::packageVersion("dsNoLimits")),
       nolimitsr = tryCatch(as.character(utils::packageVersion("NoLimitsR")),
                            error = function(e) NA_character_),
       nolimits = nolimits,
       julia = jl)
}
