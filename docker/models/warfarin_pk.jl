# One-compartment oral PK model (ODE) with log-normal random effects on ka, cl and v; p = 7.
@fixedEffects begin
    ka       = RealNumber(1.0)
    cl       = RealNumber(0.13)
    v        = RealNumber(8.0)
    omega_ka = RealNumber(0.4, scale=:log)
    omega_cl = RealNumber(0.3, scale=:log)
    omega_v  = RealNumber(0.2, scale=:log)
    sigma    = RealNumber(0.5, scale=:log)
end

@covariates begin
    t    = Covariate()
    Dose = ConstantCovariate(constant_on=:ID)
end

@randomEffects begin
    eta_ka = RandomEffect(LogNormal(0.0, omega_ka); column=:ID)
    eta_cl = RandomEffect(LogNormal(0.0, omega_cl); column=:ID)
    eta_v  = RandomEffect(LogNormal(0.0, omega_v);  column=:ID)
end

@preDifferentialEquation begin
    kai = ka * eta_ka
    cli = cl * eta_cl
    vi  = v * eta_v
end

@DifferentialEquation begin
    D(depot)   ~ -kai * depot
    D(central) ~ kai * depot - (cli / vi) * central
end

@initialDE begin
    depot   = Dose
    central = 0.0
end

@formulas begin
    cp = central(t) / vi
    conc ~ Normal(cp, sigma)
end
