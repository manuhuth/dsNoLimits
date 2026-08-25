# Closed-form one-compartment IV model with a log-normal random effect on clearance (no ODE solver).
@fixedEffects begin
    cl    = RealNumber(1.0)
    v     = RealNumber(10.0)
    omega = RealNumber(0.3, scale=:log)
    sigma = RealNumber(0.5, scale=:log)
end

@covariates begin
    t    = Covariate()
    Dose = ConstantCovariate(constant_on=:ID)
end

@randomEffects begin
    eta_cl = RandomEffect(LogNormal(0.0, omega); column=:ID)
end

@formulas begin
    cli = cl * eta_cl
    conc ~ Normal(Dose / v * exp(-(cli / v) * t), sigma)
end
