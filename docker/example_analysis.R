# Analyst-side end-to-end fit against three Opal-backed sites. This is the DSLite
# example (dsNoLimitsClient/inst/examples/warfarin_dslite.R) retargeted from the
# DSLiteDriver to the OpalDriver; the model string is identical.
#   Rscript docker/example_analysis.R
#
# The data owner has loaded three warfarin study tables (warfarin.site1/2/3,
# columns ID/t/Dose/conc) and enabled the "nolimits" profile - see setup_opal.R.

library(DSI)
library(DSOpal)
library(dsNoLimitsClient)

model <- "
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
"

# driver is the S4 CLASS name, "OpalDriver", not the "Opal" constructor.
b <- DSI::newDSLoginBuilder(.silent = TRUE)
b$append(server = "site1", url = "http://localhost:8080", table = "warfarin.site1",
         user = "administrator", password = "password",
         driver = "OpalDriver", profile = "nolimits")
b$append(server = "site2", url = "http://localhost:8080", table = "warfarin.site2",
         user = "administrator", password = "password",
         driver = "OpalDriver", profile = "nolimits")
b$append(server = "site3", url = "http://localhost:8080", table = "warfarin.site3",
         user = "administrator", password = "password",
         driver = "OpalDriver", profile = "nolimits")
conns <- DSI::datashield.login(b$build(), assign = TRUE, symbol = "D")

# String mode sends the model source (a permissive-profile method). On a strict
# profile the owner registers the model instead and the analyst names it:
#   ds.nolimitsFit(model = "warfarin_pk", data = "D", model.mode = "registered", ...)
fit <- ds.nolimitsFit(model = model, data = "D", id.col = "ID", time.col = "t",
                      estimator = "laplace", model.mode = "string")
print(fit)
print(summary(fit))

DSI::datashield.logout(conns)
