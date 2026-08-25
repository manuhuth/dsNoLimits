# Test setup. Julia-dependent tiers skip themselves; see stage 1.
# The MCEM/SAEM tiers need the EM dev-API primitives, which ship in
# NoLimits >= 0.2.7; NoLimitsR resolves them from its default shared env.
