# Julia-baked Rock profile image for dsNoLimits.
#
# Build once on an x86_64 host, push, and hand the tag to your data owner; nobody
# compiles Julia or R at analysis time. Every version and git ref is a build ARG
# so a data owner can bump one without editing this file:
#
#   docker build -t dsnolimits/rock-nolimits:0.0.3 docker/
#
# The base tag encodes BOTH dsBase and R versions as <dsBase>-R<Rver>. Match the
# dsBase your analysts' dsNoLimitsClient expects; 6.3.5 is what this package was
# developed and tested against.
FROM datashield/rock-base:6.3.5-R4.5.2

LABEL org.opencontainers.image.source="https://github.com/manuhuth/dsNoLimits"

# Pin everything: a rebuild of this file must reproduce the same library.
# Julia >= 1.10 is required by NoLimits; 1.10 is the current LTS. A Julia MINOR
# bump invalidates the precompile cache, so pin the full patch.
ARG JULIA_VER=1.10.9
ARG JULIA_MM=1.10
# R-side git refs. DSNOLIMITS_ORG is a placeholder until the repo is public.
ARG NOLIMITSR_REF=main
ARG DSNOLIMITS_ORG=manuhuth
ARG DSNOLIMITS_REF=v0.0.3

ENV ROCK_LIB=/var/lib/rock/R/library
# The depot must be a BAKED, world-readable location. Rock gives each session a
# temp working dir (unixtools::set.tempdir), so a depot under tempdir() is wiped
# between sessions and every session would recompile. Baking it here and making
# it world-readable means the runtime (uid 10041, user rock) only ever READS it.
ENV JULIA_DEPOT_PATH=/opt/julia-depot
# One Julia thread by default: Julia threads and OpenBLAS threads both try to
# claim the node's cores next to the other R sessions. Let the owner raise it.
ENV JULIA_NUM_THREADS=1

LABEL dsNoLimits=$DSNOLIMITS_REF julia=$JULIA_VER

# 1. Julia runtime (~1 GB unpacked). x86_64 tarball; Rock images are amd64.
RUN curl -fsSL "https://julialang-s3.julialang.org/bin/linux/x64/${JULIA_MM}/julia-${JULIA_VER}-linux-x86_64.tar.gz" \
      | tar -xz -C /opt && \
    ln -s /opt/julia-${JULIA_VER}/bin/julia /usr/local/bin/julia

# 2. R side: JuliaConnectoR (the bridge), NoLimitsR (the Julia interop layer),
#    and dsNoLimits itself, all into the DataSHIELD library. One RUN so the final
#    chown covers every file and the layer cache is reused.
RUN Rscript -e "install.packages('JuliaConnectoR', repos='https://cloud.r-project.org')" && \
    Rscript -e "remotes::install_github('manuhuth/NoLimitsR', ref='${NOLIMITSR_REF}', dependencies=TRUE, upgrade=FALSE, lib='${ROCK_LIB}')" && \
    Rscript -e "remotes::install_github('${DSNOLIMITS_ORG}/dsNoLimits', ref='${DSNOLIMITS_REF}', dependencies=TRUE, upgrade=FALSE, lib='${ROCK_LIB}', repos=c('https://cloud.r-project.org','https://cran.obiba.org'))" && \
    chown -R rock ${ROCK_LIB}

# 3. Warm the shared NoLimitsR Julia environment INTO the baked depot. Without
#    this, the first NoLimits call in every session pays multi-second JIT.
#    install_nolimits() populates the shared "@NoLimitsR" environment with the
#    registered NoLimits release (>= 0.2.6); the precompile bakes the compile
#    caches; chmod makes the whole depot world-readable so the runtime never
#    needs to write it.
RUN Rscript -e "NoLimitsR::install_nolimits()" && \
    julia --project=@NoLimitsR -e 'using Pkg; Pkg.precompile()' && \
    chmod -R a+rX ${JULIA_DEPOT_PATH}

# 4. Build-time guards (the cheapest possible check that the image is usable).
#    a. dsNoLimits actually installed; b. inst/DATASHIELD made it into the
#    installed package - the single most common cause of "installed but
#    invisible"; c. Julia boots through the bridge, so a session will not fall
#    back to the provisioning error.
RUN Rscript -e "if (!requireNamespace('dsNoLimits', quietly = TRUE, lib.loc = '${ROCK_LIB}')) quit(status = 1)" && \
    Rscript -e "stopifnot(nchar(system.file('DATASHIELD', package = 'dsNoLimits', lib.loc = '${ROCK_LIB}')) > 0)" && \
    Rscript -e "if (!isTRUE(JuliaConnectoR::juliaSetupOk())) quit(status = 1)"

# 5. Registered models a strict-profile owner exposes. Copy into the modelDir the
#    profile's dsNoLimits.modelDir option points at (see setup_opal.R).
COPY models/ /var/lib/rock/nolimits-models/
RUN chown -R rock /var/lib/rock/nolimits-models
