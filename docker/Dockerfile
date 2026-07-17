# LinReg: Python orchestrator + R statistics engine in one image.
# Base: rocker/r-ver gives a pinned, reproducible R toolchain with build
# essentials already configured for compiling CRAN packages (lme4, glmmTMB...).
FROM rocker/r-ver:4.3.2

LABEL org.opencontainers.image.title="LinReg" \
      org.opencontainers.image.description="Automatic statistical analysis pipeline (Python orchestrator + R engine)"

ENV DEBIAN_FRONTEND=noninteractive \
    PIP_NO_CACHE_DIR=1 \
    PYTHONUNBUFFERED=1

# System deps: Python3 + pip, and libraries needed to build common R packages
# (lme4/glmmTMB need BLAS/LAPACK, curl/xml for various CRAN deps).
RUN apt-get update && apt-get install -y --no-install-recommends \
        python3 python3-pip python3-venv \
        libcurl4-openssl-dev libssl-dev libxml2-dev \
        libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
        pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/linreg

# --- R dependencies -----------------------------------------------------
# Copied and installed first so this (slow) layer is cached independently
# of Python/application code changes.
COPY R/install_packages.R R/install_packages.R
RUN Rscript R/install_packages.R

# --- Python dependencies -------------------------------------------------
COPY python/requirements.txt python/requirements.txt
RUN python3 -m pip install --upgrade pip \
    && python3 -m pip install -r python/requirements.txt

# --- Application code ------------------------------------------------------
COPY R/ R/
COPY python/ python/
COPY CONTRACT.md CONTRACT.md
COPY examples/ examples/

RUN python3 -m pip install -e ./python

# Default working directory for mounted datasets.
RUN mkdir -p /data /results
WORKDIR /data

ENTRYPOINT ["linreg"]
CMD ["--help"]
