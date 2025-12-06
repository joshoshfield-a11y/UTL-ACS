FROM haskell:9.4 as builder
WORKDIR /app
RUN apt-get update && apt-get install -y --no-install-recommends zlib1g-dev libpq-dev && rm -rf /var/lib/apt/lists/*
COPY . .
RUN cabal update && cabal build utl-acs-exe --enable-tests --disable-library-profiling --disable-profiling

FROM gcr.io/distroless/cc-debian12-amd64
WORKDIR /usr/local/bin
COPY --from=builder /app/dist-newstyle/build/x86_64-linux/ghc-9.4.*/utl-acs-1.0.0.0/x/utl-acs-exe/build/utl-acs-exe/utl-acs-exe /usr/local/bin/utl-acs-exe
ENV UTL_ACS_ENV="PRODUCTION_HARDENED" \
    Z_ANCHOR_MANDATE="ENFORCED"
ENTRYPOINT ["/usr/local/bin/utl-acs-exe"]
