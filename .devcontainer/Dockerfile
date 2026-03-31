FROM haskell:9.10.1

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        alex \
        ca-certificates \
        curl \
        g++ \
        gcc \
        git \
        happy \
        libgomp1 \
        make \
        pkg-config \
        z3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspaces/hydrangea-compiler
