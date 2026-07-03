FROM dyne/devuan:excalibur

ENV TERM=vt100
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    wget \
    ca-certificates \
    git \
    make \
    gcc \
    g++ \
    sbcl \
    clisp \
    ecl \
    gcl \
    zip \
    unzip \
    xz-utils \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /lisp-binary
