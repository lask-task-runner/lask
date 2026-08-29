FROM debian:bookworm-slim

# Pinned to match the "1.9" series used elsewhere in this example.
ARG TERRAFORM_VERSION=1.9.8

# Debian (glibc) is used instead of an Alpine-based terraform image: Alpine
# 3.20's aarch64 python3 package fails to load pyexpat against its own libexpat
# (symbol not found), which breaks both pip- and apk-installed awscli.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        unzip \
        python3 \
        python3-pip \
        groff \
        less \
    && pip3 install --no-cache-dir --break-system-packages awscli \
    && curl -fsSL -o terraform.zip \
        "https://releases.hashicorp.com/terraform/${TERRAFORM_VERSION}/terraform_${TERRAFORM_VERSION}_linux_$(dpkg --print-architecture).zip" \
    && unzip -q terraform.zip -d /usr/local/bin \
    && rm terraform.zip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

ENTRYPOINT ["/bin/sh"]
