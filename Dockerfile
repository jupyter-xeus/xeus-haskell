FROM ghcr.io/prefix-dev/pixi:latest

WORKDIR /opt/xeus-haskell
COPY pixi.toml pixi.lock ./
RUN pixi install -e default

ENV CONDA_PREFIX=/opt/xeus-haskell/.pixi/envs/dev
ENV PATH="${CONDA_PREFIX}/bin:${PATH}"

COPY <<'EOF' /usr/local/bin/runs
#!/bin/bash
set -euo pipefail
for cmd in "$@"; do
  bash -c "$cmd"
done
EOF
RUN chmod +x /usr/local/bin/runs

WORKDIR /work
ENTRYPOINT ["/usr/local/bin/runs", "cp -vR /host/* /work", "cmake -B /tmp /work", "cmake --build /tmp", "cmake --install /tmp"]
CMD ["pytest"]
