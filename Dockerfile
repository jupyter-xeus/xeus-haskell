FROM mambaorg/micromamba:ubuntu

COPY --from=busybox:musl /bin/busybox /bin/busybox

USER root
RUN chmod 777 -R /usr/local

USER $MAMBA_USER
COPY --chown=$MAMBA_USER:$MAMBA_USER ./environment-dev.yml /tmp/env.yaml
RUN micromamba install -y -n base -f /tmp/env.yaml && \
    micromamba clean --all --yes

COPY --chown=$MAMBA_USER:$MAMBA_USER <<-EOF /usr/local/bin/runs
#!/bin/bash
set -e
for cmd in "\$@"; do
  bash -c "\$cmd"
done
EOF

RUN chmod +x /usr/local/bin/runs

WORKDIR /work
ENTRYPOINT ["/usr/local/bin/_entrypoint.sh", "runs", "cmake -B /tmp /work", "cmake --build /tmp", "cmake --install /tmp"]
CMD ["pytest"]

