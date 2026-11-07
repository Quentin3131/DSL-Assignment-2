# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.245.2/containers/java/.devcontainer/base.Dockerfile

# [Choice] Java version: 21, 25 (append -bookworm or -trixie to pin the OS)
ARG VARIANT="25"
FROM mcr.microsoft.com/devcontainers/java:${VARIANT}

# Graphviz is used by PlantUML to render diagrams; the X11/freetype libraries
# are needed by the JDK's AWT when PlantUML generates images.
RUN apt-get update \
    && apt-get -y install --no-install-recommends graphviz libfreetype6 libxi6 libxtst6 libxrender1 libxext6 \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN su vscode -c "umask 0002 && . /usr/local/sdkman/bin/sdkman-init.sh && sdk install scala 3.9.0"
RUN su vscode -c "umask 0002 && . /usr/local/sdkman/bin/sdkman-init.sh && sdk install sbt 2.0.9"
