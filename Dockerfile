FROM ubuntu:20.04

# Set working directory
WORKDIR /app

# Install OpenJDK, curl, and make
RUN apt-get update && \
    apt install openjdk-17-jdk openjdk-17-jre -y && \
    apt-get install -y curl && \
    curl -sSLf https://scala-cli.virtuslab.org/get | sh && \
    rm -rf /var/lib/apt/lists/*

# Set PATH
ENV PATH=/root/.local/share/coursier/bin:$PATH

# Set entry point to source ~/.profile and then execute command
ENTRYPOINT ["/bin/bash", "-c", "source ~/.profile && exec \"$@\"", "--"]

# Default command
CMD ["jshell"]