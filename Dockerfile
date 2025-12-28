# Build stage
FROM haskell:9.10 as builder

WORKDIR /app

# Copy cabal files first for better caching
COPY *.cabal ./
RUN cabal update

# Copy source files
COPY . .

# Build dependencies first (for better caching)
RUN cabal build --dependencies-only --enable-executable-dynamic

# Build the application (threaded runtime is set in cabal file)
RUN cabal build trying-haskell-web

# Find and copy the executable to a known location
RUN EXEC_PATH=$(find dist-newstyle -name trying-haskell-web -type f -executable | head -1) && \
    cp "$EXEC_PATH" /tmp/trying-haskell-web && \
    chmod +x /tmp/trying-haskell-web

# Runtime stage
FROM debian:bullseye-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libffi7 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the built executable from builder
COPY --from=builder /tmp/trying-haskell-web ./trying-haskell-web

# Copy static files
COPY static ./static

# Expose port (Render.com will set PORT env var)
EXPOSE 3000

# Use PORT environment variable if set, otherwise default to 3000
ENV PORT=3000

# Run the application
CMD ["./trying-haskell-web"]

