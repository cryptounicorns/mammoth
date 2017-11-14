FROM fpco/stack-build:lts-9.12 as builder
WORKDIR ./app
COPY stack.yaml package.yaml ./
RUN stack --system-ghc build --only-dependencies
COPY . .
RUN stack --system-ghc --local-bin-path /usr/local/bin install

FROM fedora:latest
COPY --from=builder /usr/local/bin/mammoth /usr/local/bin/mammoth
CMD ["mammoth"]
