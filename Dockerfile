FROM golang:1.9.2 as builder

WORKDIR /go/src/github.com/cryptounicorns/mammoth
COPY    . .
RUN     make

FROM fedora:latest

RUN  mkdir          /etc/mammoth
COPY                /go/src/github.com/cryptounicorns/mammoth/config.json           /etc/mammoth/config.json
COPY --from=builder /go/src/github.com/cryptounicorns/mammoth/build/mammoth  /usr/bin/mammoth

CMD [                                 \
    "/usr/bin/mammoth",        \
    "--config",                       \
    "/etc/mammoth/config.json" \
]
