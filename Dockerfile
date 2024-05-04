FROM ghcr.io/stefan-hoeck/idris2-pack

RUN apt-get update
RUN apt-get install -y golang-go

WORKDIR /idris2-go
COPY . .

RUN pack --no-prompt build idris2-go-lib.ipkg
RUN pack --no-prompt build idris2-go.ipkg
RUN pack --no-prompt test idris2-go-lib
RUN pack --no-prompt install-app idris2-go
RUN pack --no-prompt test idris2-go

