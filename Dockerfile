FROM terrorjack/asterius:latest

ARG jobs=1

ARG ASTERIUS_AHC_LD_IGNORE=1

COPY --chown=asterius:asterius . /tmp/build

RUN \
  cd /tmp/build && \
  ahc-cabal v1-update && \
  mkdir ~/.cabal/bin && \
  ahc-cabal v1-install -j$jobs --prefix=$ASTERIUS_LIB_DIR --package-db=clear --package-db=global --constraint "plzwrk +plzwrk-enable-asterius" plzwrk

USER root

RUN \
  rm -rf -v \
    $ASTERIUS_LIB_DIR/bin \
    /home/asterius/.cabal \
    /tmp/* \
    /var/tmp/*

USER asterius
