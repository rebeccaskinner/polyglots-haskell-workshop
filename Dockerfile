FROM haskell:8
WORKDIR /opt/build/
RUN stack build

FROM haskell:8
WORKDIR /root/
COPY --from=0 .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/example/example .
CMD ["./example"]