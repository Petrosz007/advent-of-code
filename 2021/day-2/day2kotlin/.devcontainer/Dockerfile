FROM alpine:3.15

RUN apk add --no-cache bash curl git zip openjdk17 just && \
  curl -s "https://get.sdkman.io" | bash
RUN bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && sdk version && sdk install kotlin && sdk install gradle 7.3.1"

ENTRYPOINT ["bash"]