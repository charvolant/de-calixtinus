ARG VERSION="0.10-SNAPSHOT"
FROM haskell:9.8-bullseye AS build
ARG VERSION
# Add debian packages
RUN apt-get update && apt-get install -y pkg-config libexpat1-dev libbz2-dev
# First build all the dependencies as a separate layer so we don't have to do it all over with each minor change
COPY stack.yaml package.yaml de-calixtinus.cabal /build/
WORKDIR /build
RUN stack setup
RUN stack build --only-dependencies
# Then build the application and copy configuration data into an application directory
COPY . /build/
RUN mkdir -p /app/lib && mkdir -p /app/bin && mkdir -p /app/data && mkdir -p /app/data/features && mkdir -p /app/data/images && cp -r static import-*.json  camino-*.json bondi-manly.json /app/lib
RUN case "$VERSION" in \
    *-SNAPSHOT) \
      sed "s/S3/https:\/\/de-calixtinus-devel.s3.ap-southeast-2.amazonaws.com\/$VERSION/" < config-docker-template.yaml > /app/lib/config.yaml ;;\
    *) \
      sed "s/S3/https:\/\/de-calixtinus.s3.ap-southeast-2.amazonaws.com\/$VERSION/" < config-docker-template.yaml > /app/lib/config.yaml; ;; \
    esac
RUN stack install --local-bin-path /app/bin :camino-server-exe
# Finally build a clean image without all the build gunk
FROM debian:bullseye-slim
ARG VERSION
LABEL title="De Calixtinus"
LABEL description="A web server that allows users to enter travel preferences and creates a staged plan for the Camino Santiago"
LABEL version="$VERSION"
RUN apt-get update && apt-get install -y libexpat1 bzip2
COPY --from=build /app /app
EXPOSE 3000
ENTRYPOINT [ "/app/bin/camino-server-exe", "-s", "/app/lib/static", "-f", "/app/data/features", "-i", "/app/data/images", "-c", "/app/lib/config.yaml" ]

