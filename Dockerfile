FROM haskell:9.8-bullseye AS build
# Add debian packages
RUN apt-get update && apt-get install -y pkg-config libexpat1-dev libbz2-dev
# First build all the dependencies as a separate layer so we don't have to do it all over with each minor change
COPY stack.yaml package.yaml de-calixtinus.cabal /build/
WORKDIR /build
RUN stack setup
RUN stack build --only-dependencies
# Then build the application and copy configuration data into an application directory
COPY . /build/
RUN mkdir -p /app/lib && mkdir -p /app/bin && cp -r static config-docker.yaml import-arzua-santiago.json import-santiago.json import-melide-santiago.json import-villaviciosa.json camino-portuguese.json camino-fisterra.json camino-frances.json camino-ingles.json camino-norte.json camino-primitivo.json bondi-manly.json /app/lib
RUN stack install --local-bin-path /app/bin :camino-server-exe
# Finally build a clean image without all the build gunk
FROM debian:bullseye-slim
LABEL title="De Calixtinus"
LABEL description="A web server that allows users to enter travel preferences and creates a staged plan for the Camino Santiago"
LABEL version=0.7-SNAPSHOT
RUN apt-get update && apt-get install -y libexpat1 bzip2
COPY --from=build /app /app
EXPOSE 3000
ENTRYPOINT [ "/app/bin/camino-server-exe", "-s", "/app/lib/static", "-c", "/app/lib/config-docker.yaml" ]

