FROM haskell:9.6 as build
# First build all the dependencies as a separate layer so we don't have to do it all over with each minor change
COPY stack.yaml package.yaml de-calixtinus.cabal /build/
WORKDIR /build
RUN stack setup
RUN stack build --only-dependencies
# Then build the application and copy configuration data into an application directory
COPY . /build/
RUN mkdir -p /app/lib && mkdir -p /app/bin && cp -r static config-static.yaml camino-portuguese.json camino-fisterra.json /app/lib
RUN stack install --local-bin-path /app/bin :camino-server-exe
# Finally build a clean image without all the build gunk
FROM debian:buster-slim
LABEL title="De Calixtinus"
LABEL description="A web server that allows users to enter travel preferences and creates a staged plan for the Camino Santiago"
LABEL version=0.2
COPY --from=build /app /app
EXPOSE 3000
CMD /app/bin/camino-server-exe -s /app/lib/static -c /app/lib/config-static.yaml /app/lib/camino-portuguese.json /app/lib/camino-fisterra.json

