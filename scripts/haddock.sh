#!/usr/bin/env sh -xv
base=`stack path --local-install-root`
package=`stack ls dependencies | grep de-calixtinus | sed 's/ /-/'`
stack clean --full
stack build
stack run  generate-static-exe -- -c config-s3.yaml -o . --colours
stack haddock --no-haddock-deps
cp doc/colours.svg "$base/doc/$package"
