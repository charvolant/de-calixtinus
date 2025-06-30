
# Build Notes

To build this program, you will need a haskell distribution and the
[stack](https://docs.haskellstack.org/en/stable/) build tool.
The easiest way to install all this is via [ghcup](https://www.haskell.org/ghcup/) system.
Or you could install via the instructions at [haskellstack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Once you have that, `stack build` will build the programs.
You will need to watch the log to determine where stack puts the resulting executable,
since it will be down a long rabbit-hole.

## Static Files

The files in the `static` subdirectory can be served directly,
without generation.
If you change the CSS or other templated static files, run
`generate-static-exe -c CONFIG` to generate the appropriate CSS files.

## Docker Image

To create a docker image, make sure that you have the 
static files up to date and then use

```shell
docker build . -t yourname/de-calixtinus:0.7-SNAPSHOT
```

substituing your docker username for `yourname`.
This builds the current source in a docker container and then
creates a slimmed-down version for deployment.

## Icons and Fonts

The camino planner uses an icon font [Camino-Icons.woff](fonts/Camino-Icons.woff)
built from SVG icons in the [fonts/svg](fonts/svg) directory.
A [FontForge](https://fontforge.org/) project can be found in the [fonts](fonts) directory,
which is used to generate the WOFF font.

The SVG icons use a [template](fonts/svg/template.svg) with guides for a standard
80% above/20% below the line font.
However, it seems to be next to impossible to import the SVG in a way that doesn't produce
overly small glyphs.
The general procedure that works is:

1. Select the appropriate glyph
2. Name the glyph via the `Glyph Info ...` dialog
3. Open the outline via `New Outline Window`
4. `Import ...` the SVG, save and close
5. Select all new icons
6. Use `Element > Transformations > Transform ...` with a `Scale Uniformly` of 150%
7. Use `Metrics > Auto-Width ...` with a separation of 10 to set widths
8. Use `Metrics > Set Both Bearings ...` with bearings of 5

Leaving the "action" icons, such as the globe and information symbol unscaled seems sensible.

## text-icu

**No longer required**
Using `text-icu` seems to create constant headaches in terms of compilation and
installation and has been replaced by a simple system that canonicalises most
romance language accented letters.
This may be a bad choice, which is why the instructions are still here.

The `text-icu` package allows canonicalisation of unicode characters.
This package depends on the [icu4c](https://icu.unicode.org/) library.
For macOS, you will need to have [brew](https://brew.sh/) installed and install the library
and the pkg-config cask.

```shell
brew install pkg-config
brew install icu4c@73
```

See https://hackage.haskell.org/package/text-icu

If the underlying icu4c library changes version, you may need to unregister the library and
rebuild, using

```shell
stack exec -- ghc-pkg unregister --force text-icu
stack build
```

## Customised Bootstrap

The customised bootstrap is the basic Bootstrap 5.3 with some colour adjustments.
The background has been made "older-looking" and the blue changed to the
blue found on the Camino tiles.
To create the customised boostrap, you will need `scss`, usually installed with `npm`.
You will also need the [bootstrap source](https://getbootstrap.com/docs/5.3/getting-started/download/) in a separate directory.
After that, you can run the following in the project directory

```shell
sass ./scss/custom.scss ./static/css/bootstrap-dc.css
```

## Stack can't build

It appears that the yesod build will sometimes fail due to
problems parsing some UTF-8 encoded files.
This manifests as an error of the form
*happy: src/Language/JavaScript/Parser/Grammar7.y: hGetContents: invalid argument (invalid byte sequence)*
Ensure that the `LANG` environment variable is set with a suitable encoding
`export LANG=C.UTF-8` or `export LANG=en_US.UTF-8`

See https://stackoverflow.com/questions/70803989/cannot-compile-yesod-hgetcontents-invalid-argument
