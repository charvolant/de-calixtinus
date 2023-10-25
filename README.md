# Camino Stage Planner

* Use dynamic programming to plan camino stages
* Inspired by TeX layout algorithm
* Calculate *penetance* for each stage
* Based on km travelled
  * Use [Naismith's Rule](https://en.wikipedia.org/wiki/Naismith%27s_rule) to handle ascent and descent
  * 1 hour for each 5km, 600m (reconsider this)
  * Tranters corrections allow for fatigue and fitness
  * Aitken 5km roads, 4km other surfaces
  * Langmuir incluides descent

## Build Notes

### Icons and Fonts

The camino planner uses an icon font [Camino-Icons.woff](fonts/Camino-Icons.woff)
built from SVG icons in the [fonts/svg](fonts/svg) directory.
A [FontForge](https://fontforge.org/) project can be found in the [fonts](fonts) directory,
which is used to generate the WOFF font.

The SVG icons use a [template](fonts/svg/template.svg) with guides for a standard 
80% above/20% below the line font.
Hoqwever, it seems to be next to impossible to import the SVG in a way that doesn't produce
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
  
### text-icu

The `text-icu` package allows canonicalisation of unicode characters.
This package depends on the [icu4c](https://icu.unicode.org/) library.
For MacOS, you will need to have [brew](https://brew.sh/) installed and install the library
and the pkg-config cask.

```shell
brew install pkg-config
brew install icu4c
```

See https://hackage.haskell.org/package/text-icu