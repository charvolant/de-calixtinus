# Camino Planner

This is a program to help people plan walking stages on the Camino Santiago.
You can supply a set of preferences in the form of fitness, preferred and hard
limits on travel distance, accomodation etc. and the program will attempt to
build a *globally* optimal set of stages for you.
Globally, means that the program will try to find the best set of stages over the
entire trip, stretching and squeezing as appropriate to try and limit the number of
ludicrously long or disappointingly short stages.

To see an example, based on someone unfit who wants to walk about 20km
a day and wants to go to via Fátima-Ansião, most of the Coastal/Littoral 
routes and then transfer to Tui, look 
[here](https://camino-planner.s3.ap-southeast-2.amazonaws.com/example/index.html).
Or [here](https://camino-planner.s3.ap-southeast-2.amazonaws.com/example/camino.kml) for a KML version 
displayable in Google Earth.
(This route happens to correspond to the route the author took, although the planner comes
up with some different stages to the ones we chose.)

I started planning this program while walking the Camino Portugués.
It's partially a learning exercise.
I had always wanted to learn the [Haskell](https://www.haskell.org/) programming language properly but
had never had that special project to make me grit through the difficulties of learning
to program idiomatically.
It turns out that this was sufficient motivation.

## Usage

To run the program, use

```shell
camino-planner-exe CAMINO PREFERENCES BEGIN END [-r ROUTES] [-s STOPS] [-x EXCLUDED] [-c CONFIG] [-o OUTPUT]
```

| Argument    | Description                                                                                                                                             | Example                |                  |
|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------|------------------| 
| CAMINO      | A camino description in JSON form                                                                                                                       | lisbon-porto.json      |                  |
| PREFERENCES | A preference file in JSON form                                                                                                                          | short-preferences.json |                  |
| BEGIN       | The identifier of the start point                                                                                                                       | P1                     | Lisbon           |
| END         | The identifier of the end point                                                                                                                         | P284                   | Santiago         |
| ROUTES      | An optional list, separated by commas, of variant routes that you wish to use. The default main route is always used.                                   | RS                     | Spritual Variant |
| STOPS       | An optional list, separated by commas, of places where you wish to stop for the night. If absent, the standard stops from the selected routes are used. | P78,F10                | Coimbra, Fátima  |
| EXCLUDED    | An optional list, separated by commas, of places where you do not wish to stop for the night                                                            | P63                    | Zambujal         |
| CONFIG      | An optional configuration file for generating the appropriate HTML output, in YAML form                                                                 | config.yaml            |                  |
| OUTPUT      | The optional output directory to write the HTML plan to                                                                                                 | ./plan                 |                  |                                                                     

At the moment, you have to use the location identifiers when specifying start- and
end-points, etc.
You will need to open the camino file and note down the `id` value.

By default, the planner will choose the quickest and easiest route.
You can use the required stops option to control this.
For example, if you want to visit Fátima add `-s F10` to the command.

## Sources

The Camino Planner needs data.
In particular, the various waypoints, services, accommodation, distances, climbs,
descents and what-have you need to be described in loving detail.
The sample data has been collected by working through a number of sources.
Most of these we used when walking the Camino ourselves.

* The major source is the excellent **Buen Camino** app.
  This app gives most of the information above in a useful form while walking the Camino.
  It also updates dynamically and can be used to load multiple Camino routes.
  You can get more information about this at the Apple Store, Google Play or at the
  [app website](https://www.editorialbuencamino.com/) (Information used with permission.)
* A secondary source is *The Camino Portugués* by Kat Davis (Cicerone, 2019), which we
  used as a planning book.
* Good-old [Google Maps](https://www.google.com/maps) was used to estimate total ascent and descent between
  waypoints and fix suitable waypoint locations.
  Google Maps was also used to estimate train and bus stations.
* The [Stingy Nomads](https://stingynomads.com/camino-fatima-walk-lisbon-porto/) aided us on 
  our route to Fátima.
* And, of course, we did our own stuff.

### Beware

This is intended to be a helpful tool for people planning their trip.
Just because "Computer Says So!" doesn't mean it's a good idea.
So use your own judgement.
In particular, the planner doesn't really distinguish between an interesting place
to stop, full of sights and entertainment, and a boring, dusty truckstop beside a
major highway.
Although I hasten to add that the truckstops we did stop at were wonderfully hospitable,
so whatever floats your boat.

I've done my best to be accurate about the data but ...
if you're relying on something to be true, check with other sources, as well.
Also, things change: places to stay come and go, temporarily shut down
or get filled up; roads get blocked; on Sunday *everything* shuts down and you need
to plan accordingly.
This output from this program is no substitute for either careful planning or
casual resilience in the face of adversity.

## A Work in Progress

The eventual aim is to have this with a nice front-end that allows users to
enter preferences and get a nicely formatted plan.
There's a bit of a way to go for that.

### The TODO List

* A full website implementation
* Start scoring stages (see the [design notes](#some-design-notes)) for
  * ~~Services available at the end-point and at accommodation (eg clothes washing facilities)~~
  * ~~Services available along the way (eg. an ATM somewhere during the day)~~
  * Handling the Sunday famine
* Include points of interest
  * And make allowance for breaks so that people can visit

### Some design notes

* Uses dynamic programming to plan camino stages
* Inspired by TeX layout algorithm
* Calculate a *penance* for each stage.
  * "Penance" is my own in-joke, following a comment by a hotel owner that we'd paid for all our sins at once
    after a particularly miserable day.
  * Penance is used to optimise stages and essentially converts everything
    into an equivalent of kilometres walked.
    * Once you step outside your preferred distance band, additional penance will be
      added to the stage's score.
      * There are also hard maximum and minimum distances.
    * Accommodation is, essentially, the number of extra kilometres you would walk to avoid a
      particular style of accommodation.
    * There is a "day cost" that factors in the business of having to stop of the night.
      Reducing the day cost to zero will tend to make the program give you a lot of
      short stages.
      Making the day cost large will cause the program to try to extend the amount of
      walking that you do each day.
  * The program works to minimise total penance but, if you're that sort of
    pilgrim, it could be made to maximise penance, instead.
* Time is estimated from kilometres travelled
  * [Naismith's Rule](https://en.wikipedia.org/wiki/Naismith%27s_rule) to handles the extra time taken for ascent and descent 
    or, preferentially [Tobler's Hiking Function](https://en.wikipedia.org/wiki/Tobler%27s_hiking_function)
  * [Tranter's corrections](https://en.wikipedia.org/wiki/Naismith%27s_rule#Tranter's_corrections) allow for fatigue and fitness
  * These are used to build a *perceived* distance travelled, taking into account
   ascent, descent and building fatigue.
 
## Build Notes

To build this program, you will need a haskell distribution and the
[stack](https://docs.haskellstack.org/en/stable/) build tool.
The easiest way to install all this is via [ghcup](https://www.haskell.org/ghcup/) system.

Once you have that, `stack build` will build the programs.
You will need to watch the log to determine where stack puts the resulting executable, 
since it will be down a long rabbit-hole.

### Icons and Fonts

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
  
### text-icu

The `text-icu` package allows canonicalisation of unicode characters.
This package depends on the [icu4c](https://icu.unicode.org/) library.
For macOS, you will need to have [brew](https://brew.sh/) installed and install the library
and the pkg-config cask.

```shell
brew install pkg-config
brew install icu4c@73
```

See https://hackage.haskell.org/package/text-icu

If the underlying icu4c library changes version, you may need to unregisteer the library and
rebuild, using

```shell
stack exec -- ghc-pkg unregister --force text-icu
stack build
```

### Customised Bootstrap

The customised bootstrap is the basic Bootstrap 5.3 with some colour adjustments.
The background has been made "older-looking" and the blue changed to the
blue found on the Camino tiles.
To create the customised boostrap, you will need `scss`, usually installed with `npm`.
You will also need the [bootstrap source](https://getbootstrap.com/docs/5.3/getting-started/download/) in a separate directory.
After that, you can run the following in the project directory

```shell
sass ./scss/custom.scss ./static/css/bootstrap-dc.css
```