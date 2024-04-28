# De Calixtinus

This is a program to help people plan walking stages on the Camino Santiago.
You can supply a set of preferences in the form of fitness, preferred and hard
limits on travel distance, accommodation etc. and the program will attempt to
build a *globally* optimal set of stages for you.
Globally, means that the program will try to find the best set of stages over the
entire trip, stretching and squeezing as appropriate to try and limit the number of
ludicrously long or disappointingly short stages.

I started planning this program while walking the Camino Portugués.
It's partially a learning exercise.
I had always wanted to learn the [Haskell](https://www.haskell.org/) programming language properly but
had never had that special project to make me grit through the difficulties of learning
to program idiomatically.
It turns out that this was sufficient motivation.

## Usage

De Calixtinus runs at https://de-calixtinus.org

### Sample

To see an exmaple plan, based on someone unfit who wants to walk about 20km
a day and wants to go to via Fátima-Ansião, most of the Coastal/Littoral
routes and then transfer to Tui, look
[here](https://de-calixtinus.s3.ap-southeast-2.amazonaws.com/example/index.html).
Or [here](https://de-calixtinus.s3.ap-southeast-2.amazonaws.com/example/camino.kml) for a KML version
displayable in Google Earth.
(This route happens to correspond to the route the author took, although the planner comes
up with some different stages to the ones we chose.)

### Docker Image

The [docker image](https://hub.docker.com/repository/docker/charvolant/de-calixtinus/general)
allows you to run de calixtinus locally, if you wish to play.

### Web Server
To run the web server, use

```shell
camino-server-exe [-c CONFIG] [-s STATIC] [-d] [-r ROOT] [-p PORT] CAMINO ...
```

| Argument  | Description                                                                             | Example/default        |
|-----------|-----------------------------------------------------------------------------------------|------------------------|
| CAMINO    | A list of camino descriptions in JSON form                                              | camino-portuguese.json |
| -c CONFIG | An optional configuration file for generating the appropriate HTML output, in YAML form | ./config.yaml          |
| -s STATIC | The location of static (asset) files, such as CSS, fonts and icons                      | ./static               |                                                                     
| -r ROOT   | The root URL for links.                                                                 | 3000                   |                                                                     
| -p PORT   | The port to listen on                                                                   | http://localhost:3000  |                                                                     
| -d        | Use debug mode                                                                          |                        |                                                                     

If you are using something like nginx as a front-end, you will need to set the root so that links are correctly generated.

### Planner 

To run the simple planner, use

```shell
camino-planner-exe CAMINO PREFERENCES BEGIN END [-r ROUTES] [-s STOPS] [-x EXCLUDED] [-c CONFIG] [-o OUTPUT]
```

| Argument    | Description                                                                                                                                             | Example                |                   |
|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------|-------------------| 
| CAMINO      | A camino description in JSON form                                                                                                                       | camino-portuguese.json |                   |
| PREFERENCES | A preference file in JSON form                                                                                                                          | short-preferences.json |                   |
| BEGIN       | The identifier of the start point                                                                                                                       | P1                     | Lisbon            |
| END         | The identifier of the end point                                                                                                                         | P284                   | Santiago          |
| ROUTES      | An optional list, separated by commas, of variant routes that you wish to use. The default main route is always used.                                   | RS                     | Spiritual Variant |
| STOPS       | An optional list, separated by commas, of places where you wish to stop for the night. If absent, the standard stops from the selected routes are used. | P78,F10                | Coimbra, Fátima   |
| EXCLUDED    | An optional list, separated by commas, of places where you do not wish to stop for the night                                                            | P63                    | Zambujal          |
| CONFIG      | An optional configuration file for generating the appropriate HTML output, in YAML form                                                                 | config.yaml            |                   |
| OUTPUT      | The optional output directory to write the HTML plan to                                                                                                 | ./plan                 |                   |                                                                     

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

* ~~A full website implementation~~
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

See [doc/BUILD.md](doc/BUILD.md)
