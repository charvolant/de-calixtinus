# Camino File Format

* [Header](#header)
  * [Identifiers and References](#identifiers-and-references)
  * [Metadata](#metadata)
* [Locations](#locations)
  * [Types, Services, etc.](#types-services-etc)
  * [Accommodation](#accommodation)
    * [Generic Accommodation](#generic-accommodation)
* [Legs](#legs)
* [Routes](#routes)
* [Route Logic](#route-logic)
  * [Conditions](#conditions)

A "Camino" from the point of view of De Calixtinus is a coherent collection of
locations, legs between locations and options and variants about how one might approach
the route.
It roughly corresponds to what one might expect if someone says, 
"I'm doing the Camino Français" or a guide-book, or one of the caminos downloaded from
Buen Camino.
The camino is encoded as a huge slab of JSON.

The basic camino file has the following format:

```json
{
  "id": ...,
  "name": ...,
  "description": ...,
  "metadata" : {
    ...
  },
  "locations": [
    ...
  ],
  "legs": [
    ...
  ],
  "routes": [
    ...
  ],
  "route-logic": [
    ...
  ],
  "default-route": ...
```

The `id`, `name`, `description` and `metadata` entries for the [header](#header) and
gives descriptive information about the camino.

The `locations` and `legs` entries describe the various possible 
[waypoints](#locations) on the camino and the [connections](#legs) between them.

The `routes`, `route-logic` and `default-route` chunk the locations and legs into
optional [variants](#routes) of the main, default route.
Choosing routes can add extra possible locations to the trip or remove them if they
are redundant.
The consequences of multiple routes can get very complicated and the [logic](#route-logic)
helps untangle what's in, what's out and where you can go.

## Header

The header contains descriptive information about the camino

* `id` The camino identifier. See [below](#identifiers-and-references)
* `name` A short name for the camino. **TBD** Localised variants
* `description` Descriptive information about the camino. 
  **TBD** More structured information and localised variants.
* `metadata` See [below](#metadata)

### Identifiers and References

Most significant parts of the camino have an identifier, given by the `id` JSON field.
*Identifiers must be globally unique,* since users may mix up preferences across multiple  caminos.
The convention used in the supplied file is to have a single identifier for the camino
itself and then this is used as a prefix for the location and route identifiers.
For example, if the identifier of the camino is "F" then the identifier for a location
might be "F-M001" with the "F-" prefix ensuring that another camino doesn't have the same
location identifier.

In most cases, once a location or route has been defined, it can be referred to elsewhere
by its identifier.
So, for example, if a route wants to list the location above, it simply needs to put
"F-M001", not repeat the location information.
The camino parser will sort out all the references when it reads the file.

### Metadata

Metadata contains information about the camino file itself.
In general, it allows you to encode information about the file, what it is for,
where it came from, how current it is, etc. in a form corresponding to standards
such as [Dublin Core](https://www.dublincore.org/)
An example metadata entry is

```json
"metadata": {
    "namespaces": [
      {
        "prefix": "dc",
        "namespace": "http://purl.org/dc/elements/1.1/"
      },
      {
        "prefix": "dcterms",
        "namespace": "http://purl.org/dc/terms/"
      }
    ],
    "statements": [
      {
        "term": "dcterms:title",
        "value": "Caminho Português",
        "lang": "pt"
      },
      {
        "term": "dcterms:modified",
        "value": "2024-04-09"
      },
      {
        "term": "dcterms:version",
        "value": "0.1.1"
      },
      {
        "term": "dcterms:license",
        "value": "https://creativecommons.org/licenses/by/4.0/"
      },
      {
        "term": "dcterms:source",
        "value": "https://www.editorialbuencamino.com/"
      },
    ]
  }
```

Terms in metadata statements are (preferably resolvable) URIs.
The `namespaces` section allows you to define prefixes for these URIs so that you
do not have to write out the entirety of a term.
On the above example, `dcterms:modified` corresponds to `http://purl.org/dc/terms/modified`

The `statements` section is just a list of metadata terms and their values.
The `term` and `value` entries must be present.
An optional `lang` field allows you to specify the language the value is in.

You can have multiple statements all using the same term.
For example, multiple source statements are perfectly sensible.

## Locations

Locations describe potential stops and waypoints on the camino route.
They are usually things like towns or villages but can be something like a bridge,
a monastery or just a generic point of interest.
An example location is:

```json
{
  "id": "P-P15",
  "name": "Azambuja",
  "type": "Town",
  "position": { "latitude": 39.0695, "longitude": -8.8667 },
  "services": ["BicycleRepair", "Restaurant", "Groceries", "Pharmacy", "Medical", "Bank", "Train", "Bus" ],
  "accommodation": [
    {
      "name": "Azambuja Pilgrims Hostel",
      "type": "MunicipalAlbergue",
      "services": ["Handwash"],
      "sleeping": ["Shared"]
    },
    {
      "name": "Residencial Flôr da Primavera",
      "type": "PrivateAlbergue",
      "services": ["Kitchen", "WiFi", "Bedlinen", "Towels"],
      "sleeping": ["Single", "DoubleWC", "Triple"]
    },
    {
      "name": "Casa da Rainha",
      "type": "PrivateAlbergue",
      "services": ["WiFi", "Bedlinen", "Towels"],
      "sleeping": ["Single", "DoubleWC"]
    }
  ]
}
```

The elements are:

* `id` The globally unique identifier for the location
* `name` The name of the location
* `description` Optional descriptive information
* `href` An optional URL to more information
  **TBD** Improve descriptive modelling.
* `type` The type of location. See [below](#types-services-etc)
* `position` The position of the location in decimal latitude and longitude.
* `services` A list of the publically available services at the location. See [below](#types-services-etc).
* `accommodation` A list of the accommodation options. See [below](#accommodation)

When you are constructing a location, the position can be difficult.
Generally a latitude/logitude accurate to 5 decimal places identifies the position to
within a metre.
That enables you to place the location on a suitable road or intersection.
What the "position" of a location actually is tends to be a matter of opinion.
It seems to be a good idea to place locations at municipal albergues, if one exists, 
intersections, churches, town squares and the like.

### Types, Services, etc.

Things like location type, services, bedding, travel types and the like are 
encoded as strings.
These correspond to the values in the main [camino model](../src/Camino/Camino.hs).

### Accommodation

An accommodation entry gives an albergue, hotel, hostel and the like.
A typical accommodation entry looks like:

```json
{
  "name": "Residencial Flôr da Primavera",
  "type": "PrivateAlbergue",
  "services": ["Kitchen", "WiFi", "Bedlinen", "Towels"],
  "sleeping": ["Single", "DoubleWC", "Triple"]
}
```

* The `name` is the name of the guest house, camp site, hotel, etc.
* The `type` is the type of accommodation. See [above](#types-services-etc)
* The `services` lists the services available to guests. See [above](#types-services-etc).
  These can overlap with public servces.
* `sleeping` lists the types of sleeping arrangements available. See [above](#types-services-etc)

#### Generic Accommodation

Places like cities tend to have too many accommodation options to list.
Or you may just be feeling lazy.
Instead of listing specific accommodation, you can list generic accommodation
options instead by just giving the types of accommodation.
For example:

```json
"accommodation": [ "PrivateAlbergue", "Hotel" ]
```

Creates a generic private albergue and a hotel with typical services and sleeping arrangements.

## Legs

Legs link locations.
A leg typically describes the amount of effort needed to get from the start of the leg
to its finish.
A typical leg is:

```json
{
  "from": "P-P5",
  "to": "P-P8",
  "distance": 3.78,
  "ascent": 25,
  "descent": 5
}
```

This describes a leg on a generally navigable road or trail with a distance of 3.78km, a
total ascent of 25m and a total descent of 5m.

Legs should be generally short enough that collecting total ascent and descent into
two figures passes the sniff test.
The planner will divide the ascent and descent into two segments and calculate slop and
effort based on that approximation.

A single location can have multiple legs leaving `from` it and also multiple legs 
arriving `to` it.
The planner chooses between alternative sequences of legs based on minimum penance.

A more complex leg can include travel type and alter the time and distance calculations.
For examople, a leg by ferry, rather than walking, might be:

```json
{
  "type": "Ferry",
  "from": "P-E22",
  "to": "P-P258",
  "time": 2.5,
  "penance": 2.0,
  "distance": 0,
  "ascent": 0,
  "descent": 0,
  "notes": "Pilgrim's ferry"
}
```

* `type` gives the type of travel. See [above](#types-services-etc)
* `time` gives an explicit time of travel, rather than calculating it from fitness, distances etc.
  Generally, if you use time, set the distance, ascent and descent to zero.
* `penance` gives an extra amount of penance to apply when using this leg.
  As well as time-based legs, such as the one above, you can use this to denote a particularly
  nasty part of the route for some reason or other and use it to influence the planner.
  For example, out of two routes, the most direct passes though a particularly miserable
  industrial area.
* `notes` Allow you to add comments about the leg.

## Routes

Routes are collections of locations grouped together into named optional variants
of the main route.
For example, the *Spiritual Variant* takes the pilgrim away from the main route
and follows the path of St. James.
An example route is:

```json
{
  "id": "P-RS",
  "name": "Variante Espiritual",
  "description": "The Spiritual Variant of the main route that follows the last part of the journey of the remains of the Apostle St.James. The variant ends with a boat trip up the Río Ulla",
  "locations": [
    "P-E1", "P-E2", "P-E3", "P-E4", "P-E5", "P-E6", "P-E7", "P-E8", "P-E9", "P-E10",
    "P-E11", "P-E12", "P-E13", "P-E14", "P-E15", "P-E16", "P-E17", "P-E18", "P-E19", "P-E20",
    "P-E21", "P-E22"
  ],
  "stops": [
    "P-E21"
  ],
  "palette": {
    "colour": "F00810"
  }
}
```

* `id` The route identifier
* `name` The route name
* `description` a long-form description of the route
* `locations` a list of the locations (by identifier) that are included in the route.
  Using a route usually also *excludes* locations from the pilgrimage by bypassing them.
  Additional inclusions and exclusions can get very complicated and are handled by
  the [route logic](#route-logic) section.
* `starts` a list of common start points for people beginning their pilgrimage
* `finishes` A list of common finish points for people ending their pilgrimage
  This is usually set to Santiago de Compostella in the default route.
* `stops` Suggested stop points.
  These are usually places that have some sort of significance and where it would be
  a shame for the planner to just propel the pilgrim past.
* `palette` The colour denoting the route.

Locations not explicitly listed in other routes are assigned to the specified
default route.
The default route is always one of the possible routes selected by the planner.

## Route Logic

When routes are combined, things can get complicated as some locations become
accessible, some become unreachable and other routes become options.
The route logic section describes the effect of various route combinations.
An example route logic entry is:

```json
{
  "condition": {
    "and": [
      {
        "not": "P-RT1"
      },
      "P-RT2"
    ]
  },
  "description": "Transferring from the central route down to the coastal route",
  "requires": [ "P-RC" ],
  "allows": [ "P-RT3", "P-RT4" ],
  "prohibits": [ "P-RL" ],
  "include": [
    "P-P140", "P-P141", "P-P142", "P-P143", "P-P144", "P-P145", "P-P146", "P-P147", "P-P148", "P-P149",
    ...
  ],
  "exclude": [
              "P-C2",   "P-C3",   "P-C4",   "P-C5",   "P-C6",   "P-C7",   "P-C8",   "P-C9",   "P-C10",
    "P-C11",  "P-C12",  "P-C13",  "P-C14",  "P-C15",  "P-C16",  "P-C17",
    "P-P155", "P-P156", "P-P157", "P-P158", "P-P159",
    ...
  ]
}
```

* `condition` The conditions under which this particular set of alterations applies. See [below](#conditions)
* `description` An optional description of what this is intended to capture.
* `requires` A list of other routes that are required to be present for this combination to make sense.
  The default route is always included.
* `allows` A list of additional routes that become possible under these conditions.
  Usually, this means the route now passes the point where these options diverge.
* `prohibits` A list of routes that become impossible to use under these conditions.
  Usually, this means that those routes have been bypassed by the current combination.
* `include` Additional locations to include
* `exclude` Locations to remove from consideration

Route logic includes and excludes are applied in sequence.
It is quite possible to have locations added by one piece of route logic and then
removed by a subsequent piece.

### Conditions

Conditions are boolean algebra expressions of combinations of routes.
The possible expressions are:

* *id* True if the route with *id* has been selected.
* `"and": [ ... ]` All listed conditions must be true
* `"or": [ ... ]` At least on listed conditions must be true
* `"not": c` The condition must be false
* `true` Always true. (Not terribly useful but may be used for machine-generated conditions)
* `false` Always false

Conditions can be simple, so `"condition": "R1"` means this route logic applies if the route R1 has
been selected.
Complex conditions can be nested so "either R3 or R2 and not R1 are selected" can be expressed as:

```json
{
  "or": [
    "R3",
    "and": [
      {
        "not": "R1"
      },
      "R2"
    ]
  ]
}
```

