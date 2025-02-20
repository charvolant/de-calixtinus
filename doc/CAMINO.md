# Camino File Format

* [Metadata](#metadata)
* [Localisation](#localisation)
  * [Supported Languages](#supported-languages)
  * [Tagged Text](#tagged-text)
  * [Localised Text](#localised-text)
  * [Localised URLs](#localised-urls)
* [Description](#description)
  * [Images](#images)
  * [Notes](#notes)
  * [Calendar and Times](#calendar-and-times)
* [Header](#header)
  * [Identifiers and References](#identifiers-and-references)
* [Locations](#locations)
  * [Types, Services, etc.](#types-services-etc)
  * [Accommodation](#accommodation)
    * [Generic Accommodation](#generic-accommodation)
  * [Points of Interest](#points-of-interest)
  * [Events](#events)
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
  "fragment": ...,
  "imports": [
    ...
  ],
  "locations": [
    ...
  ],
  "legs": [
    ...
  ],
  "transport-links": [
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

The `imports` list gives a list of the ids of other partial caminos to import into this  one.
Imports allow you to record the data for shared route segments in a shared
definition file and then import it into other caminos.
This can be used, for example, to not have to enter the same information
about Santiago de Compostella into every single camino definition.
The `fragment` entry is true for a fragment of camino that is only intended as an import.

The `locations` and `legs` entries describe the various possible 
[waypoints](#locations) on the camino and the [connections](#legs) between them.
`transport-links` are leg-like links to nearby locations that can be used by people
not adhering strictly to the conditions of the Compostela Certificate.
People can use transport links to end their day in one location and use a nearby location
for accommodation, food and services.

The `routes`, `route-logic` and `default-route` chunk the locations and legs into
optional [variants](#routes) of the main, default route.
Choosing routes can add extra possible locations to the trip or remove them if they
are redundant.
The consequences of multiple routes can get very complicated and the [logic](#route-logic)
helps untangle what's in, what's out and where you can go.


## Metadata

Metadata contains information about the camino file itself, or things like
images that need appropriate attribution.
In general, it allows you to encode information about the something, what it is for,
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
The `dc:` and `dcterms:` namespaces are automatically included and do not need to
be declared.

The `statements` section is just a list of metadata terms and their values.
The `term` and `value` entries must be present.
An optional `lang` field allows you to specify the language the value is in.

You can have multiple statements all using the same term.
For example, multiple source statements are perfectly sensible.

## Localisation

Many parts of a camino file can be localised, with different names and descriptions
appearing depending on the requested languages.

### Supported Languages

| Language   | Code | Messages | Camino | Date & Time |
|------------|------|----------|--------| --- |
| Basque     | eu   | N | N      | Y |
| English    | en   | Y        | Y      | Y |
| French     | fr   | N | N      | Y |
| Gallacian  | ga   | N | Y      | Y |
| Portuguese | pt   | N        | Y      | Y |
| Spanish    | es   | N        | Y      | Y |

Not all languages are fully supported.
Messages mean that labels and text on the web pages can be expressed in the requested language.
Languages supported on the Camino means that there are some names, descriptions etc.
specifically in a Camino file.
Date & Time means that months and days of the week have language-specific versions.

### Tagged Text

The key element for localisation is a piece of *tagged text* where a language-tag
of the form "@code" at the end of the text denotes the language.
For example,
`mystery number one@en` is in English and
`misterio número uno@es` is in Spanish.

If there is no tag, then the tag is assumed to be the universal tag `*`.

### Localised Text

Tagged text can be grouped together to provide localisable variants by using a
JSON list.
For example:

```json
{
  "name": [
    "Lisboa@pt",
    "Lisbon@en"
  ]
}
```

means that the name is *Lisboa* in Portuguese and *Lisbon* in English.

If there is no matching language, then the first element of the list is chosen.
Note that including an entry with a universal tag will always match a requested language.

If there is only one entry, then a string can be used instead of an array.
For example,

```json
"name": "Statue of King Denis I@en"
```

and

```json
"name": [
  "Statue of King Denis I@en"
]
```

are equivalent.

### Localised URLs

Localised URLs allow locale-specific links to external resources.
A simple URL can be written as a string, for example

```json
"about": "https://wikipedia.org/wiki/Lisbon"
```

A group of localised URLs can be structured as a list, for example

```json
"about": [
    {
        "locale": "pt",
        "url": "https://pt.wikipedia.org/wiki/Lisboa",
        "title": "Lisboa"
    }.
    {
        "locale": "en",
        "url": "https://en.wikipedia.org/wiki/Lisbon",
        "title": "Lisbon"
    }.
]
```

In the latter case, the element of each entry are:

* `locale` A [language code](#supported-languages)
* `url`: The localised URL
* `title` An optional title giving link or title text

## Description

Descriptive information appears in multiple places and
gathers together descriptive text, images, additional notes and links
to external information.
In general, if there is a `description` field then it will take this format.

* `summary` A simple summary line, which may be [localised](#localisation). If absent the text is used where a summary is needed.
* `text` Descriptive text, which may be [localised](#localisation). If descriptive text is used, then the summary is not.
* `about` A URL link, which may be [localised](#localisation)
* `image` An image associated with the description. See [below](#images)
* `notes` A list of additional notes. See [below]()

A description field can also be a simple [tagged](#tagged-text) string, in which case it will
be created as a mostly empty description with `text`.

### Images

Images allow an image to be linked to descriptive text.
In general a thumbnail of the image is shown alongside the description; 
clicking on the thumbnail opens the full-sized image in a dialog box.

Image URLs may be relative, in which case they are resolved against the
`images` URL in the configuration.

* `source` A URL to the full-size image source.
* `thumbnail` An optional URL to a thumbnail of the image.
* `title` An image title, which may be [localised](#localisation)
* `metadata` [Metadata](#metadata) about the image. This should contain dcterms for things like rights, licensing etc.

Since images are likely to have come from a third party, the metadata statement
can be used to provide information on citation, attribution and licensing.
These can be expressed in Dublin Core as `dcterms` or `dc` URIs and a suitable
attribution statement will be assembled out of the elements.

Example image JSON, for an image drawn from Wikipedia, is shown below

```json
{
  "source": "P/P-P1-Lisbon.jpg",
  "thumbnail": "P/P-P1-Lisbon-thumbnail.jpg",
  "title": "Praça do Comércio@pt",
  "metadata": {
    "statements": [
      {
        "term": "dcterms:creator",
        "value": "Berthold Werner"
      },
      {
        "term": "dcterms:rightsHolder",
        "value": "Wikimedia Commons"
      },
      {
        "term": "dcterms:license",
        "value": "CC BY-SA 4.0 <https://creativecommons.org/licenses/by-sa/4.0>"
      },
      {
        "term": "dcterms:source",
        "value": "https://commons.wikimedia.org/wiki/File:Lisbon_Pra%C3%A7a_do_Com%C3%A9rcio_BW_2018-10-03_13-33-44_s.jpg"
      }
    ]
  }
}
```

### Notes

Notes represent additional information that can be drawn away from the more
general descriptive text.
There are four types of notes: `Information`, `Warning`, `Address`, `Directions`
with each note type providing a visual indication of the sort of thing the note
is about.

If the type is absent, `Information` is assumed and a piece of [tagged text](#tagged-text)
is assumed to be an information note.
Otherwise, a note is a JSON object with a `type` and `text` field. For example

```json
{
  "notes": [
    "The crossing is the only way across the river.",
    {
      "type": "Directions",
      "text": "The crossing can be seen from the overpass. Go down the stairs to the left.@en"
    }
    {
      "type": "Warning",
      "text": [
        "busy road@en",
        "errepide okupatua@eu"
      ]
    }
  ]
}
```

contains a simple information note in English, directions in English and a 
warning in both English and Basque.

### Calendar and Times

Some items occur only on certain days or at particular times.

A calendar can be either daily (the default by implication) or
occur on certain days of the week or month, or certain months in the year.

The types of calendar are:

```json
[
  {
    "type": "daily"
  },
  {
    "type": "weekly",
    "days": [ "monday", "friday"]
  },
  {
    "type": "monthly",
    "days": [ 8, 15 ]
  },
  {
    "type": "yearly",
    "months": [ 5, 6, 7 ]
  },
  {
    "type": "day-of-year",
    "days": [ "04-21", "10-23" ]
  },
  {
    "type": "range",
    "from": {
      "type": "day-of-year",
      "days": ["01-01"]
    },
    "to": {
      "type": "weekly",
      "days": ["friday"]
    }
  },
  {
    "type": "union",
    "calendars": [
      {
        "type": "named",
        "key": "Christmas"
      },
      {
        "type": "named",
        "key": "BoxingDay"
      }
    ]
  },
  {
    "type": "intersection",
    "calendars": [
      {
        "type": "monthly",
        "days": [ 1, 8, 15 ]
      },
      {
        "type": "weekly",
        "days": [ "monday", "wednesday"]
      }
    ]
  },
  {
    "type": "except",
    "calendar": [
      {
        "type": "yearly",
        "months": [ 1, 2, 3 ]
      }
    ]
  },
  {
    "type": "nth-day-after",
    "nth": 4,
    "calendar": [
      {
        "type": "monthly",
        "days": [ 1, 14, 21 ]
      }
    ]
  },
  {
    "type": "nth-weekday",
    "nth": "Last",
    "day": "friday"
  },
  {
    "type": "nth-weekday-after",
    "nth": 2,
    "day": "friday",
    "calendar": {
      "type": "named",
      "key": "Christmas"
    }
  },
  {
    "type": "list",
    "dates": [
      "2023-10-11",
      "2024-10-15",
      "2025-10-09",
      "2026-10-14"
    ]
  },
  {
    "type": "named",
    "key": "EasterSunday"
  },
  {
    "type": "public-holiday",
    "region": "ES-GA"
  },
  {
    "type": "closed-day",
    "region": "PT"
  },
  {
    "type": "conditional",
    "calendar":     {
      "type": "weekly",
      "days": [ "monday", "friday"]
    },
    "condition": "Except when Friday is the last day of the month.@en"
  }
]
```

* Weekly calendars occur on the listed days of the week.
 The days of the week are in English and all lower-case.
 In the example, the venue is open only Monday and Friday.
* Monthly calendars occur on specific days of the month.
  In the example, the venue is open only on the 8th and 15th of the month.
* Yearly calendars occur in specific months of the year.
  The months are month numbers, counting from 0 for January.
  In the sample, the venue is open only in June, July and August.
* Day of year calendars give dates that repeast every year, with days given in month-day format.
  In the example, the calendar specifies the 21st of April and the 23rd of October every year.
* Range calendars specify a range from the start of one calendar to the end of another, relative to the start point.
  In the example, the range is from the 1st of January each year until the following (or on) Friday.
* Union calendars are a collection of other calendars. Any matching calendar indicates a date in the calendar.
  In the example, both Christmas and Boxing Day match.
* Intersection calendars are a collection of other calendars. All the component calendars must match a date for it to be in the intersection.
  In the example, the intersection is the 1st, 8th and 15th of the month when they fall on a Monday or Wednesday. 
* An except calendar inverts the calendar. Any data not in the component calendar is a match.
  In the example, January, February and March are excluded but any other day is in the calnedar.
* The Nth-day-after calendar fits a data to a number of days after (or before, if the number is negative) a reference calendar.
  In the example, the calendar days 4 days after the 1st, 14th or 21st of the month.
* The Nth-weekday specifies dates such as "second Tuesday in the month". 
  The nth value can be `First`, `Second`, `Third`, `Fouth` `Fifth` or `Last`, with the fifth being the same as the fourth if there is no fifth.
  In the example, an even falls on the last Friday of every month.
* The Nth-weekday-after calndar specifies the nth day of week after a reference calendar.
  In the example, the calendar specifies the second Friday after Christmas.
  If the value is negative then it means the nth weekday before the reference calendar.
* A list calendar just gives up trying to specify a rule and just gives a list of dates in ISO-8601 format.
  In the example, different days in October are specified for a number of years.
* A named calendar has an external specification, usually in the configuration file.
  The calendar has a key that can be used to look up the calendar definition.
  These calndars can be configured to have locale-specific names (eg. Christmas or Navidad) and use a key, rather than a name.
  The example refers to Easter Sunday, which moves about the calendar and is usually configured as a list.
* A public holiday calendar refers to all the public holidays in a region, given by a region identifier.
  The example refers to all public holidays take in Galicia, including Spanish national holidays.
* A closed day calendar refers to the standard days where the shops are likely to be closed in a region, usually some
  part of the weekend.
  The region is given by a region identifier.
  The example refers to all closed days in Portugal, where shops usually close on Sunday.
* A conditional calendar takes another calendar and adds a text condition.

**Times** give the hour of the day that something happens, is open, etc.
Times are represented by a string containing pairs of start and finish times.
The times are local times using the 24-hour clock.
Start and finish times are separated by a `-` and ranges by a `,`.
For example: `"0830-1200,1400-1930"` means 8:30am to 12 noon and then
2pm to 7:30pm.

The special text `"open"` means open all hours.
The special text `"closed"` means that the location is closed.
This can be used, in conjunction with hours (below) to indicate exceptions.

**Hours** give the opening hours or event times for something and usually
represent a paired calendar and set of times.
If multiple hours are given, then the first one that matches the calendar
applies.
For example:

```json
"hours": [
    {
      "calendar": {
        "type": "named",
        "key": "Christmas"
      },
      "hours": "closed"
    },
    {
      "calendar": {
        "type": "weekly",
        "days": [ "monday", "tuesday", "wednesday", "thursday" ]
      },
      "hours": "1000-1430,1630-200"
    }
]
```

means something that is open on weekdays from 10am to 2:30pm and then
4:30pm to 8pm, except on Christmas Day, when it is closed.

## Header

The header contains descriptive information about the camino

* `id` The camino identifier. See [below](#identifiers-and-references)
* `name` A short name for the camino. **TBD** Localised variants
* `description` [Descriptive](#description) information about the camino. 
* `metadata` See [above](#metadata)

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

## Imports

Imports allow you to import other camino definitions.
This can be useful when you have a common piece of route or a location
shared by several caminos, since it allows you to just update one piece of
data when doing maintenance.

For example:

```json
"imports": [ "I-MS"]
```

will include the segment from Melide to Santiago de Compostella.

Imports are transative, so the Melide to Santiago import also has an import,
importing the common definition of Santiago de Compostella.

Since you will not want to have the imports included in the list of complete caminos,
Adding `fragment: true` to the camino definition indicates a fragmentary definition intended
for import.

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
  "region": "PT",
  "services": ["BicycleRepair", "Restaurant", "Groceries", "Pharmacy", "Medical", "Bank", "Train", "Bus" ],
  "accommodation": [
    {
      "name": "Azambuja Pilgrims Hostel",
      "type": "PilgrimAlbergue",
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
* `type` The type of location. See [below](#types-services-etc)
* `position` The position of the location in decimal latitude and longitude.
* `region` The region code for the location.
* `services` A list of the publicly available services at the location. See [below](#types-services-etc).
* `accommodation` A list of the accommodation options. See [below](#accommodation)
* `pois` A list of points of interest. See [below](#points-of-interest)
* `events` A list of local events. See [below](#events)
* `camping` Set to true/false if you do not want to use the default camping permissions.
  By default, towns and cities do not allow camping, whereas other locations do.
* `always-open` If set to true, then this location has services (eg. a supermarket) that are
  always available, even on normal days of rest like Sundays.
  This is usually only true for the largest cities.

When you are constructing a location, the position can be difficult.
Generally a latitude/longitude accurate to 5 decimal places identifies the position to
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
  These can overlap with public services.
* `sleeping` lists the types of sleeping arrangements available. See [above](#types-services-etc)
* `multi-day` If true, allows multi-day stays.
  By default, accomodation allows multi-day stays, except for pilgrim albergues,
  where you are expected to move on the next day.

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

### Points of Interest

Points of interest (PoIs) are locations not directly connected with travelling the Camino
but which might be of interest to pilgrims on the way:
churches, cathedrals, statues, museums, parks etc.
Generally, PoIs have the same types as locations and descriptive information.
They may also have [calendar and times](#calendar-and-times) information 
and attached [events]().

And example PoI is

```json
{
  "id": "P-P1-1",
  "name": [
    "Museu Nacional do Azulejo@pt",
    "National Tile Museum@en"
  ],
  "type": "Museum",
  "position": { "latitude": 38.72508, "longitude": -9.11347 },
  "hours": [
    {
      "calendar": {
        "type": "weekly",
        "days": [ "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday" ]
      },
      "hours": "1000-1800"
    }
  ]
}
```
The fields are

* `id` A unique identifier for the PoI.
  This is usually an extension of the location the PoI is associated with.
* `name` The name of the PoI, possibly [localised](#localisation)
* `description` Any optional [descriptive](#description) information
* `type` The PoI (location) type
* `categories` The broad PoI categories.
  If not specfied, then a default set will be used.
  For example, a cathedral defaults to both religious and historical.
* `position` The latitude and longitude of the PoI
* `hours` An optional set of [opening hours](#calendar-and-times) showing when the PoI is open
* `time` An optional estimate of how long (in hours) a visitor will spend at the PoI.
  This is used by the planner to factor in visiting time during a days walk.
  If not specified, then the PoI is assumed to be one of passing interest that does not 
  need time to be included in planning.
* `events` An optional list of [events](#events) associated with the PoI

### Events

Events are things like festivals or ceremonies that might interest a pilgrim, if they happen
to be there at the right time.
Events can be attached to both locations, for things like a town festival, or 
points of interest, for things like pilgrim masses and blessings.
An example event is 

```json
{
  "name": "Pilgrim's Mass",
  "type": "PilgrimMass",
  "hours": "0730-0830,0930-1030,1200-1300,1930-2030"
}
```

The fields are

* `name` The name of the event, possibly [localised](#localisation)
* `description` Any optional [descriptive](#description) information
* `type` The event type
* `hours` Optional [hours](#calendar-and-times) showing when the event occurs

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
For example, a leg by ferry, rather than walking, might be:

```json
{
  "type": "FerryLink",
  "from": "P-E22",
  "to": "P-P258",
  "time": 2.5,
  "penance": 2.0,
  "distance": 0,
  "ascent": 0,
  "descent": 0,
  "description": "Pilgrim's ferry"
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
* `description` Allow you to add additional description to the leg.

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
* `suggested-pois` A list of PoIs (by `id`) that are probably good places to visit.
  Only suggested PoIs with a significan `time` need to be listed.
* `palette` The colour denoting the route.
  The `text-colour` field can be added to provide a more contrasting text colour, if needed.

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
* `description` An optional (plain text) description of what this is intended to capture.
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

