# Configuration

Configuration is via YAML files.
Most elements have defaults and the configuration only needs to
override things different to the defaults.
The basic configuration consists of three sections

```yaml
web:
  # Web server configuration, including the location of static assets
caminos:
  # Links to camino definition files
calendar:
  # Named calendar definitions
regions:
  # Region definitions and hierarchy
```

## Web Configuration

```yaml
web:
  assets:
    - id: bootstrap-css
      type: Css
    - id: icons
      type: Directory
      path: https://de-calixtinus.s3.ap-southeast-2.amazonaws.com/static/icons
    - id: "Camino Icons"
      type: Font
      path: ../fonts/Camino-Icons.woff
  links:
    - id: helpLink
      type: Header
      links:
        - locale: ""
          label: Help
          path: "help-en.html"
  maps:
    - id: googleMaps,
      tiles: http://mt0.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}
```

The web configuration contains paths and extra configuration
for web pages, so that assets can be re-located to static servers,
such as S3 buckets, and the locations of additional links and map
tiles.

### Assets

Assets are references to (usually static) loadable information and can have the following elements

| Field | Description | Example |
| --- | --- | --- |
| id | The identifier of the asset. The system looks up assets based on identifier and configuration files use the identifier to override the default. | `jquery-js` |
| type | The asset type. See below for types | `JavaScriptEarly` |
| path | An absolute or relative path to the asset | `../icons` |
| integrity | An optional asset integrrity checksum | |
| crossorigin | An optional cross-origin access type. One of `Unused`, `Anonymous` or `UseCredentials` | `Unused` |

The asset type can be one of

| Type               | Description                                                                                              |
|--------------------|----------------------------------------------------------------------------------------------------------|
| `JavaScript`       | A javascript asset, loaded after the page has loaded.                                                    |
| `JavaScriptEarly`  | A javascript asset, loaded while loading the header so that the script can be used in-page.              |
| `Css`              | A CSS style-sheet                                                                                        |
| `Font`             | The location of a font used by a CSS style-sheet. The asset id corresponds to the `font-family` CSS name. |
| `Icon`             | A specific icon, such as a page header. Rarely used.  |
| `CaminoDefinition` | A [camino definition file](CAMINO.md) |
| `Directory`        | A directory holding a group of assests, such as PNG icons. |

Links are links that can be inserted into the header or footer of the page layout,
to allow a certain level of customisation.
The link elements are:

| Field | Description | Example |
| --- | --- | --- |
| id | The link identifier, to allow location and over-riding of links | `organisation` |
| type | The link type, giving the location of the link, one of `Header`, `Footer` or `Embedded` | `Header` |
| links | A list of localised URLs with localised titles. See [localisation](CAMINO.md#localisation) for more information | |

Localised links can be associated with a specific locale, a locale-specific link and a locale-specific title.

Maps allow the user to specify alternative map sources,
such as Google Maps or Open Street Map (the default)
The map elements are:

| Field | Description                                                                                                  | Example                                               |
|-------|--------------------------------------------------------------------------------------------------------------|-------------------------------------------------------|
| id    | The map identifier, to allow location and over-riding of links                                               | `google`                                              |
| tiles | The URL of the tile server for the map, with `{x}`, `{y}` and `{z}` giving the longitude, latitude and zoom. | `http://mt0.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}` |

## Caminos

Caminos are specified by [asset links](#assets) to [camino definition files](CAMINO.ms).

For example:

```yaml
caminos:
  - id: I-ST
    type: CaminoDefinition
    path: "https://test.com/import-santiago.json"
  - id: I-MS
    type: CaminoDefinition
    path: "https://test.com/import-melide-santiago.json"
  - id: F
    type: CaminoDefinition
    path: "file:camino-frances.json"
  - id: P
    type: CaminoDefinition
    path: "file:/data/caminos/camino-portuguese.json"
```

Only the `file`, `http` and `https` schemes are supported.
Caminos appear in the camino menu in load order.
It is also generally a good idea to specify any imported
camino fragments before the main camino definition files.

## Calendars

Calendars are well-known calendar events that can be easily named,
with everyone knowing what you are talking about.
Christmas, Easter Sunday and, er, Mainday Thursday.

The calendar definitions give a calendar key (unique identifier),
a set of [localisable](CAMINO.md#localisation) names for the event
and a [calendar definition](CAMINO.md#calendar-and-times).
For example,

```yaml
calendar:
  - key: LabourDay
    name:
      - Labour Day@en
      - Día del Trabajador@es
      - Dia do Trabalhador@pt
    calendar:
      type: day-of-year
      days:
        - 05-01
  - key: MaundayThursday
    name:
      - Maundy Thursday@en
      - Jueves Santo@es
    calendar:
      type: nth-day-after
      nth: -3
      calendar:
        type: named
        key: EasterSunday

```

This defines Labour Day (or Dia do Trabalhador if you are Portuguese) as being on the 1st of May
and Maunday Thursday as being three days before Easter Sunday 
(you can reference other calendar definitions).
These named calendars can be used during region definition and
point of interest and event definitions.

## Regions

Regions are geographical regions -- continents, countries, islands, etc.
They are used to provide information about where you are: what sort of
language is spoken, local holidays, etc.
These regions can be used to identify when shops are likely to be shut
and the like.

Regions form a hierarchy.
For example, Galicia is an autonomous community of Spain, which is in Europe, which is part of the World.
Or Faure Island is part of Shark Bay, in the Indian Ocean in the World.
Faure Island is also part of Western Australia, which is also part of Australia,
so there is a system for secondary membership of other regions.

An example pair of regions is 

```yaml
  - id: "ES"
    name:
      - España@es
      - Spain@en
      - Espagne@fr
      - España@ga
      - Espanha@pt
    type: Country
    parent: Europe
    locale: es
    holidays:
      - type: named
        key: NewYear
      - type: named
        key: Epiphany
      # ...
    closed-days:
      - type: weekly
        days:
          - sunday
  - id: "ES-GA"
    name:
      - Galicia@ga
      - Galicia@es
    type: Province
    parent: ES
    member: 
      - GNPE
    locale: ga
    holidays:
      - type: named
        key: GalicianLiteratureDay
      - type: named
        key: GalicianNationalDay

```

The fields are

| Field       | Description                                                                                                                       | Example     |
|-------------|-----------------------------------------------------------------------------------------------------------------------------------|-------------|
| id          | The region identifier, used so that the region can be referenced.                                                                 | `Europe`    |
| name        | A localisable name for the region                                                                                                 | `Europa@es` |
| type        | The type of region, see below                                                                                                     | `Sea`       |
| parent      | An optional parent region, given by id                                                                                            | `World`     |
| member      | A list of other, secondary, regional memberships                                                                                  | `EU`        |
| locale      | The locale, see [localisation](CONFIG.md#supported-languages)                                                                     | `pt`        |
| holidays    | A list of observed holidays. These are usually [configured calendars](#calendars)                                                 | `Easter`    |
| closed-days | A list of days where shops and services are usually closed. These are usually weekly calendar definitions. | `sunday`    |

For identifiers, using the [ISO-3166-1](https://en.wikipedia.org/wiki/ISO_3166-1) two letter codes for countries and
[ISO-3166-2](https://en.wikipedia.org/wiki/ISO_3166-2) codes for regions is a good idea.
If you want something more complex, try the
[Getty Thesaurus od Geographic Names](https://www.getty.edu/research/tools/vocabularies/tgn/).

The hierarchy inherits or accumulates values.
For example, the locales for Galicia would be `ga`, `es` and `*` in that order.
Similarly, Galicia observes both Galician Literture Day and Epiphany as
public holidays.

The possible region types are:

| Type | Description |
| --- | --- |
| `Planet` | The entire planet, a top-level region |
| `Continent` | A continental land mass, such as Asia |
| `Ocean` | A large ocean |
| `Terrestrial` | A large terrestrial region, such as Scandinavia |
| `Marine` | A large marine region, such as North-West Shelf |
| `Country` | A country, such as France |
| `Sea` | A sea or smaller marine region, such as the Red Sea |
| `Province` | A sub-national unit such as a province, state or autonomous community |
| `Island` | An island or group of islands, such as the Chatham Islands |
| `OtherRegion` | Anything that does not fit the above types |

Region types have a rough conceptual ordering, but it is not enforced,
since there are too many possible exceptions.
For example, the Galicia-North Portugal Euroregion is a transnational cooperation zone
straddling parts of two countrues.

