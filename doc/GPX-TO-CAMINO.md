# Converting a GPX track into a Camino file

These notes are for people wanting to turn the track from a walk into a de-calixtinus
[camino](CAMINO.md) definition.
The track will be far too detailed for de-calixtinus to use and ill generally be a bit noisy.
This is a guide to breaking the track down into suitable segments.

This is going to get a bit technical.
You will need to be famililar with file formats such as XML and have a decent amount
of spreadsheet knowledge.

## Convert GPX to CSV

GPX tracks are XML files that follow the 
[Topograpfix GPX](http://www.topografix.com/GPX/1/1/gpx.xsd) XML schema.
The `gpx.xslt` [XSLT](https://en.wikipedia.org/wiki/XSLT) script will convert
the GPX track into a CSV file, suitable for importing into a spreadsheet, such as Excel.

## Calculate distances

Distances between the latitude/longitude points from the track need to be computed
using the great-circle distance, the shortest point to point distance on the
surface of the earth.
Since the differences in lat/long between each point will be relatively small,
the best approach is to use the [Haversine Formula](https://en.wikipedia.org/wiki/Haversine_formula).
The following is a VBA function that performs the necessary calculation, taking lat/longs in degrees
and returning a distance in metres:

```
Function LATLONGDISTANCE(lat1 As Double, long1 As Double, lat2 As Double, long2 As Double) As Double
  lat1r = WorksheetFunction.Radians(lat1)
  long1r = WorksheetFunction.Radians(long1)
  lat2r = WorksheetFunction.Radians(lat2)
  long2r = WorksheetFunction.Radians(long2)
  deltalat = lat2r - lat1r
  deltalong = long2r - long1r
  Radius = 6378137
  hav2 = 1 - Cos(deltalat) + Cos(lat1r) * Cos(lat2r) * (1 - Cos(deltalong)) / 2
  hav = Sqr(hav2)
  If hav > 1 Then
    hav = 1
  End If
  If hav < -1 Then
    hav = -1
  End If
  LATLONGDISTANCE = 2 * Radius * WorksheetFunction.Asin(hav)
End Function```

## Calculate Ascent and Descent

GPX tracks tend to wobble about vertical distances, with a relatively flat walk
flipping up or down a metre or so quite often.
This doesn't really reflect any meaningful ascent or descent and needs to be
smoothed over.

An approach which seems to work is to accumulate elevation error until a
threshold is passed and then move the elevation up or down accordingly.
For example, if the threshold is 5m, then any change under five metres will
simply accumulate until the 5m boundary is crossed, at which point the 
elevation accumulates and the accumulated error returns to zero.

For example

|     | A            | B          | C          | D           | E                                                   | F                 | G                    |
|-----|--------------|------------|------------|-------------|-----------------------------------------------------|-------------------|----------------------|
| 1   | latitude	    | longitude	 | elevation	 | distance    | nominal elevation change                            | accumulated error | calculated elevation |
| 2   | -33.89107958 | 151.2761157 | 13         | 0           | 0                                                   | 13                | 
| 3   | -33.89098001 | 151.27643  | 12         | 43.96588767 | 0 `=IF(ABS(C3-C2 + F2) > 5, ROUND(C3-C2+F2, 0), 0)` | -1 `=F2+C3-C2-E3` | 13 `=G2+E3`          | 
| 4   | -33.89082    | 151.27676  | 12         | 49.9428894  | 0                                                   | -1                | 13                   | 
| 5   | -33.89072    | 151.277    | 13         | 35.08995304 | 0                                                   | 0                 | 13                   | 
| 6   | -33.89062    | 151.27727  | 13         | 38.63471821 | 0                                                   | 0                 | 13                   | 
| 7   | -33.89052998 | 151.27759  | 12         | 44.15732201 | 0                                                   | -1                | 13                   | 
| ... | ...          | ...        | ...        | ...         | ...                                                 | ...               | ...                  |
| 12  | -33.89051003 | 151.28216  | 13         | 23.29841518 | 0                                                   | 0                 | 13                   |
| 13  | -33.89057768 | 151.2823145 | 14         | 22.83374174 | 0                                                   | 1                 | 13                   | 
| 14  | -33.89050777 | 151.2823799 | 15         | 13.93242174 | 0                                                   | 2                 | 13                   | 
| 15  | -33.89048003 | 151.282402 | 15         | 5.232666096 | 0                                                   | 2                 | 13                   | 
| 16  | -33.89044658 | 151.2824106 | 15         | 5.38356296  | 0                                                   | 2                 | 13                   | 
| 17  | -33.89038548 | 151.2823778 | 16         | 10.53008008 | 0                                                   | 3                 | 13                   | 
| 18  | -33.89019781 | 151.2826272 | 19         | 43.98687958 | 6                                                   | 0                 | 19                   |

## Following the Track

This uses Google Maps.
You will need to be logged in to Google to use My Maps.
Add the GPX track to [My Maps](https://mymaps.google.com/) by creating a new map and importing the GOX track.
You can then use the tracks to 