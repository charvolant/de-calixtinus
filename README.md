# camino-planner
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