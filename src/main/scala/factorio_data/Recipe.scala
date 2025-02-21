package calculator.factorio_data

class Recipe(val id: String,
             val name: String,
             val time: Double,
             val producers: Array[Machine],
             val in: Map[Item, Int],
             val out: Map[Item, Int],
             disallowedEffects: Array[String],
             catalyst: Map[Item, Int])
