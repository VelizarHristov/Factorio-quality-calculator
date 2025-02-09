package calculator.factorio_data

class Recipe(val id: String,
             val name: String,
             time: Double,
             producers: Array[Machine],
             in: Map[Item, Int],
             out: Map[Item, Int],
             disallowedEffects: Array[String],
             catalyst: Map[Item, Int])
