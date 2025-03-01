package calculator.factorio_data

case class Recipe(val id: String,
                  val name: String,
                  val time: Double,
                  val producers: Array[Machine],
                  val in: Map[Item, Int],
                  val out: Map[Item, Double],
                  val disallowedEffects: Array[String],
                  val catalyst: Map[String, Int]):
    // number from 0 to 1, 0.0 means not a catalyst, 0.5 means half of the output is catalyst
    def getCatalystDegree(item: String): Double =
        val totalOutput = out.find(_._1.id == item).get._2
        catalyst.getOrElse(item, 0) / totalOutput
