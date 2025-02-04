package calculator

import com.raquo.laminar.api.L.{*, given}

import math.BigDecimal.RoundingMode

object MainPage {
  val qualities = List(
    1 -> "Normal",
    2 -> "Uncommon",
    3 -> "Rare",
    4 -> "Epic",
    5 -> "Legendary")
  def qualityToStr = qualities.toMap.apply
  def formatNumber(n: Double) =
    if n >= 1 then
      f"$n%.2f"
    else // show first 3 non-zero digits, e.g. 0.00402695 => 0.00403
      val digitsBeforeFirstNonZero = -Math.log10(n).floor.toInt
      String.format(s"%1.${digitsBeforeFirstNonZero + 2}f", n)

  val ingredientQualityVar = Var(initial = 1)
  val unlockedQualityVar = Var(initial = 5)
  val inputCountVar = Var(initial = 1)
  val outputCountVar = Var(initial = 1)
  val productivityStrVar = Var(initial = "0.0")
  val recipeQualityStrVar = Var(initial = "10.0")
  val recyclerQualityStrVar = Var(initial = "10.0")
  val recipeCraftingTimeSecStrVar = Var(initial = "1.0")
  val machineSpeedStrVar = Var(initial = "1.25")

  val outputStrSignal = Signal.combine(ingredientQualityVar.signal, unlockedQualityVar.signal, inputCountVar.signal, outputCountVar.signal,
    productivityStrVar.signal, recipeQualityStrVar.signal, recyclerQualityStrVar.signal, recipeCraftingTimeSecStrVar.signal, machineSpeedStrVar.signal
  ).map { case (ingredientQuality, unlockedQuality, inputCount, outputCount, productivityStr, recipeQualityStr, recyclerQualityStr, recipeCraftingTimeSecStr, machineSpeedStr) => {
    import Calculator._
    (for {
      prod <- productivityStr.toDoubleOption.map(_ / 100)
      recipeQual <- recipeQualityStr.toDoubleOption.map(_ / 100)
      recyclerQual <- recyclerQualityStr.toDoubleOption.map(_ / 100)
      craftingTime <- recipeCraftingTimeSecStr.toDoubleOption
      spd <- machineSpeedStr.toDoubleOption
    } yield (prod, recipeQual, recyclerQual, craftingTime, spd)) match {
      case Some((productivity, recipeQuality, recyclerQuality, recipeCraftingTimeSec, machineSpeed)) =>
        val calc = Calculator(unlockedQuality)
        val recipe = Recipe("Tungsten Carbide", "Speed Module 3", inputCount, outputCount)
        val ProductionRes(ingredients, prodMachines, recMachines) = calc.calcSpeeds(recipe, recipeQuality, recyclerQuality, productivity, ingredientQuality, recipeCraftingTimeSec, machineSpeed)
        val costRes = s"${formatNumber(ingredients)} ${qualityToStr(ingredientQuality)} inputs needed for 1 ${qualityToStr(unlockedQuality)} output"
        val prodStr = (1 to 5).toList.filter(qual => prodMachines(qual) > 0.0).map(qual => {
            f"${formatNumber(prodMachines(qual) / 60)} ${qualityToStr(qual)}"
          }).mkString(" | ")
        val recStr = formatNumber(recMachines.values.sum / 60)
        (costRes, "Machines: " + prodStr, "Recyclers: " + recStr)
      case None => ("Error parsing - productivity, quality, and machine speed must be decimal numbers.", "", "")
    }
  }}

  def apply(): HtmlElement = {
    div(
      h2("Description"),
      p(
        "This tool calculates how many ingredients you need in order to create a product of a desired quality.", br(),
        "It assumes that every product below the specified quality is recycled, and the resulting ingredients are fed back as inputs.", br(),
        "If the recipe has multiple different types of inputs then their required counts are always proportional with each other.", br(),
        "For example, an offshore pump requires 2 gears and 3 pipes. If you enter '2' for input count, then you will get the number of desired gears.", br(),
        "If the calculator tells you that you need 200 gears, then it also means that you need 300 pipes of the same quality, as their ratio is always 2:3.", br(),
        "Version ", BuildInfo.version, " ", a("Github repo", href("https://github.com/VelizarHristov/Factorio-quality-calculator")) 
      ),
      h2("Calculator"),
      p(
        "Ingredient quality: ",
        select(
          value <-- ingredientQualityVar.signal.map(_.toString),
          onChange.mapToValue.map(_.toInt) --> ingredientQualityVar,
          qualities.map { case (num, str) => option(value := num.toString, str) }
        )
      ),
      p(
        "Unlocked quality: ",
        select(
          value <-- unlockedQualityVar.signal.map(_.toString),
          onChange.mapToValue.map(_.toInt) --> unlockedQualityVar,
          qualities.map { case (num, str) => option(value := num.toString, str) }
        )
      ),
      p(
        label("Input count: "),
        input(
          size(5),
          controlled(
            value <-- inputCountVar.signal.map(_.toString),
            onInput.mapToValue.filter(_.forall(Character.isDigit)).map(_.toInt) --> inputCountVar
          )
        )
      ),
      p(
        label("Output count: "),
        input(
          size(5),
          controlled(
            value <-- outputCountVar.signal.map(_.toString),
            onInput.mapToValue.filter(_.forall(Character.isDigit)).map(_.toInt) --> outputCountVar
          )
        )
      ),
      p(
        label("Productivity (%): "),
        input(
          size(5),
          controlled(
            value <-- productivityStrVar.signal.map(_.toString),
            onInput.mapToValue --> productivityStrVar
          )
        )
      ),
      p(
        label("Recipe quality (%): "),
        input(
          size(5),
          controlled(
            value <-- recipeQualityStrVar.signal.map(_.toString),
            onInput.mapToValue --> recipeQualityStrVar
          )
        )
      ),
      p(
        label("Recycler quality (%): "),
        input(
          size(5),
          controlled(
            value <-- recyclerQualityStrVar.signal.map(_.toString),
            onInput.mapToValue --> recyclerQualityStrVar
          )
        )
      ),
      p(
        label("Recipe crafting time (seconds): "),
        input(
          size(5),
          controlled(
            value <-- recipeCraftingTimeSecStrVar.signal.map(_.toString),
            onInput.mapToValue --> recipeCraftingTimeSecStrVar
          )
        )
      ),
      p(
        label("Machine speed: "),
        input(
          size(5),
          controlled(
            value <-- machineSpeedStrVar.signal.map(_.toString),
            onInput.mapToValue --> machineSpeedStrVar
          )
        )
      ),
      p(text <-- outputStrSignal.map(_._1)),
      h4("To produce 1 per minute"),
      p(text <-- outputStrSignal.map(_._2)),
      p(text <-- outputStrSignal.map(_._3))
    )
  }
}
