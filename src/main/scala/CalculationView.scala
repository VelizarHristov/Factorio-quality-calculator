package calculator

import calculator.factorio_data.Recipe

object CalculationView:
    private def formatNumber(n: Double) =
        if n == 0 then
            0
        else
            val num = 
                if n >= 1 then
                    f"$n%.2f"
                else
                    // show first 3 non-zero digits, e.g. 0.00402695 => 0.00403
                    val digitsBeforeFirstNonZero = -Math.log10(n).floor.toInt
                    String.format(s"%1.${digitsBeforeFirstNonZero + 2}f", n)
            if num.contains('.') then
                // remove trailing zeroes after decimal point
                num.reverse.dropWhile(_ == '0').dropWhile(_ == '.').reverse
            else
                num

    def calcAndDisplay(unlockedQuality: Int,
                       ingredientQuality: Int,
                       targetQuality: Int,
                       recipe: Recipe,
                       machineQuality: Double,
                       recyclerQuality: Double,
                       productivity: Double,
                       machineSpeed: Double,
                       recyclerSpeed: Double): (String, List[String], String, String) =
        import CommonData.qualityToStr
        val calc = Calculator(unlockedQuality)
        val recipeCraftingTimeSec = recipe.time
        // TODO: later on needs to choose based on:
        //   1. should not be liquid
        //   2. should recycle into the inputs
        //   3. arbitrarily if multiple meet this criteria
        val (mainOutput, mainOutputCount) = recipe.out.head
        val prodMultiplier = 1.0 - recipe.getCatalystDegree(mainOutput.id)
        val (results, prodMachines, recMachines, prodCount) = calc.calcSpeeds(
            machineQuality, recyclerQuality, productivity * prodMultiplier,
            ingredientQuality, targetQuality, recipeCraftingTimeSec, machineSpeed, recyclerSpeed)
        val requiredIngredientsMult = 1 / results(targetQuality) / mainOutputCount
        val costStr = recipe.in.toList.map { (item, input) =>
            s"${formatNumber(requiredIngredientsMult * input)} ${qualityToStr(ingredientQuality)} ${item.name}"
        }.mkString(" | ")

        def displayQualMap(map: Map[Int, Double], name: String): String =
            name + ": " +
            (1 to 5).toList
                .filter(qual => map(qual) > 0.0)
                .map(qual => f"${formatNumber(map(qual) * requiredIngredientsMult)} ${qualityToStr(qual)}")
                .mkString(" | ")

        val mainOutputStr = displayQualMap(results.view.mapValues(_ * mainOutputCount).toMap, mainOutput.name)
        val byproductOutputsStr = recipe.out.toList
            .filter(_._1.id != mainOutput.id)
            .map: (item, outputCount) =>
                val recipeProd = 1 + productivity * (1.0 - recipe.getCatalystDegree(item.id))
                displayQualMap(prodCount.view.mapValues(_ * outputCount * recipeProd).toMap, item.name)
        val outputsStrings = mainOutputStr :: byproductOutputsStr
        val prodStr = displayQualMap(prodMachines, "Machines")
        val recStr = formatNumber(recMachines.values.sum * requiredIngredientsMult)
        (costStr, outputsStrings, prodStr, "Recyclers: " + recStr)
