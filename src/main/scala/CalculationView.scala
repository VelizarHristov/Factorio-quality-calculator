package calculator

import calculator.factorio_data.Recipe

object CalculationView:
    def formatNumber(n: Double): String =
        if n == 0 then
            "0"
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

    type CalcRes = (List[(String, Int, Double)], List[(String, List[(Int, Double)])], (String, List[(Int, Double)]), Double)
    def calcForDisplay(unlockedQuality: Int,
                       ingredientQuality: Int,
                       targetQuality: Int,
                       recipe: Recipe,
                       machineQuality: Double,
                       recyclerQuality: Double,
                       productivity: Double,
                       machineSpeed: Double,
                       recyclerSpeed: Double): CalcRes =
        import Calculator.ProductionRes
        val calc = Calculator(unlockedQuality)
        val recipeCraftingTimeSec = recipe.time
        // TODO: later on needs to choose based on:
        //   1. should not be liquid
        //   2. should recycle into the inputs
        //   3. arbitrarily if multiple meet 1 and none meets 2
        val (mainOutput, mainOutputCount) = recipe.out.head
         // TODO: should show no machines, only recyclers
        val ProductionRes(results, prodCount, recCount) = if recipe.recyclesIntoItself then
            calc.calcSpeedsUnit(recyclerQuality, 0, 0, ingredientQuality, targetQuality)
        else
            val prodMultiplier = 1.0 - recipe.getCatalystDegree(mainOutput.id)
            calc.calcSpeedsUnit(machineQuality, recyclerQuality, productivity * prodMultiplier, ingredientQuality, targetQuality)
        val craftingTimeSec = recipeCraftingTimeSec / machineSpeed
        val recyclingRecipeSpeed = if (recipe.isRecycler) recipeCraftingTimeSec else recipeCraftingTimeSec / 16
        val recyclingTimeSec = recyclingRecipeSpeed / recyclerSpeed
        val requiredIngredientsMult = 1 / results(targetQuality) / mainOutputCount

        val inputs = recipe.in.toList.map: (item, input) =>
            (item.name, ingredientQuality, requiredIngredientsMult * input)
        def makeQualMap(map: Map[Int, Double], name: String): (String, List[(Int, Double)]) =
            name -> (1 to 5).toList
                .filter(qual => map(qual) > 0.0)
                .map(qual => qual -> map(qual) * requiredIngredientsMult)
        val mainOutputThing = makeQualMap(results.view.mapValues(_ * mainOutputCount).toMap, mainOutput.name)
        val byproductOutputsThing = recipe.out.toList
            .filter(_._1.id != mainOutput.id)
            .map: (item, outputCount) =>
                val recipeProd = 1 + productivity * (1.0 - recipe.getCatalystDegree(item.id))
                makeQualMap(prodCount.view.mapValues(_ * outputCount * recipeProd).toMap, item.name)
        val outputs = mainOutputThing :: byproductOutputsThing
        val prodMachines = prodCount.view.mapValues(_ * craftingTimeSec / 60).toMap
        val recMachines = recCount.view.mapValues(_ * recyclingTimeSec / 60).toMap
        val machineUsage = makeQualMap(prodMachines, if recipe.isRecycler then "" else "Machines")
        val recyclerTotalUses = recMachines.values.sum * requiredIngredientsMult

        (inputs, outputs, machineUsage, recyclerTotalUses)

    def displayCalc(res: CalcRes): (String, List[String], String, String) =
        import CommonData.qualityToStr
        val (inputs, outputs, (machineName, machineUsage), recyclerUses) = res
        val inputsStr = inputs.map { (name, quality, count) =>
            s"${formatNumber(count)} ${qualityToStr(quality)} $name"
        }.mkString(" | ")
        val outputsStr = outputs.map: (name, amounts) =>
            name + ": " + amounts.map { (quality, amount) =>
                f"${formatNumber(amount)} ${qualityToStr(quality)}"
            }.mkString(" | ")
        val machineUsageStr =
            if machineName.nonEmpty then
                machineName + ": " + machineUsage.map { (quality, amount) =>
                    f"${formatNumber(amount)} ${qualityToStr(quality)}"
                }.mkString(" | ")
            else
                ""
        val recyclerUsageStr = if (recyclerUses == 0) "" else "Recyclers: " + formatNumber(recyclerUses)

        (inputsStr, outputsStr, machineUsageStr, recyclerUsageStr)        

    def calcAndDisplay(unlockedQuality: Int,
                       ingredientQuality: Int,
                       targetQuality: Int,
                       recipe: Recipe,
                       machineQuality: Double,
                       recyclerQuality: Double,
                       productivity: Double,
                       machineSpeed: Double,
                       recyclerSpeed: Double): (String, List[String], String, String) =
        val res = calcForDisplay(unlockedQuality, ingredientQuality, targetQuality, recipe,
            machineQuality, recyclerQuality, productivity, machineSpeed, recyclerSpeed)
        displayCalc(res)
