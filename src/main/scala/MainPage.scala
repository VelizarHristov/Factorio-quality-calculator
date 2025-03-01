package calculator

import com.raquo.laminar.api.L.*
import be.doeraene.webcomponents.ui5.ComboBox

import factorio_data.{Item, Recipe}

class MainPage(items: Vector[Item], recipes: Vector[Recipe]):
    val qualities = List(
        1 -> "Normal",
        2 -> "Uncommon",
        3 -> "Rare",
        4 -> "Epic",
        5 -> "Legendary")
    val recipeNameToRecipe = recipes.map(i => i.name -> i).toMap
    def qualityToStr = qualities.toMap.apply
    def formatNumber(n: Double) =
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
    def recipeComboItems(cb: ComboBox.type) = recipes.filter(!_.disallowedEffects.contains("quality")).map(r =>
        cb.item(_.text := r.name)
    )

    val ingredientQualityVar = Var(initial = 1)
    val targetQualityVar = Var(initial = 5)
    val unlockedQualityVar = Var(initial = 5)
    val selectedRecipeVar = Var(initial = recipeNameToRecipe("Quality module 3"))
    val productivityStrVar = Var(initial = "0.0")
    val machineQualityStrVar = Var(initial = "10.0")
    val recyclerQualityStrVar = Var(initial = "10.0")
    val machineSpeedStrVar = Var(initial = "1.25")
    val recyclerSpeedStrVar = Var(initial = "0.5")

    val unlockedQualitySelectionObserver = Observer[Int](newValue => {
        if (targetQualityVar.now() > newValue)
            targetQualityVar.set(newValue)
        unlockedQualityVar.set(newValue)
    })
    val ingredientQualSelectionObserver = Observer[Int](newValue => {
        if (unlockedQualityVar.now() < newValue)
            unlockedQualitySelectionObserver.onNext(newValue)
        if (targetQualityVar.now() < newValue)
            targetQualityVar.set(newValue)
        ingredientQualityVar.set(newValue)
    })

    val qualitiesSignal = Signal.combine(ingredientQualityVar.signal, unlockedQualityVar.signal, targetQualityVar.signal)
    val outputStrSignal = Signal.combine(qualitiesSignal, selectedRecipeVar.signal, productivityStrVar.signal, machineQualityStrVar.signal,
        recyclerQualityStrVar.signal, machineSpeedStrVar.signal, recyclerSpeedStrVar.signal).map:
        case ((ingredientQuality, unlockedQuality, targetQuality), selectedRecipe, productivityStr, machineQualityStr, recyclerQualityStr,
              machineSpeedStr, recyclerSpeedStr) =>
            (for {
                prod <- productivityStr.toDoubleOption.map(_ / 100)
                machineQual <- machineQualityStr.toDoubleOption.map(_ / 100)
                recyclerQual <- recyclerQualityStr.toDoubleOption.map(_ / 100)
                spd <- machineSpeedStr.toDoubleOption
                recSpd <- recyclerSpeedStr.toDoubleOption
            } yield (prod, machineQual, recyclerQual, spd, recSpd)) match
                case Some((productivity, machineQuality, recyclerQuality, machineSpeed, recyclerSpeed)) =>
                    import Calculator._
                    val calc = Calculator(unlockedQuality)
                    val recipeCraftingTimeSec = selectedRecipe.time
                    val ProductionRes(results, prodMachines, recMachines) = calc.calcSpeeds(
                        machineQuality, recyclerQuality, productivity,
                        ingredientQuality, targetQuality, recipeCraftingTimeSec, machineSpeed, recyclerSpeed)
                    val mainOutputCount = selectedRecipe.out.head._2
                    val requiredIngredientsMult = 1 / results(targetQuality) / mainOutputCount
                    val costStr = selectedRecipe.in.toList.map { (item, input) =>
                        s"${formatNumber(requiredIngredientsMult * input)} ${qualityToStr(ingredientQuality)} ${item.name}"
                    }.mkString(" | ")
                    def displayQualMap(map: Map[Int, Double], name: String): String =
                        name + ": " +
                        (1 to 5).toList
                            .filter(qual => map(qual) > 0.0)
                            .map(qual => f"${formatNumber(map(qual) * requiredIngredientsMult)} ${qualityToStr(qual)}")
                            .mkString(" | ")
                    val outputsStrings = selectedRecipe.out.toList.map: (item, outputCount) =>
                        displayQualMap(results.view.mapValues(_ * outputCount).toMap, item.name)
                    val prodStr = displayQualMap(prodMachines, "Machines")
                    val recStr = formatNumber(recMachines.values.sum * requiredIngredientsMult)
                    (costStr, outputsStrings, prodStr, "Recyclers: " + recStr)
                case None => ("Error parsing - productivity, quality, and machine speed must be decimal numbers.", List(), "", "")

    def render(): HtmlElement =
        div(
            h2("Description"),
            p(
                "This tool calculates how many ingredients you need in order to create a product of a desired quality.", br(),
                "It assumes that every product below the specified quality is recycled, and the resulting ingredients are fed back as inputs.", br(),
                "Version ", BuildInfo.version, " ", a("Github repo", href("https://github.com/VelizarHristov/Factorio-quality-calculator")) 
            ),
            h2("Calculator"),
            ComboBox(
                _.placeholder := "Select recipe",
                value <-- selectedRecipeVar.signal.map(_.name),
                onChange.mapToValue.map(recipeNameToRecipe) --> selectedRecipeVar,
                recipeComboItems
            ),
            div(
                cls := "grid-container",
                span(
                    "Ingredient quality: "
                ),
                qualities.map: (num, str) =>
                    img(
                        src := s"/Quality_${str.toLowerCase}.png",
                        cls("quality"),
                        cls("selected-quality") <-- ingredientQualityVar.signal.map(_ == num),
                        onClick.map(_ => num) --> ingredientQualSelectionObserver
                    ),
                span(
                    "Unlocked quality: "
                ),
                (1 to 4).toList.map: i =>
                    div(hidden <-- ingredientQualityVar.signal.map(i >= _)),
                qualities.map: (num, str) =>
                    img(
                        src := s"/Quality_${str.toLowerCase}.png",
                        cls("quality"),
                        cls("selected-quality") <-- unlockedQualityVar.signal.map(_ == num),
                        hidden <-- ingredientQualityVar.signal.map(num < _),
                        onClick.map(_ => num) --> unlockedQualitySelectionObserver
                    ),
                span(
                    "Target quality: "
                ),
                (1 to 4).toList.map: i =>
                    div(hidden <-- ingredientQualityVar.signal.map(i >= _)),
                qualities.map: (num, str) =>
                    img(
                        src := s"/Quality_${str.toLowerCase}.png",
                        cls("quality"),
                        cls("selected-quality") <-- targetQualityVar.signal.map(_ == num),
                        hidden <-- qualitiesSignal.signal.map { case (ingr, unl, _) => ingr > num || num > unl },
                        onClick.map(_ => num) --> targetQualityVar
                    ),
                (2 to 5).toList.map: i =>
                    div(hidden <-- unlockedQualityVar.signal.map(i < _)),
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
                label("Machine quality (%): "),
                input(
                    size(5),
                    controlled(
                        value <-- machineQualityStrVar.signal.map(_.toString),
                        onInput.mapToValue --> machineQualityStrVar
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
                label("Recycler speed: "),
                input(
                    size(5),
                    controlled(
                        value <-- recyclerSpeedStrVar.signal.map(_.toString),
                        onInput.mapToValue --> recyclerSpeedStrVar
                    )
                )
            ),
            h4("Ingredients:"),
            p(text <-- outputStrSignal.map(_._1)),
            h4("Outputs:"),
            children <-- outputStrSignal.map(_._2.map(div(_))),
            h4("To produce 1 per minute"),
            p(text <-- outputStrSignal.map(_._3)),
            p(text <-- outputStrSignal.map(_._4))
        )
