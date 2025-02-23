package calculator

import com.raquo.laminar.api.L.*
import be.doeraene.webcomponents.ui5.{ComboBox, TabContainer}

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
    def recipeComboItems(cb: ComboBox.type) = recipes.map(r =>
        cb.item(_.text := r.name)
    )

    val ingredientQualityVar = Var(initial = 1)
    val targetQualityVar = Var(initial = 5)
    val unlockedQualityVar = Var(initial = 5)
    val inputCountVar = Var(initial = 1)
    val outputCountVar = Var(initial = 1)
    val productivityStrVar = Var(initial = "0.0")
    val machineQualityStrVar = Var(initial = "10.0")
    val recyclerQualityStrVar = Var(initial = "10.0")
    val recipeCraftingTimeSecStrVar = Var(initial = "1.0")
    val machineSpeedStrVar = Var(initial = "1.25")
    val recyclerSpeedStrVar = Var(initial = "0.5")

    val targetQualSelectionObserver = Observer[Int](newValue => {
        targetQualityVar.set(newValue)
        if (unlockedQualityVar.now() < newValue)
            unlockedQualityVar.set(newValue)
    })
    val ingredientQualSelectionObserver = Observer[Int](newValue => {
        ingredientQualityVar.set(newValue)
        if (targetQualityVar.now() < newValue)
            targetQualSelectionObserver.onNext(newValue)
    })

    val qualitiesSignal = Signal.combine(ingredientQualityVar.signal, unlockedQualityVar.signal, targetQualityVar.signal)
    val outputStrSignal = Signal.combine(qualitiesSignal, inputCountVar.signal, outputCountVar.signal, productivityStrVar.signal, machineQualityStrVar.signal,
        recyclerQualityStrVar.signal, recipeCraftingTimeSecStrVar.signal, machineSpeedStrVar.signal, recyclerSpeedStrVar.signal).map:
        case ((ingredientQuality, unlockedQuality, targetQuality), inputCount, outputCount, productivityStr, machineQualityStr, recyclerQualityStr,
              recipeCraftingTimeSecStr, machineSpeedStr, recyclerSpeedStr) =>
            (for {
                prod <- productivityStr.toDoubleOption.map(_ / 100)
                machineQual <- machineQualityStr.toDoubleOption.map(_ / 100)
                recyclerQual <- recyclerQualityStr.toDoubleOption.map(_ / 100)
                craftingTime <- recipeCraftingTimeSecStr.toDoubleOption
                spd <- machineSpeedStr.toDoubleOption
                recSpd <- recyclerSpeedStr.toDoubleOption
            } yield (prod, machineQual, recyclerQual, craftingTime, spd, recSpd)) match
                case Some((productivity, machineQuality, recyclerQuality, recipeCraftingTimeSec, machineSpeed, recyclerSpeed)) =>
                    import Calculator._
                    val calc = Calculator(unlockedQuality)
                    val recipe = Calculator.Recipe("Tungsten Carbide", "Speed Module 3", inputCount, outputCount)
                    val ProductionRes(results, prodMachines, recMachines) = calc.calcSpeeds(
                        recipe, machineQuality, recyclerQuality, productivity, ingredientQuality, targetQuality, recipeCraftingTimeSec, machineSpeed, recyclerSpeed)
                    val requiredIngredients = 1 / results(targetQuality)
                    val Seq(resStr, prodStr) = Seq(results, prodMachines).map(machines => {
                        (1 to 5).toList.filter(qual => machines(qual) > 0.0).map(qual => {
                            f"${formatNumber(machines(qual) * requiredIngredients)} ${qualityToStr(qual)}"
                        }).mkString(" | ")
                    })
                    val costRes = s"${formatNumber(requiredIngredients)} ${qualityToStr(ingredientQuality)} inputs needed to make: " + resStr
                    val recStr = formatNumber(recMachines.values.sum * requiredIngredients)
                    (costRes, "Machines: " + prodStr, "Recyclers: " + recStr)
                case None => ("Error parsing - productivity, quality, and machine speed must be decimal numbers.", "", "")
    def onRecipeSelection(name: String) =
        val recipe = recipeNameToRecipe(name)
        inputCountVar.set(recipe.in.values.head) // TODO: handle multiple inputs
        outputCountVar.set(recipe.out.values.head) // TODO: handle multiple outputs
        recipeCraftingTimeSecStrVar.set(recipe.time.toString)

    def render(): HtmlElement =
        div(
            h2("Description"),
            p(
                "This tool calculates how many ingredients you need in order to create a product of a desired quality.", br(),
                "It assumes that every product below the specified quality is recycled, and the resulting ingredients are fed back as inputs.", br(),
                "Version ", BuildInfo.version, " ", a("Github repo", href("https://github.com/VelizarHristov/Factorio-quality-calculator")) 
            ),
            h2("Calculator"),
            TabContainer(
                _.tab(
                    _.text := "Recipe selection",
                    ComboBox(
                        _.placeholder := "Select recipe",
                        onChange.mapToValue --> Observer(onRecipeSelection),
                        recipeComboItems
                    )
                ),
                _.tab(
                    _.text := "Manual",
                    div(
                        p(
                            "If the recipe has multiple different types of inputs then their required counts are always proportional with each other.", br(),
                            "For example, an offshore pump requires 2 gears and 3 pipes. If you enter '2' for input count, then you will get the number of desired gears.", br(),
                            "If the calculator tells you that you need 200 gears, then it also means that you need 300 pipes of the same quality, as their ratio is always 2:3."
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
                            label("Recipe crafting time (seconds): "),
                            input(
                                size(5),
                                controlled(
                                    value <-- recipeCraftingTimeSecStrVar.signal.map(_.toString),
                                    onInput.mapToValue --> recipeCraftingTimeSecStrVar
                                )
                            )
                        ),
                    )
                )
            ),
            // TODO: make the buttons smaller
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
                    "Target quality: "
                ),
                (1 to 4).toList.map: i =>
                    div(hidden <-- ingredientQualityVar.signal.map(i >= _)),
                qualities.map: (num, str) =>
                    img(
                        src := s"/Quality_${str.toLowerCase}.png",
                        cls("quality"),
                        cls("selected-quality") <-- targetQualityVar.signal.map(_ == num),
                        hidden <-- ingredientQualityVar.signal.map(num < _),
                        onClick.map(_ => num) --> targetQualSelectionObserver
                    ),
                span(
                    "Unlocked quality: "
                ),
                (1 to 4).toList.map: i =>
                    div(hidden <-- targetQualityVar.signal.map(i >= _)),
                qualities.map: (num, str) =>
                    img(
                        src := s"/Quality_${str.toLowerCase}.png",
                        cls("quality"),
                        cls("selected-quality") <-- unlockedQualityVar.signal.map(_ == num),
                        hidden <-- targetQualityVar.signal.map(num < _),
                        onClick.map(_ => num) --> unlockedQualityVar
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
            p(text <-- outputStrSignal.map(_._1)),
            h4("To produce 1 per minute"),
            p(text <-- outputStrSignal.map(_._2)),
            p(text <-- outputStrSignal.map(_._3))
        )
