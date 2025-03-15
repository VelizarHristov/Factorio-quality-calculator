package calculator_test

import calculator.CalculationView.{calcForDisplay, displayCalc}
import calculator.factorio_data.{Item, Recipe}
import calculator.CalculationView.CalcRes
import calculator.factorio_data.Machine

class CalculationViewSuite extends munit.FunSuite:
    val i1 = Item("superconductor", "Superconductor", false)
    val i2 = Item("speed-module-2", "Speed module 2", false)
    val i3 = Item("advanced-circuit", "Advanced circuit", false)
    val i4 = Item("processing-unit", "Processing unit", false)
    val i5 = Item("speed-module-3", "Speed module 3", false)
    val f1 = Item("water", "Water", true)

    val recycler = Machine("recycler", "Recycler", 0.5, 4)
    val assembler2 = Machine("assembling-machine-2", "Assembling machine 2", 0.75, 2)

    val speedModule3Recipe = Recipe(
        "speed-module-3", "Speed module 3", 60, Array(assembler2),
        Map(i1 -> 1, i2 -> 4, i3 -> 5, i4 -> 5),
        Map(i5 -> 1), Array(), Map()
    )

    // relies on `formatNumber` being called inside displayCalc to smooth out the doubles (with minor precision loss)
    // calling assertEquals is better if it passes
    def assertEqualRes(res1: CalcRes, res2: CalcRes) =
        assertEquals(displayCalc(res1), displayCalc(res2))

    test("Works for Speed Module 3 recipe with max quality modules"):
        val res = calcForDisplay(5, 1, 5, speedModule3Recipe, 0.31, 0.248, 0.5, 1.5, 0.4)
        val expInputs = List(
            ("Superconductor", 1, 30.1),
            ("Speed module 2", 1, 120.4),
            ("Advanced circuit", 1, 150.5),
            ("Processing unit", 1, 150.5),
        )
        val expOutputs = List(
            ("Speed module 3", List(5 -> 1.0))
        )
        val expMachineUsage = List(
            1 -> 24.91,
            2 -> 4.22,
            3 -> 1.86,
            4 -> 0.695,
            5 -> 0.15,
        )
        val expRecyclerUses = 11.04
        val exp = (expInputs, expOutputs, ("Machines", expMachineUsage), expRecyclerUses)
        assertEqualRes(res, exp)

    def makeRecipe(outputs: Map[Item, Double],
                   inputs: Map[Item, Int] = Map(i1 -> 1, i4 -> 2),
                   producers: Array[Machine] = Array(assembler2),
                   craftingTime: Double = 1.0,
                   catalysts: Map[String, Int] = Map()) = Recipe(
        "test", "Test", craftingTime, producers, inputs, outputs, Array(), catalysts
    )
    def makeCatalystRecipe(outputs: Map[Item, Double], catalysts: Map[String, Int]) =
        makeRecipe(inputs = Map(i1 -> 1, i4 -> 2), outputs = outputs, catalysts = catalysts)

    test("Works with catalyst - simple"):
        val recipe1 = makeCatalystRecipe(Map(i2 -> 10), Map())
        val recipe2 = makeCatalystRecipe(Map(i2 -> 10), Map(i2.id -> 4))
        val res1 = calcForDisplay(5, 1, 5, recipe1, 0.5, 0.4, 0.12, 1.2, 0.6)
        val res2 = calcForDisplay(5, 1, 5, recipe2, 0.5, 0.4, 0.2, 1.2, 0.6)
        assertEquals(res1, res2)

    test("Works with catalyst - multiple outputs, main is catalyst"):
        val recipe1 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map())
        val recipe2 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map(i2.id -> 4))
        val res1 = calcForDisplay(5, 1, 4, recipe1, 0.41, 0.28, 0.12, 1, 0.4)
        val (inputs2, outputs2, machines2, recyclerUses2) = calcForDisplay(5, 1, 4, recipe2, 0.41, 0.28, 0.2, 1, 0.4)
        val expOutputs = outputs2.map: (name, results) =>
            if name == i3.name then
                // should make their amounts match 0.12 productivity
                (name, results.map((qual, amount) => (qual, amount / 1.2 * 1.12)))
            else
                (name, results)
        assertEqualRes(res1, (inputs2, expOutputs, machines2, recyclerUses2))

    test("Works with catalyst - multiple outputs, byproduct is catalyst"):
        val recipe1 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map())
        val recipe2 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map(i3.id -> 4))
        val res1 = calcForDisplay(5, 2, 5, recipe1, 0.3, 0.2, 0.2, 3.6, 1.4)
        val (inputs2, outputs2, machines2, recyclerUses2) = calcForDisplay(5, 2, 5, recipe2, 0.3, 0.2, 0.2, 3.6, 1.4)
        val expOutputs = outputs2.map: (name, results) =>
            if name == i3.name then
                // should make their amounts match 0.2 productivity
                (name, results.map((qual, amount) => (qual, amount / 1.12 * 1.2)))
            else
                (name, results)
        assertEqualRes(res1, (inputs2, expOutputs, machines2, recyclerUses2))

    test("Works with catalyst - multiple outputs, both are catalysts"):
        val recipe1 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map())
        val recipe2 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map(i2.id -> 4, i3.id -> 4))
        val res1 = calcForDisplay(5, 1, 5, recipe1, 0.2, 0.23, 0.12, 1.5, 0.4)
        val res2 = calcForDisplay(5, 1, 5, recipe2, 0.2, 0.23, 0.2, 1.5, 0.4)
        assertEquals(res1, res2) // stricter than assertEqualsRes

    test("Works with a recipe that recycles into itself"):
        val recipe = makeRecipe(inputs = Map(i1 -> 1), outputs = Map(i1 -> 0.25), producers = Array(recycler), craftingTime = 0.2)
        val res = calcForDisplay(5, 1, 4, recipe, 0.97, 0.248, 70, 11, 0.4)
        val expInputs = List(("Superconductor", 1, 511.2))
        val expOutputs = List(("Superconductor", List(4 -> 1.0, 5 -> 0.111)))
        val expRecyclerUses = 5.63
        val exp = (expInputs, expOutputs, ("", res._3._2), expRecyclerUses)
        assertEqualRes(res, exp)

    // test("Works with a fluid as an input"):
    //     // Exactly equivalent to making rocket fuel in an assembler
    //     val recipe1 = makeRecipe(inputs = Map(i1 -> 10, f1 -> 10), outputs = Map(i2 -> 1))
    //     val recipe2 = makeRecipe(inputs = Map(i1 -> 10), outputs = Map(i2 -> 1))
    //     val (inputs1, outputs1, machine1, rec1) = calcForDisplay(5, 1, 5, recipe1, 0.248, 0.248, 0, 1, 0.4)
    //     val (inputs2, outputs2, machine2, rec2) = calcForDisplay(5, 1, 5, recipe2, 0.248, 0.248, 0, 1, 0.4)
    //     val (solidInputs, liquidInputs) = inputs1.span(_._1 == i1.name)
    //     val expLiquid = (f1.name, 1, 2060.0) // TODO: adjust the number (trust the code if it's ~2060)
    //     assertEquals(liquidInputs.head, expLiquid)
    //     assertEquals(solidInputs, inputs2)
    //     assertEquals(outputs1, outputs2)
    //     assertEquals(machine1, machine2)
    //     assertEquals(rec1, rec1)
