package calculator_test

import calculator.CalculationView.{calcForDisplay, displayCalc}
import calculator.factorio_data.{Item, Recipe}
import calculator.CalculationView.CalcRes

class CalculationViewSuite extends munit.FunSuite:
    val i1 = Item("superconductor", "Superconductor", false)
    val i2 = Item("speed-module-2", "Speed module 2", false)
    val i3 = Item("advanced-circuit", "Advanced circuit", false)
    val i4 = Item("processing-unit", "Processing unit", false)
    val i5 = Item("speed-module-3", "Speed module 3", false)
    val speedModule3Recipe = Recipe(
        "speed-module-3", "Speed module 3", 60, Array(),
        Map(i1 -> 1, i2 -> 4, i3 -> 5, i4 -> 5),
        Map(i5 -> 1), Array(), Map()
    )
    // relies on `formatNumber` being called inside displayCalc to smooth out the doubles (with minor precision loss)
    def assertEqualRes(res1: CalcRes, res2: CalcRes) =
        assertEquals(displayCalc(res1), displayCalc(res2))

    test("Works for Speed Module 3 recipe with max quality modules"):
        val res1 = calcForDisplay(5, 1, 5, speedModule3Recipe, 0.31, 0.248, 0.5, 1.5, 0.4)
        val expectedInputs = List(
            ("Superconductor", 1, 30.1),
            ("Speed module 2", 1, 120.4),
            ("Advanced circuit", 1, 150.5),
            ("Processing unit", 1, 150.5),
        )
        val expectedOutputs = List(
            ("Speed module 3", List(5 -> 1.0))
        )
        val expectedMachineUsage = List(
            1 -> 24.91,
            2 -> 4.22,
            3 -> 1.86,
            4 -> 0.695,
            5 -> 0.15,
        )
        val expectedRecyclerUses = 11.04
        val res2 = (expectedInputs, expectedOutputs, ("Machines", expectedMachineUsage), expectedRecyclerUses)
        assertEqualRes(res1, res2)

    def makeCatalystRecipe(outputs: Map[Item, Double], catalysts: Map[String, Int]) = Recipe(
        "test", "Test", 1, Array(), Map(i1 -> 1, i4 -> 2), outputs, Array(), catalysts
    )

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
        val modifiedOutputs2 = outputs2.map: (name, results) =>
            if name == i3.name then
                // should make their amounts match 0.12 productivity
                (name, results.map((qual, amount) => (qual, amount / 1.2 * 1.12)))
            else
                (name, results)
        assertEqualRes(res1, (inputs2, modifiedOutputs2, machines2, recyclerUses2))

    test("Works with catalyst - multiple outputs, byproduct is catalyst"):
        val recipe1 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map())
        val recipe2 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map(i3.id -> 4))
        val res1 = calcForDisplay(5, 2, 5, recipe1, 0.3, 0.2, 0.2, 3.6, 1.4)
        val (inputs2, outputs2, machines2, recyclerUses2) = calcForDisplay(5, 2, 5, recipe2, 0.3, 0.2, 0.2, 3.6, 1.4)
        val modifiedOutputs2 = outputs2.map: (name, results) =>
            if name == i3.name then
                // should make their amounts match 0.2 productivity
                (name, results.map((qual, amount) => (qual, amount / 1.12 * 1.2)))
            else
                (name, results)
        assertEqualRes(res1, (inputs2, modifiedOutputs2, machines2, recyclerUses2))

    test("Works with catalyst - multiple outputs, both are catalysts"):
        val recipe1 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map())
        val recipe2 = makeCatalystRecipe(Map(i2 -> 10, i3 -> 10), Map(i2.id -> 4, i3.id -> 4))
        val res1 = calcForDisplay(5, 1, 5, recipe1, 0.2, 0.23, 0.12, 1.5, 0.4)
        val res2 = calcForDisplay(5, 1, 5, recipe2, 0.2, 0.23, 0.2, 1.5, 0.4)
        assertEquals(res1, res2)
