package calculator

import collection.mutable

// quality is represented by numbers 1-5 for normal-legendary (in-game it's 0,1,2,3,5 instead)
object Calculator:
    case class ProductionRes(results: Map[Int, Double], prodMachine: Map[Int, Double], recMachine: Map[Int, Double]):
        def *(by: Double) = ProductionRes(
            results.view.mapValues(_ * by).toMap,
            prodMachine.view.mapValues(_ * by).toMap,
            recMachine.view.mapValues(_ * by).toMap)
        def /(by: Double) = this * (1 / by)
        def +(that: ProductionRes) = ProductionRes(
            (1 to 5).map(i => i -> (results(i) + that.results(i))).toMap,
            (1 to 5).map(i => i -> (prodMachine(i) + that.prodMachine(i))).toMap,
            (1 to 5).map(i => i -> (recMachine(i) + that.recMachine(i))).toMap)
        def addResults(qual: Int, count: Double) = copy(results = results.updatedWith(qual)(_.map(_ + count)))
        def addProdUses(qual: Int, uses: Double) = copy(prodMachine = prodMachine.updatedWith(qual)(_.map(_ + uses)))
        def addRecUses(qual: Int, uses: Double) = copy(recMachine = recMachine.updatedWith(qual)(_.map(_ + uses)))
    def toQualMap(map: Map[Int, Double]) = (1 to 5).map(i => i -> (map.getOrElse(i, 0.0))).toMap
    def newProdRes(results: Map[Int, Double] = Map.empty,
                   prodMachine: Map[Int, Double] = Map.empty,
                   recMachine: Map[Int, Double] = Map.empty) = ProductionRes(
        toQualMap(results), toQualMap(prodMachine), toQualMap(recMachine))

class Calculator(lastUnlockedQual: Int = 5):
    import Calculator._
    def doRecipe(inputCount: Double,
                 inputQuality: Int,
                 qualityInMachine: Double,
                 productivityInMachine: Double): Map[Int, Double] =
        val totalOutput = inputCount * (1 + productivityInMachine)
        if inputQuality == lastUnlockedQual then
            Map(lastUnlockedQual -> totalOutput)
        else
            doRecipe(inputCount * qualityInMachine, inputQuality + 1, 0.1, productivityInMachine)
                .updated(inputQuality, totalOutput * (1 - qualityInMachine))

    def doRecycle(inputCount: Double, inputQuality: Int, qualityInRecycler: Double): Map[Int, Double] =
        doRecipe(inputCount, inputQuality, qualityInRecycler, 0).view.mapValues(_ / 4).toMap

    /** Assumes that the following equal 1: recipe crafting time, machine speed, recycler speed, number of inputs, number of outputs.
     * 
     * First calculate how many legendary products we are getting per epic ingredient
     *     It works as follows:
     *     1. Put 1.0 epic ingredients in the machine, to get epic + legendary products
     *     2. Put all epic products in the recycler, to get epic + legendary ingredients (+ leftover legendary products from last step)
     *     3. Put all legendary ingredients in the machine, to get more legendary products
     *     4. Now we have put 1.0 epic ingredients and gotten fewer epic ingredients + some legendary products
     *     5. Subtract the leftover epic ingredients from how many we've inputted, for example:
     *         1 ingredient => 0.2 ingredients + 0.3 legendary products, then 0.8 ingredients => 0.3 legendary products
     *     6. Save the ratio - in the above example, 1 epic ingredient gives 0.3/0.8 = 3/8 = 0.375 legendary products.
     * Then similarly calculate how many legendary products we are getting per epic product (by recycling it).
     *     We reuse most of the previous work for this.
     * With this information, we can similarly calculate for rare ingredients to legendary products:
     *     Any epic products or ingredients are converted to legendary using the already calculated ratio above.
     * Repeat for the lower qualities.
     */
    def calcSpeedsUnit(qualInMachine: Double,
                       qualInRec: Double,
                       prodInMachine: Double,
                       ingredientQual: Int,
                       targetQual: Int): ProductionRes =
        val productToProduct = mutable.Map[Int, ProductionRes]()
        val ingredientToProduct = mutable.Map[Int, ProductionRes]()
        for (qual <- lastUnlockedQual to ingredientQual by -1)
            if qual >= targetQual then
                productToProduct(qual) = newProdRes(Map(qual -> 1))
                ingredientToProduct(qual) = newProdRes(
                    doRecipe(1, qual, qualInMachine, prodInMachine), Map(qual -> 1))
            else
                val products = doRecipe(1, qual, qualInMachine, prodInMachine)
                val nextIngredients = doRecycle(products(qual), qual, qualInRec)
                val higherQualRes = products
                    .filter(_._1 > qual)
                    .map((q, c) => productToProduct(q) * c)
                    .reduce(_ + _)
                val higherQualResFromIngredients = nextIngredients
                    .filter(_._1 > qual)
                    .map((q, c) => ingredientToProduct(q) * c)
                    .reduce(_ + _)
                val allHigherQualRes = (higherQualRes + higherQualResFromIngredients)
                    .addProdUses(qual, 1).addRecUses(qual, products(qual))
                val ingredientCost = 1 - nextIngredients(qual)
                ingredientToProduct(qual) = allHigherQualRes / ingredientCost
                productToProduct(qual) = doRecycle(1, qual, qualInRec)
                    .map((q, c) => ingredientToProduct(q) * c)
                    .reduce(_ + _).addRecUses(qual, 1)
        ingredientToProduct(ingredientQual)

    // Assumes recipe is 1 input => 1 output (e.g. 1 Iron Plate => 1 Pipe)
    def calcSpeeds(qualInMachine: Double,
                   qualInRec: Double,
                   prodInMachine: Double,
                   ingredientQual: Int,
                   targetQual: Int,
                   recipeCraftingTimeSec: Double,
                   machineSpeed: Double,
                   recyclerSpeed: Double): ProductionRes =
        val ProductionRes(resCount, prodMachines, recMachines) = calcSpeedsUnit(qualInMachine, qualInRec, prodInMachine, ingredientQual, targetQual)
        val craftingTimeSec = recipeCraftingTimeSec / machineSpeed
        val recyclingTimeSec = (recipeCraftingTimeSec / 16) / recyclerSpeed
        ProductionRes(resCount,
            prodMachines.view.mapValues(_ * craftingTimeSec / 60).toMap,
            recMachines.view.mapValues(_ * recyclingTimeSec / 60).toMap)
