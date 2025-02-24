package calculator

import collection.mutable

object Calculator:
    // quality is represented by numbers 1-5 for normal-legendary (in-game it's 0,1,2,3,5 instead)
    case class Item(count: Double, quality: Int)
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
    def doRecipe(input: Item,
                 qualityInMachine: Double,
                 productivityInMachine: Double): Seq[Item] =
        val Item(inputCount, inputQuality) = input
        val totalOutput = inputCount * (1 + productivityInMachine)
        if inputQuality == lastUnlockedQual then
            List(Item(totalOutput, lastUnlockedQual))
        else
            Item(
                totalOutput * (1 - qualityInMachine), inputQuality
            ) +: doRecipe(Item(inputCount * qualityInMachine, inputQuality + 1), 0.1, productivityInMachine)

    def doRecycle(input: Item, qualityInRecycler: Double): Seq[Item] =
        doRecipe(input, qualityInRecycler, 0).map:
            case Item(count, qual) => Item(count / 4, qual)

    /**
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
        val productToProduct = mutable.Map(lastUnlockedQual -> newProdRes(Map(lastUnlockedQual -> 1)))
        val ingredientToProduct = mutable.Map(lastUnlockedQual -> newProdRes(
            Map(lastUnlockedQual -> (1 + prodInMachine)),
            Map(lastUnlockedQual -> 1)
        ))
        if ingredientQual == targetQual then
            val products = doRecipe(
                Item(1, ingredientQual), qualInMachine, prodInMachine
            ).map{ case Item(count, quality) =>
                quality -> count
            }.toMap
            val prodRes = newProdRes(products, toQualMap(Map(ingredientQual -> 1.0)))
            ingredientToProduct(ingredientQual) = prodRes
        else
            for (qual <- (lastUnlockedQual - 1) to ingredientQual by -1)
                val products = doRecipe(Item(1, qual), qualInMachine, prodInMachine)
                val sameQualProducts = products.minBy(_.quality).count
                val sameQualProductRatio = sameQualProducts / products.map(_.count).sum
                val higherQualRes = products.filter(_.quality != qual).map{ case Item(count, quality) =>
                    if (quality >= targetQual)
                        newProdRes(Map(quality -> count))
                    else
                        productToProduct(quality) * count
                }.reduce(_ + _).addProdUses(qual, 1 - sameQualProductRatio)

                val ingredientsOf1 = doRecycle(Item(1, qual), qualInRec)
                val nextIngredients = ingredientsOf1.map(i => i.copy(count = i.count * sameQualProducts))
                val sameQualIngredients = nextIngredients.minBy(_.quality).count
                val sameQualIngredientRatio = sameQualIngredients / nextIngredients.map(_.count).sum
                val higherQualResFromIngredients = nextIngredients.filter(_.quality != qual).map(
                    p => ingredientToProduct.getOrElse(p.quality, newProdRes()) * p.count
                ).reduce(_ + _).addProdUses(qual, sameQualProductRatio * (1 - sameQualIngredientRatio))
                .addRecUses(qual, sameQualProductRatio * (1 - sameQualIngredientRatio))
                val allHigherQualRes = (higherQualRes + higherQualResFromIngredients
                ).addProdUses(qual, sameQualProductRatio * sameQualIngredientRatio)
                .addRecUses(qual, sameQualProductRatio * sameQualIngredientRatio)
                val ingredientCost = 1 - sameQualIngredients
                ingredientToProduct(qual) = allHigherQualRes / ingredientCost
                productToProduct(qual) = ingredientsOf1.map(p => ingredientToProduct(p.quality) * p.count).reduce(_ + _)
        ingredientToProduct(ingredientQual)

    def calcSpeeds(inputCount: Int,
                   outputCount: Int,
                   qualInMachine: Double,
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
        ProductionRes(
            resCount.view.mapValues(_ * outputCount / inputCount).toMap,
            prodMachines.view.mapValues(_ * craftingTimeSec / inputCount / 60).toMap,
            recMachines.view.mapValues(_ * recyclingTimeSec / inputCount / 60).toMap)
