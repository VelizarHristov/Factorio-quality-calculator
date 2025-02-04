package calculator

import collection.mutable

object Calculator {
  // quality is represented by numbers 1-5 for normal-legendary (in-game it's 0,1,2,3,5 instead)
  case class Item(name: String, count: Double, quality: Int)
  case class Recipe(item1: String, item2: String, inputCount: Double, outputCount: Double)
  case class ProductionRes(legResults: Double, prodMachine: Map[Int, Double], recMachine: Map[Int, Double]) {
    def *(by: Double) = ProductionRes(
      legResults * by,
      prodMachine.view.mapValues(_ * by).toMap,
      recMachine.view.mapValues(_ * by).toMap
    )
    def /(by: Double) = this * (1 / by)
    def +(that: ProductionRes) = ProductionRes(
      legResults + that.legResults,
      (1 to 5).map(i => i -> (prodMachine(i) + that.prodMachine(i))).toMap,
      (1 to 5).map(i => i -> (recMachine(i) + that.recMachine(i))).toMap
    )
    def addResults(results: Double) = copy(legResults = results + legResults)
    def addProdUses(qual: Int, uses: Double) = copy(prodMachine = prodMachine.updatedWith(qual)(_.map(_ + uses)))
    def addRecUses(qual: Int, uses: Double) = copy(recMachine = recMachine.updatedWith(qual)(_.map(_ + uses)))
  }
  def newProdRes(legResults: Double = 0.0,
                 prodMachine: Map[Int, Double] = Map.empty,
                 recMachine: Map[Int, Double] = Map.empty) = ProductionRes(legResults,
    (1 to 5).map(i => i -> (prodMachine.getOrElse(i, 0.0))).toMap,
    (1 to 5).map(i => i -> (recMachine.getOrElse(i, 0.0))).toMap)
}

class Calculator(lastUnlockedQual: Int = 5) {
  import Calculator._
  def doRecipe(recipe: Recipe,
               input: Item,
               qualityInMachine: Double,
               productivityInMachine: Double): Seq[Item] = {
    val Recipe(_, outputName, recipeInputCount, recipeOutputCount) = recipe
    val Item(inputName, inputCount, inputQuality) = input
    val totalOutput = (input.count / recipeInputCount) * recipeOutputCount * (1 + productivityInMachine)
    if (inputQuality == lastUnlockedQual) {
      List(Item(outputName, totalOutput, lastUnlockedQual))
    } else {
      Item(
        outputName, totalOutput * (1 - qualityInMachine), inputQuality
      ) +: doRecipe(recipe, Item(inputName, inputCount * qualityInMachine, inputQuality + 1), 0.1, productivityInMachine)
    }
  }

  def doRecycle(recipe: Recipe, input: Item, qualityInMachine: Double): Seq[Item] = {
    val Recipe(item1, item2, count1, count2) = recipe
    doRecipe(Recipe(item2, item1, count2 * 4, count1), input, qualityInMachine, 0)
  }

  /**
   * First calculate how many legendary products we are getting per epic ingredient
   *   It works as follows:
   *   1. Put 1.0 epic ingredients in the machine, to get epic + legendary products
   *   2. Put all epic products in the recycler, to get epic + legendary ingredients (+ leftover legendary products from last step)
   *   3. Put all legendary ingredients in the machine, to more legendary products
   *   4. Now we have put 1.0 epic ingredients and gotten fewer epic ingredients + some legendary products
   *   5. Subtract the leftover epic ingredients from how many we've inputted, for example:
   *        1 ingredient => 0.2 ingredients + 0.3 legendary products, then 0.8 ingredients => 0.3 legendary products
   *   6. Save the ratio - in the above example, 1 epic ingredient gives 0.3/0.8 = 3/8 = 0.375 legendary products.
   * Then similarly calculate how many legendary products we are getting per epic product (by recycling it).
   *   We reuse most of the previous work for this.
   * With this information, we can similarly calculate for rare ingredients to legendary products:
   *   Any epic products or ingredients are converted to legendary using the already calculated ratio above.
   * Repeat for the lower qualities.
    */
  def calcSpeeds(recipe: Recipe,
                 qualInMachine: Double,
                 qualInRec: Double,
                 prodInMachine: Double,
                 ingredientQual: Int,
                 recipeCraftingTimeSec: Double,
                 machineSpeed: Double): ProductionRes = {
    val Recipe(item1, item2, inputCount, outputCount) = recipe
    val productToProduct = mutable.Map(lastUnlockedQual -> newProdRes(1.0))
    val ingredientToProduct = mutable.Map(lastUnlockedQual -> newProdRes(
        (outputCount / inputCount) * (1 + prodInMachine),
        Map(lastUnlockedQual -> 1)
    ))
    for (qual <- (lastUnlockedQual - 1) to ingredientQual by -1) {
      val products = doRecipe(recipe, Item(item1, 1, qual), qualInMachine, prodInMachine)
      val sameQualProducts = products.minBy(_.quality).count
      val sameQualProductRatio = sameQualProducts / products.map(_.count).sum
      val higherQualRes = products.map(p =>
        productToProduct.getOrElse(p.quality, (newProdRes())) * p.count
      ).reduce(_ + _).addProdUses(qual, 1 - sameQualProductRatio)

      val ingredients = doRecycle(recipe, Item(item2, 1, qual), qualInRec)
      val nextIngredients = ingredients.map(i => i.copy(count = i.count * sameQualProducts))
      val sameQualIngredients = nextIngredients.minBy(_.quality).count
      val sameQualIngredientRatio = sameQualIngredients / nextIngredients.map(_.count).sum
      val higherQualResFromIngredients = nextIngredients.map(
        p => ingredientToProduct.getOrElse(p.quality, newProdRes()) * p.count
      ).reduce(_ + _).addProdUses(qual, sameQualProductRatio * (1 - sameQualIngredientRatio))
      .addRecUses(qual, sameQualProductRatio * (1 - sameQualIngredientRatio))
      val allHigherQualRes = (higherQualRes + higherQualResFromIngredients
      ).addProdUses(qual, sameQualProductRatio * sameQualIngredientRatio)
      .addRecUses(qual, sameQualProductRatio * sameQualIngredientRatio)
      val ingredientCost = 1 - sameQualIngredients
      ingredientToProduct(qual) = allHigherQualRes / ingredientCost
      productToProduct(qual) = ingredients.map(p => ingredientToProduct(p.quality) * p.count).reduce(_ + _)
    }

    val ProductionRes(resCount, prodMachines, recMachines) = ingredientToProduct(ingredientQual)
    val totalInputsNeeded = 1 / resCount
    val scale = totalInputsNeeded / inputCount
    val craftingTimeSec = recipeCraftingTimeSec / machineSpeed
    ProductionRes(totalInputsNeeded,
      prodMachines.view.mapValues(_ * scale * craftingTimeSec).toMap,
      recMachines.view.mapValues(_ * scale * recipeCraftingTimeSec / 8).toMap)
  }
}
