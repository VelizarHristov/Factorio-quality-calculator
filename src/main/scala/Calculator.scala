package calculator

import collection.mutable

object Calculator {
  // quality is presented by numbers 1-5 for normal-legendary (in-game it's 0,1,2,3,5 instead)
  case class Item(name: String, count: Double, quality: Int)
  case class Recipe(item1: String, item2: String, inputCount: Double, outputCount: Double)
}

class Calculator(lastUnlockedQuality: Int = 5) {
  import Calculator._
  def doRecipe(recipe: Recipe,
              input: Item,
              qualityInMachine: Double,
              productivityInMachine: Double): Seq[Item] = {
    val Recipe(_, outputName, recipeInputCount, recipeOutputCount) = recipe
    val Item(inputName, inputCount, inputQuality) = input
    val totalOutput = (input.count / recipeInputCount) * recipeOutputCount * (1 + productivityInMachine)
    if (inputQuality == lastUnlockedQuality) {
      List(Item(outputName, totalOutput, lastUnlockedQuality))
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
   * 
   * (The naming used in the code says "legendary" or "leg" instead of "last unlocked quality" to avoid verbosity)
    */
  def calcCostToLegendary(recipe: Recipe,
                          qualityInMachine: Double,
                          qualityInRecycler: Double,
                          productivityInMachine: Double,
                          ingredientQuality: Int): Double = {
    val Recipe(item1, item2, inputCount, outputCount) = recipe
    val productToProduct = mutable.Map(lastUnlockedQuality -> 1.0)
    val ingredientToProduct = mutable.Map(lastUnlockedQuality -> (outputCount / inputCount) * (1 + productivityInMachine))
    for (qual <- (lastUnlockedQuality - 1) to ingredientQuality by -1) {
      val products = doRecipe(recipe, Item(item1, 1, qual), qualityInMachine, productivityInMachine)
      val initialLegProducts = products.map(p => p.count * productToProduct.getOrElse(p.quality, 0.0)).sum
      val sameQualityProducts = products.minBy(_.quality).count

      val ingredients = doRecycle(recipe, Item(item2, 1, qual), qualityInRecycler)
      val nextIngredients = ingredients.map(i => i.copy(count = i.count * sameQualityProducts))
      val sameQualityIngredients = nextIngredients.minBy(_.quality).count
      val legProductsFromIngredients = nextIngredients.map(p => p.count * ingredientToProduct.getOrElse(p.quality, 0.0)).sum
      val legProducts = initialLegProducts + legProductsFromIngredients
      val ingredientCost = 1 - sameQualityIngredients
      ingredientToProduct(qual) = legProducts / ingredientCost
      productToProduct(qual) = ingredients.map(p => p.count * ingredientToProduct(p.quality)).sum
    }
    1 / ingredientToProduct(ingredientQuality)
  }
}
