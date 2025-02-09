package calculator

import upickle.default.{read, ReadWriter}

import factorio_data.{Item => FItem, Recipe => FRecipe, Machine => FMachine, Beacon => FBeacon, Module => FModule}

object DataParser:
    private case class Machine(speed: Double, modules: Int = 0) derives ReadWriter
    private case class Beacon(effectivity: Double, modules: Int, profile: Array[Double]) derives ReadWriter
    private case class Module(consumption: Double = 0, speed: Double = 0, productivity: Double = 0, quality: Double = 0) derives ReadWriter
    private case class Item(id: String, name: String, isFluid: Boolean, machine: Option[Machine] = None,
                            beacon: Option[Beacon] = None, module: Option[Module] = None) derives ReadWriter
    private case class Recipe(id: String, name: String, time: Double, producers: Array[String], in: Map[String, Int], out: Map[String, Int],
                              disallowedEffects: Array[String] = Array.empty, catalyst: Map[String, Int] = Map.empty) derives ReadWriter
    private case class FactorioData(items: Vector[Item], recipes: Vector[Recipe]) derives ReadWriter
    def parse(json: String): (Vector[FItem], Vector[FRecipe]) =
        val parsed = read[FactorioData](json)
        val items = parsed.items.map:
            case Item(id, name, isFluid, Some(Machine(speed, modules)), _, _) => FMachine(id, name, isFluid, speed, modules)
            case Item(id, name, isFluid, _, Some(Beacon(effectivity, modules, profile)), _) => FBeacon(id, name, isFluid, effectivity, modules, profile)
            case Item(id, name, isFluid, _, _, Some(Module(consumption, speed, productivity, quality))) => FModule(id, name, isFluid, consumption, speed, productivity, quality)
            case Item(id, name, isFluid, _, _, _) => FItem(id, name, isFluid)
        val itemsMap = items.map(i => i.id -> i).toMap
        val machinesMap = itemsMap.collect { case (k, v: FMachine) => (k, v) }
        val recipes = parsed.recipes.map:
            case Recipe(id, name, time, producers, in, out, disallowedEffects, catalyst) =>
                val newProducers = producers.map(machinesMap)
                val newIn = in.map((k, v) => itemsMap(k) -> v)
                val newOut = out.map((k, v) => itemsMap(k) -> v)
                val newCatalysts = catalyst.map((k, v) => itemsMap(k) -> v)
                FRecipe(id, name, time, newProducers, newIn, newOut, disallowedEffects, newCatalysts)
        (items, recipes)
