package calculator.factorio_data

import scalajs.js.annotation.JSExportAll

@JSExportAll
class Machine(id: String, name: String, isFluid: Boolean, speed: Double, modules: Int) extends Item(id, name, isFluid)
