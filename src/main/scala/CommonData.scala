package calculator

object CommonData:
    val qualities = List(
        1 -> "Normal",
        2 -> "Uncommon",
        3 -> "Rare",
        4 -> "Epic",
        5 -> "Legendary")
    def qualityToStr = qualities.toMap.apply
