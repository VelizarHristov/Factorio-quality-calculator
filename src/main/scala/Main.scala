package calculator

import com.raquo.laminar.api.L.renderOnDomContentLoaded
import org.scalajs.dom

@main
def main(): Unit =
    renderOnDomContentLoaded(
        dom.document.getElementById("app"),
        MainPage()
    )
