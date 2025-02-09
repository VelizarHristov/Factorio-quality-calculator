package calculator

import com.raquo.laminar.api.L.render
import org.scalajs.dom.{document, fetch, RequestInit, HttpMethod}

import concurrent.ExecutionContext

@main
def main(): Unit =
    given ExecutionContext = ExecutionContext.global
    for response <- fetch("/data.json", new RequestInit { method = HttpMethod.GET }).toFuture
        jsonStr <- response.text().toFuture
    do
        val (items, recipes) = DataParser.parse(jsonStr)
        val mainPage = MainPage(items, recipes)
        render(
            document.getElementById("app"),
            mainPage.render()
        )
