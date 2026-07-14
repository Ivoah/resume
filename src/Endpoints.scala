package net.ivoah.resume

import net.ivoah.vial.*

import java.io.File
import java.nio.file.Paths
import java.time.LocalDate

class Endpoints(debug: Boolean = false) {
  def router: Router = Router {
    case (_, _, _, e) if debug =>
      e.printStackTrace()
      Response.InternalServerError(e)

    case ("GET", s"/static/$file", _) =>
      Response.forFile(Paths.get("static"), Paths.get(file))

    case ("GET", "/", _) => Response(Templates.resume())
  }
}
