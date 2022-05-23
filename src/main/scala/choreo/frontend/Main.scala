package choreo.frontend

import caos.frontend.Site.initSite
import choreo.syntax.Choreo

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
//    initSite[Choreo](ICECaos)
//    initSite[Choreo](ChoreoSOSme)
    initSite[Choreo](APICaos)
}

