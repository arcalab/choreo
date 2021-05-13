package choreo.frontend

import choreo.frontend.ChoreoSOSme
import choreo.syntax.Choreo
import caos.frontend.Configurator
import caos.frontend.Site.initSite
import org.scalajs.dom.document
/**
 * Created by guillecledou on 03/05/2021
 */

object Main {
  def main(args: Array[String]):Unit = {
    initSite[Choreo](ChoreoSOSme)
  }

}
