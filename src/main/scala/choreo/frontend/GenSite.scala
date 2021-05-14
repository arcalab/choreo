package choreo.frontend

import caos.frontend.Site.initSite
import choreo.syntax.Choreo

/** Main function called by ScalaJS' compiled javascript when loading. */
@main def genSite() = initSite[Choreo](ChoreoSOSme)
