package choreo.api

import choreo.api.MiniScala.*
import choreo.api.LocalAPI.*

case class LocalAPI(set:SetAPI,singles:List[SingleAPI]) extends Code:

  def toCode(implicit i: Int): String =
    set.toCode ++ "\n\n" ++ singles.map(l=>l.toCode).mkString("\n\n")

object LocalAPI:

  case class SetAPI(co: ScalaObject, magic: ScalaClass) extends Code:

    def toCode(implicit i: Int): String = co.toCode ++ "\n\n" ++ magic.toCode

  case class SingleAPI(clas:ScalaClass,co:ScalaObject) extends Code:

    def toCode(implicit i: Int): String =
      clas.toCode ++ "\n\n" ++ co.toCode
      
