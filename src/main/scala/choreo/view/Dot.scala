package choreo.view

/**
 * Created by guillecledou on 31/10/2020
 */

trait Dot[-A]:
  extension(e:A)
    def toDot:String
