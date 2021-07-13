package choreo.datastructures

import choreo.datastructures.Isomorphism.IsoResult

/**
 * Created by guillecledou on 12/07/2021
 */

//trait Isomorphic[S1,S2]:
//  def isIsomorphic[V1,V2](s1:S1,s2:S2,comp:((V1,V2))=>Boolean):IsoResult[V1,V2]

object Isomorphism:
  type Isomorphism[V1,V2]   = Set[(V1,V2)]
  type Isomorphisms[V1,V2]  = Set[Isomorphism[V1,V2]]
  type IsoResult[V1,V2]     = Option[Isomorphisms[V1,V2]]

  def ppIso[V1,V2](iso:Isomorphism[V1,V2]):String =
    iso.mkString(",")

  def ppIsos[V1,V2](isos:Isomorphisms[V1,V2]):String =
    isos.map(iso=>ppIso(iso)).mkString("\n")

