package choreo.choreo2.backend

import choreo.choreo2.syntax.Choreo.Action

trait LTS[S<:Any](init:S):
  type St = S
  def trans: Set[(Action,LTS[S])]
  def accepting: Boolean
  def isEmpty: Boolean