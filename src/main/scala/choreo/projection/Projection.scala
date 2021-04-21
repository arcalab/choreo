package choreo.projection

trait Projection[A,S]:
  def proj(s:S,a:A):S
  def getElements(s:S):Set[A]

  // Auxiliar
  def allAProj(s:S): Set[(A,S)] =
    for a<-getElements(s) yield a->proj(s,a)

  def allProj(s:S): Set[S] =
    for a<-getElements(s) yield proj(s,a)