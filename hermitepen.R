hermitepen <- function(basisobj, Lfdobj=int2Lfd(2))
{

  Lfdobj=int2Lfd(Lfdobj)
    penaltymatrix <- inprod(basisobj, basisobj, Lfdobj, Lfdobj)
  return( penaltymatrix )
}