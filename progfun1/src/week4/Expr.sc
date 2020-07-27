trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(v: String) extends Expr

def show(e: Expr):String = e match {
  case Number(n) => n.toString
  case Sum(l, r) => show(l) + " + " + show(r)
  case Prod(Sum(l1, r1), Sum(l2, r2)) => "(" + show(Sum(l1, r1)) + ") * (" + show(Sum(l2, r2)) + ")"
  case Prod(l, Sum(l2, r2)) => show(l) + " * (" + show(Sum(l2, r2)) + ")"
  case Prod(Sum(l1, r1), r) => "(" + show(Sum(l1, r1)) + ") * " + show(r)
  case Prod(l, r) => show(l) + " * " + show(r)
  case Var(v) => v
}

show(Sum(Number(1), Number(42)))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))
