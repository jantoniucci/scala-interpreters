object testProgram2 {

  // Program

  def authorized: IOProgram[Boolean] =
    Sequence(Effect(Print("user:")), (_: Unit) =>
      Sequence(Effect(Read()), (user: String) =>
        Sequence(Effect(Print("password:")), (_: Unit) =>
          Sequence(Effect(Read()), (pw: String) =>
            Value(user=="javier" && pw=="antoniucci")
          )
        )
      )
    )

  // Language

  sealed trait IOProgram[T]

  case class Effect[T](effect: IOEffect[T])
    extends IOProgram[T]

  case class Sequence[S,T](
                            program1: IOProgram[S],
                            cont: S => IOProgram[T]
                          ) extends IOProgram[T]

  case class Value[T](t: T)
    extends IOProgram[T]

  sealed trait IOEffect[T]
  case class Print(msg: String) extends IOEffect[Unit]
  case class Read() extends IOEffect[String]

  // Interpreter

  object Interpreter {
    def runEffect[T](effect: IOEffect[T]): T =
      effect match {
        case Print(msg) => println(msg)
        case Read() => readLine
      }

    def run[T](program: IOProgram[T]): T =
      program match {
        case Effect(effect) => runEffect(effect)
        case Sequence(p1,cont) =>
          val r = run(p1)
          run(cont(r))
        case Value(v) => v
      }
  }

}

import testProgram2._

Interpreter.run( authorized )

