object testProgram {

    type IOProgram = Print

    case class Print(msg: String)

    def run(program: IOProgram): Unit =
    program match {
      case Print(msg) => println(msg)
    }

}

import testProgram._;

run( Print("Hello, world!") );

