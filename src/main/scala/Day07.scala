package advent

object Day07 {

  def day07(): Unit = {
    val input = readFile(inputFile)
    println(s"Day07.part1 = ${part1(input)}")
    println(s"Day07.part2 = ${part2(input)}")
  }

  def part1(input: List[Component]): Int = {
    val result = run(State(emptyEnvironment, input))
    result.env("a")
  }

  def part2(input: List[Component]): Int = {
    val r1 = run(State(emptyEnvironment, input))
    val r2 = run(State(Map("b" -> r1.env("a")), input))

    r2.env("a")
  }

  def run(state: State): State = {
    def helper(s: State): State =
      if(s.components.isEmpty) s
      else                     helper(runIteration(s))

    helper(state)
  }

  def runIteration(state: State): State = {
    val (ne, nc) =
      state.components.foldLeft((state.env, List.empty[Component])) { case ((e, cs), c) =>
        evalComponent(e, c).fold{
          (e, c :: cs)
        } { env => (env, cs) }
      }

    State(ne, nc.reverse)
  }

  def evalComponent(env: Environment, c: Component): Option[Environment] =
    c match {
      case g: ConstGate  => evalConstGate(env, g)
      case g: AndGate    => evalAndGate(env, g)
      case g: OrGate     => evalOrGate(env, g)
      case g: NotGate    => evalNotGate(env, g)
      case g: LShiftGate => evalLShiftGate(env, g)
      case g: RShiftGate => evalRShiftGate(env, g)
    }

  def evalConstGate(env: Environment, g: ConstGate): Option[Environment] =
    g match { case ConstGate(in, Wire(out)) =>
      for {
        v <- evalConnection(env, in)
      } yield updateEnvironment(env, out, v)
    }

  def evalAndGate(env: Environment, g: AndGate): Option[Environment] =
    g match { case AndGate(i1, i2, Wire(out)) =>
      for {
        v1 <- evalConnection(env, i1)
        v2 <- evalConnection(env, i2)
        r  =  (v1 & v2) & 0x0000FFFF
      } yield updateEnvironment(env, out, r)
    }

  def evalOrGate(env: Environment, g: OrGate): Option[Environment] =
    g match { case OrGate(i1, i2, Wire(out)) =>
      for {
        v1 <- evalConnection(env, i1)
        v2 <- evalConnection(env, i2)
        r  =  (v1 | v2) & 0x0000FFFF
      } yield updateEnvironment(env, out, r)
    }

  def evalNotGate(env: Environment, g: NotGate): Option[Environment] =
    g match { case NotGate(i, Wire(out)) =>
      for {
        v <- evalConnection(env, i)
        r =  ~v & 0x0000FFFF
      } yield updateEnvironment(env, out, r)
    }

  def evalLShiftGate(env: Environment, g: LShiftGate): Option[Environment] =
    g match { case LShiftGate(in1, in2, Wire(out)) =>
      for {
        v1 <- evalConnection(env, in1)
        v2 <- evalConnection(env, in2)
        r  =  (v1 << v2) & 0x0000FFFF
      } yield updateEnvironment(env, out, r)
    }

  def evalRShiftGate(env: Environment, g: RShiftGate): Option[Environment] =
    g match { case RShiftGate(in1, in2, Wire(out)) =>
      for {
        v1 <- evalConnection(env, in1)
        v2 <- evalConnection(env, in2)
        r  =  (v1 >> v2) & 0x0000FFFF
      } yield updateEnvironment(env, out, r)
    }

  def evalConnection(env: Environment, c: Connection): Option[Int] = {
    c match {
      case Constant(v) => Some(v)
      case Wire(w)     => env.get(w)
    }
  }

  type Environment = Map[String, Int]
  val emptyEnvironment = Map.empty[String, Int]

  def updateEnvironment(env: Environment, name: String, value: Int): Environment =
    if(env.contains(name)) env
    else                   env + (name -> value)

  case class State(env: Environment, components: List[Component])

  sealed trait Connection
  case class Wire(name: String) extends Connection
  case class Constant(value: Int) extends Connection

  sealed trait Component
  case class ConstGate(in: Connection, out: Wire)                  extends Component
  case class AndGate(in1: Connection, in2: Connection, out: Wire)  extends Component
  case class OrGate(in1: Connection, in2: Connection, out: Wire)   extends Component
  case class NotGate(in1: Connection, out: Wire)                   extends Component
  case class LShiftGate(in1: Connection, in2: Constant, out: Wire) extends Component
  case class RShiftGate(in1: Connection, in2: Constant, out: Wire) extends Component

  val wireRegex = """([a-z]+)""".r
  val constantRegex = """(\d+)""".r

  def parseWire(s: String): Wire =
    s match {
      case wireRegex(name) => Wire(name)
    }

  def parseConstant(s: String): Constant =
    s match {
      case constantRegex(value) => Constant(value.toInt)
    }

  def parseConnection(s: String): Connection =
    s match {
      case constantRegex(value) => Constant(value.toInt)
      case wireRegex(name)      => Wire(name)
    }

  val constRegex = """(\S+) -> (\S+)""".r
  val andRegex = """(\S+) AND (\S+) -> (\S+)""".r
  val orRegex = """(\S+) OR (\S+) -> (\S+)""".r
  val notRegex = """NOT (\S+) -> (\S+)""".r
  val lshiftRegex = """(\S+) LSHIFT (\S+) -> (\S+)""".r
  val rshiftRegex = """(\S+) RSHIFT (\S+) -> (\S+)""".r

  def parseComponent(s: String): Component =
    s match {
      case constRegex(i1, o)      => ConstGate(parseConnection(i1), parseWire(o))
      case andRegex(i1, i2, o)    => AndGate(parseConnection(i1), parseConnection(i2), parseWire(o))
      case orRegex(i1, i2, o)     => OrGate(parseConnection(i1), parseConnection(i2), parseWire(o))
      case notRegex(i1, o)        => NotGate(parseConnection(i1), parseWire(o))
      case lshiftRegex(i1, i2, o) => LShiftGate(parseConnection(i1), parseConstant(i2), parseWire(o))
      case rshiftRegex(i1, i2, o) => RShiftGate(parseConnection(i1), parseConstant(i2), parseWire(o))
    }

  def readFile(file: String): List[Component] =
    io.Source.fromFile(file)
      .getLines()
      .map(parseComponent)
      .toList

  val inputFile = "data/Day07.txt"
}
