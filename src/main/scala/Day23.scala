package advent

object Day23 {

  def day23(): Unit = {
    println(s"Day23.part1 = ${part1(puzzleInput)}")
    println(s"Day23.part2 = ${part2(puzzleInput)}")
  }

  def part1(input: String): Int = {
    val m = runMachine(readFile(input))
    getReg('b', m.registers)
  }

  def part2(input: String): Int = {
    val m = readFile(input)
    val result = runMachine(m.copy(registers = setReg('a', 1, m.registers)))
    getReg('b', result.registers)
  }

  type Registers = Map[Char, Int]

  sealed trait Instruction
  case class HLF(register: Char)            extends Instruction
  case class TPL(register: Char)            extends Instruction
  case class INC(register: Char)            extends Instruction
  case class JMP(offset: Int)                 extends Instruction
  case class JIE(register: Char, offset: Int) extends Instruction
  case class JIO(register: Char, offset: Int) extends Instruction

  sealed trait State
  case object Runnable extends State
  case object Stopped  extends State

  case class Machine(registers: Registers, ip: Int, state: State, memory: Vector[Instruction])

  def runMachine(machine: Machine): Machine = {
    if(machine.state == Runnable) runMachine(step(machine))
    else                         machine
  }

  def evalInstruction(i: Instruction, rs: Registers, ip: Int): (Registers, Int) =
    i match {
      case HLF(r)                          => (setReg(r, getReg(r, rs) / 2, rs), ip+1)
      case TPL(r)                          => (setReg(r, getReg(r, rs) * 3, rs), ip+1)
      case INC(r)                          => (setReg(r, getReg(r, rs) + 1, rs), ip+1)
      case JMP(o)                          => (rs, ip + o)
      case JIE(r, o) if isRegEven(r, rs)   => (rs, ip + o) // Even, so jump
      case JIE(r, o)                       => (rs, ip+1)   // Not even, so don't jump
      case JIO(r, o) if(getReg(r, rs)) == 1 => (rs, ip + o)
      case JIO(r, o)                       => (rs, ip + 1)
    }

  def step(machine: Machine): Machine = {
    machine match {
      case Machine(rs, ip, Runnable, mem) =>
        if(mem.isDefinedAt(ip)) {
          val (r, i) = evalInstruction(mem(ip), rs, ip)
          machine.copy(registers=r, ip=i)
        } else {
          machine.copy(state=Stopped)
        }
      case Machine(_, _, Stopped, _) => machine
    }
  }

  def setReg(r: Char, v: Int, registers: Registers): Registers =
    registers.updated(r, v)

  def getReg(r: Char, registers: Registers): Int =
    registers.get(r).getOrElse(0)

  def isRegEven(r: Char, registers: Registers): Boolean =
    isEven(getReg(r, registers))

  def isEven(i: Int): Boolean = i % 2 == 0

  def mkMachine(mem: Vector[Instruction]): Machine =
    Machine(Map.empty[Char, Int], 0, Runnable, mem)

  val HLFRegex = """hlf\s+(\w)""".r
  val TPLRegex = """tpl\s+(\w)""".r
  val INCRegex = """inc\s+(\w)""".r
  val JMPRegex = """jmp\s+([+-]\d+)""".r
  val JIERegex = """jie\s+(\w),\s+([+-]\d+)""".r
  val JIORegex = """jio\s+(\w),\s+([+-]\d+)""".r

  def parseInstruction(s: String): Instruction =
    s match {
      case HLFRegex(r)    => HLF(r.head)
      case TPLRegex(r)    => TPL(r.head)
      case INCRegex(r)    => INC(r.head)
      case JMPRegex(o)    => JMP(o.toInt)
      case JIERegex(r, o) => JIE(r.head, o.toInt)
      case JIORegex(r, o) => JIO(r.head, o.toInt)
    }

  def readFile(file: String): Machine =
    mkMachine(
      io.Source.fromFile(file)
        .getLines()
        .map(parseInstruction)
        .toVector)

  val puzzleInput = "data/Day23.txt"
}
