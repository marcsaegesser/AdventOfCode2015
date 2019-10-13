package advent

object Day22 {

  sealed trait Spell { val cost: Int }
  case object MagicMissileSpell extends Spell { val cost = 53 }
  case object DrainSpell        extends Spell { val cost = 73 }
  case object ShieldSpell       extends Spell { val cost = 113 }
  case object PoisonSpell       extends Spell { val cost = 173 }
  case object RechargeSpell     extends Spell { val cost = 229 }

  sealed trait Turn
  case object PlayerTurn extends Turn
  case object BossTurn   extends Turn

  sealed trait Effect { val time: Int }
  case object Shield   extends Effect { val time = 6 }
  case object Poison   extends Effect { val time = 6 }
  case object Recharge extends Effect { val time = 5 }

  val magicMissileDamage = 4
  val drainAmount        = 2
  val shieldArmor        = 7
  val poisonDamage       = 3
  val rechargeAmount     = 101

  val initialPlayerHP   = 50
  val initialPlayerMana = 500

  case class GameState(
    mana: Int,
    playerArmor: Int,
    playerHP: Int,
    bossHP: Int,
    bossDamange: Int,
    effects: Map[Effect, Int],
    turnDamage: Int,
    manaSpent: Int,
    turn: Turn
  )

  type Result = Int

  def part1(initialState: GameState): Int =
    run(0, initialState, Int.MaxValue)

  def part2(initialState: GameState): Int =
    run(0, initialState.copy(turnDamage=1), Int.MaxValue)

  def run(level: Int, state: GameState, result: Result): Result = {
    val next =
      if(state.turn == PlayerTurn) state.copy(playerHP = state.playerHP - state.turnDamage)
      else                        state

    if(next.playerHP <= 0)
      result
    else if(next.bossHP <= 0) {
      println(s"Player wins:  $level - $next - $result")
      Math.min(next.manaSpent, result)
    } else
        next.turn match {
          case PlayerTurn => runPlayerTurn(level, next, result)
          case BossTurn   => runBossTurn(level, next, result)
        }
  }

  def runPlayerTurn(level: Int, state: GameState, result: Result): Result =
    nextStatesPlayerTurn(state)
      .filter(_.manaSpent < result)
      .foldLeft(result) { case (r, s) =>
        Math.min(run(level+1, s, r), r)
      }

  def runBossTurn(level: Int, state: GameState, result: Result): Result =
    nextStatesBossTurn(state)
      .foldLeft(result) { case (r, s) =>
        Math.min(run(level+1, s, r), r)
      }

  def nextStatesPlayerTurn(state: GameState): List[GameState] = {
    val effectState = applyEffects(state)
    List(
      castShield(effectState),
      castPoison(effectState),
      castRecharge(effectState),
      castMagicMissile(effectState),
      castDrain(effectState)
    ).flatten
  }

  def castMagicMissile(state: GameState): Option[GameState] =
    if(state.mana >= MagicMissileSpell.cost)
      Some(
        state.copy(
          mana      = state.mana - MagicMissileSpell.cost,
          bossHP    = state.bossHP - magicMissileDamage,
          manaSpent = state.manaSpent + MagicMissileSpell.cost,
          turn      = BossTurn))
    else
      None

  def castDrain(state: GameState): Option[GameState] =
    if(state.mana >= DrainSpell.cost)
      Some(
        state.copy(
          mana      = state.mana - DrainSpell.cost,
          playerHP  = state.playerHP + drainAmount,
          bossHP    = state.bossHP - drainAmount,
          manaSpent = state.manaSpent + DrainSpell.cost,
          turn      = BossTurn))
    else
      None

  def castShield(state: GameState): Option[GameState] =
    if(state.mana >= ShieldSpell.cost && !state.effects.contains(Shield))
      Some(
        state.copy(
          mana      = state.mana - ShieldSpell.cost,
          effects   = state.effects + ((Shield, Shield.time)),
          manaSpent = state.manaSpent + ShieldSpell.cost,
          turn      = BossTurn))
    else
      None

  def castPoison(state: GameState): Option[GameState] =
    if(state.mana >= PoisonSpell.cost && !state.effects.contains(Poison))
      Some(
        state.copy(
          mana      = state.mana - PoisonSpell.cost,
          effects   = state.effects + ((Poison, Poison.time)),
          manaSpent = state.manaSpent + PoisonSpell.cost,
          turn      = BossTurn))
    else
      None

  def castRecharge(state: GameState): Option[GameState] =
    if(state.mana >= RechargeSpell.cost && !state.effects.contains(Recharge))
      Some(
        state.copy(
          mana      = state.mana - RechargeSpell.cost,
          effects   = state.effects + ((Recharge, Recharge.time)),
          manaSpent = state.manaSpent + RechargeSpell.cost,
          turn      = BossTurn))
    else
      None

  def nextStatesBossTurn(state: GameState): List[GameState] = {
    val effectedState = applyEffects(state)
    val effectiveDamage = Math.max(effectedState.bossDamange - effectedState.playerArmor, 1)
    List(
      effectedState.copy(
        playerHP = effectedState.playerHP - effectiveDamage,
        turn     = PlayerTurn
      )
    )
  }

  def applyEffects(state: GameState): GameState = {
    val playerArmor = state.effects.get(Shield).map(_ => shieldArmor).getOrElse(0)
    val bossHits    = state.effects.get(Poison).map(_ => poisonDamage).getOrElse(0)
    val addMana     = state.effects.get(Recharge).map(_ => rechargeAmount).getOrElse(0)

    val effects =
      state.effects
        .view
        .mapValues(_ - 1)
        .filter { case (_, t) => t > 0 }
        .toMap

    state.copy(
      playerArmor = playerArmor,
      bossHP      = state.bossHP - bossHits,
      mana        = state.mana   + addMana,
      effects     = effects
    )
  }

  def initialState(playerHP: Int, playerMana: Int, bossHP: Int, bossDamage: Int): GameState =
    GameState(playerMana, 0, playerHP, bossHP, bossDamage, Map.empty[Effect, Int], 0, 0, PlayerTurn)

  val inputRegex = """Hit Points:\s+(\d+)Damage:\s+(\d+)""".r

  def parseInput(s: String): (Int, Int) = {
    val inputRegex(hp, d) = s
    (hp.toInt, d.toInt)
  }

  def readFile(f: String): GameState = {
    val (hp, d) = parseInput(io.Source.fromFile(f).getLines().flatten.mkString)
    initialState(initialPlayerHP, initialPlayerMana, hp, d)
  }

  val puzzleInput = "data/Day22.txt"
}
