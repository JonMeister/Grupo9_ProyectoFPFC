import Benchmark._
import Opinion._

object Ejecucion extends App {

  val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
  val polSec = rho(1.2, 1.2)
  val polPar = rhoPar(1.2, 1.2)

  val cmp1 = compararMedidasPol(sbms, likert5, polSec, polPar)
  println(cmp1)
  println(cmp1.map(t=>t._6))

  val i2_32768 = i2(32768)

  compararFuncionesAct(
    sbms.take(sbms.length / 2),
    i2_32768,
    confBiasUpdate,
    confBiasUpdatePar
  )
}