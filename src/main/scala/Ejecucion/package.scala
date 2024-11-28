import Benchmark._
import Opinion._

object Ejecucion extends App {

  val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
  val polSec = rho(1.2, 1.2)
  val polPar = rhoPar(1.2, 1.2)

  /*val cmp1 = compararMedidasPol(sbms, likert5, polSec, polPar)
  println(cmp1)
  */


  val i2_32768 = i2(32768)

  val evolsPar = for {
    i <- 0 until sbms.length
  } yield simEvolucion(Seq(sbms(i), sbes(i), sbts(i)),
    i2_32768, 10, polPar, confBiasUpdatePar, likert5,
    "Simulacion Paralela " ++ i.toString ++ "-" ++ sbms(i).length.toString)



  val evolsSec = for {
    i <- 0 until sbms.length
  }yield simEvolucion ( Seq ( sbms ( i ) , sbes(i) , sbts ( i ) ) ,
    i2_32768 , 10 , polSec , confBiasUpdate , likert5 ,
    "Simulacion_Secuencial_" ++ i.toString ++ "-"++ sbms(i).length.toString)
  println(evolsSec)


  val i2_65536 = i2(65536) //2^16
  val i2_16384 = i2(16384) //2^14
  val i2_262144 = i2(262144) //2^18
  val i2_1048576 = i2(1048576) //2^20


  /*compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_16384, confBiasUpdate , confBiasUpdatePar )
  compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_65536, confBiasUpdate , confBiasUpdatePar )
  compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_262144, confBiasUpdate , confBiasUpdatePar )
  compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_1048576, confBiasUpdate, confBiasUpdatePar )
*/


}