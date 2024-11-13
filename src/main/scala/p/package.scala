import Comete._

package object p {

  type SpecificBelief= Vector[Double]
  // Si b:SpecificBelief , para cada i en Int ,
  // b[ i ] es un numero entre 0 y 1
  // que indica cuanto cree el agente i
  // en la veracidad de la proposicion p
  // El numero de agentes es b.length
  // Si existe i: b(i) <0 o b(i) > 1 b esta mal definida.
  // Para i en Int A, b(i) no tiene sentido

  type GenericBelief = Int => SpecificBeliefConf
  // si gb:GenericBelief , entonces gb(n) = b
  // tal que b: SpecificBelief
  // es el tipo de las funciones generadoras de creencias

  type AgentsPolMeasure =
    ( SpecificBelief , DistributionValues) => Double
  // Si rho:AgentsPolMeasure y sb: SpecificBelief y
  // d:DistributionValues ,
  // rho(sb ,d) es la polarizacion de los agentes
  // de acuerdo a esa medida

  //Build uniform belief state
  def uniformBelief(nags:Int):SpecificBelief = {
    Vector.tabulate(nags)((i:Int) => (i+1).toDouble/nags.toDouble)
  }

  // Builds midly polarized belief state, in which
  // half of agents has belief decreasing from 0.25, and
  // half has belief increasing from 0.75, all by the given step.
  def midlyBelief(nags:Int): SpecificBelief = {
    val middle = nags/2
    Vector.tabulate(nags)((i:Int) =>
      if (i < middle) math.max(0.25-0.01*(middle-i-1),0)
      else math.min(0.75-0.01*(middle-i),1))
  }

  // Builds extreme polarized belief state, in which half
  // of the agents has belief 0, and half has belief 1.
  def allExtremeBelief(nags:Int): SpecificBelief = {
    val middle = nags/2
    Vector.tabulate(nags)((i:Int) =>
      if (i<middle) 0.0 else  1.0)
  }

  // Builds three pole belief state , in which each
  // one third of the agents has belief 0, one third has belief 0.5,
  // and one third has belief 1.
  def allTripleBelief(nags:Int): SpecificBelief = {
    val oneThird= nags/3
    val twoThird = (nags/3)*2
    Vector.tabulate(nags)((i:Int) =>
      if (i < oneThird) 0.0
      else if (i >= twoThird) 1.0
        else 0.5)
  }

  // Builds consensus belief state, in which each
  // all agents have same belief.
  def consensusBelief(b:Double)(nags: Int ): SpecificBelief = {
    Vector.tabulate(nags)(( i : Int) => b)
  }
}
