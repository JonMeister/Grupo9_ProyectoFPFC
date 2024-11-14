import Comete._

package object Opinion {

  type SpecificBelief= Vector[Double]
  // Si b:SpecificBelief , para cada i en Int ,
  // b[ i ] es un numero entre 0 y 1
  // que indica cuanto cree el agente i
  // en la veracidad de la proposicion p
  // El numero de agentes es b.length
  // Si existe i: b(i) <0 o b(i) > 1 b esta mal definida.
  // Para i en Int A, b(i) no tiene sentido

  /**
   * type GenericBelief = Int => SpecificBeliefConf
   *
   * Esta type todavía no se usa, SpecificBeliefConf no está definido
   */
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
    Vector.tabulate(nags)((i:Int) => b)
  }

  /**
   * @param alpha Parámetro que controla la sensibilidad a la masa de cada grupo (típicamente entre 1.0 y 2.0)
   * @param beta Parámetro que controla la sensibilidad a la distancia entre creencias (típicamente entre 1.0 y 2.0)
   * @return Una función que calcula la polarización normalizada para una creencia específica y valores de distribución
   */
  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    /**
     * Función interna que calcula la polarización para una creencia específica y valores de distribución.
     *
     * @param specificBelief Vector de creencias de los agentes (valores entre 0 y 1)
     * @param distributionValues Vector de valores discretos para la distribución
     * @return Valor de polarización normalizado entre 0 (mínima) y 1 (máxima)
     */

    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      // Inicializamos el vector de frecuencias con ceros
      val emptyFrequencies = Vector.fill(distributionValues.length)(0.0)

      // Calculamos las frecuencias para cada valor de la distribución
      val rawFrequencies = specificBelief.foldLeft(emptyFrequencies) { (accumulatedFreq, agentBelief) =>
        // Encontramos el valor más cercano en la distribución para la creencia del agente
        val closestValueIndex = distributionValues.indices.minBy { index =>
          math.abs(distributionValues(index) - agentBelief)
        }

        // Incrementamos la frecuencia para ese valor
        accumulatedFreq.updated(closestValueIndex, accumulatedFreq(closestValueIndex) + 1.0)
      }

      // Normalizamos las frecuencias dividiendo por el número total de agentes
      val totalAgents = specificBelief.length
      val normalizedFrequencies = rawFrequencies.map(frequency => frequency / totalAgents)

      // Creamos la distribución como un par de (frecuencias normalizadas, valores)
      val beliefDistribution = (normalizedFrequencies, distributionValues)

      // Creamos la medida de Comete con los parámetros alpha y beta
      val baseCometeMeasure = Comete.rhoCMT_Gen(alpha, beta)

      // Normalizamos la medida respecto al caso peor (máxima polarización)
      val normalizedCometeMeasure = Comete.normalizar(baseCometeMeasure)

      // Calculamos y devolvemos la polarización normalizada
      normalizedCometeMeasure(beliefDistribution)
    }
  }

  val sb_ext=allExtremeBelief(100)
  val sb_cons = consensusBelief(0.2)(100)
  val sb_unif = uniformBelief(100)
  val sb_triple = allTripleBelief (100)
  val sb_midly = midlyBelief(100)
}
