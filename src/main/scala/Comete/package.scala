package object Comete {

  type DistributionValues = Vector[Double]
  // Tipo para los valores, reales, de una distribución

  type Frequency = Vector[Double]
  // Pi.k es una frecuencia de longitud k
  // si Pi.k.length=k, 0 <= Pi.k(i) <= 1, 0<=i<=k-1
  // Pi.k.sum == 1

  type Distribution = (Frequency, DistributionValues)
  // (Pi, dv) es una distribución si Pi es una Frecuencia
  // y dv son los valores de distribución y Pi y dv
  // son de la misma longitud

  type MedidaPol = Distribution => Double

  // Hallar mínimo de la función convexa usando búsqueda ternaria
  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    // Condición de parada: si el intervalo es menor que la precisión, devolvemos el punto medio
    if (max - min < prec) {
      (min + max) / 2
    } else {
      // Dividimos el intervalo en dos puntos internos
      val mid1 = min + (max - min) / 3
      val mid2 = max - (max - min) / 3

      // Comparamos los valores de f en mid1 y mid2 para decidir en qué subintervalo continuar
      if (f(mid1) < f(mid2)) {
        min_p(f, min, mid2, prec) // Nos quedamos con el intervalo [min, mid2]
      } else {
        min_p(f, mid1, max, prec) // Nos quedamos con el intervalo [mid1, max]
      }
    }
  }

  def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
    (distribution: Distribution) => {
      val (frequencies, values) = distribution

      // Función auxiliar rho_aux que calcula la medida de polarización para un valor dado de p
      def rho_aux(p: Double): Double = {
        var sum = 0.0
        for (i <- frequencies.indices) {
          val pi = frequencies(i)
          val yi = values(i)
          sum += math.pow(pi, alpha) * math.pow(math.abs(yi - p), beta)
        }
        sum
      }

      // Usamos la función min_p para encontrar el valor de p en [0, 1] que minimiza rho_aux
      val p_final=min_p(rho_aux, 0.0, 1.0, 0.0000001)  // Precisión de 1e-6 para obtener una buena aproximación
      rho_aux(p_final)
    }
  }

  def normalizar(m: MedidaPol): MedidaPol = {
    // Calcular la polarización para el caso específico (0.5, 0, 0, 0, 0.5)
    val worstCasePolarization = m((Vector(0.5, 0.0, 0.0, 0.0, 0.5), Vector(0.0, 0.25, 0.5, 0.75, 1.0)))

    // Devolver la función normalizada
    (distribution: Distribution) => {
      val polarization = m(distribution)
      polarization / worstCasePolarization
    }
  }
}
