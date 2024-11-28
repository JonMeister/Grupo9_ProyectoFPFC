/*
Enlace al repositorio: https://github.com/JonMeister/Grupo9_ProyectoFPFC.git

Paquetes entregados:

Comete: Contiene las funciones min_p, rhoCMT_Gen y normalizar.

Opinion: Contiene las funciones rho, showWeightedGraph, confBiasUpdate, simulate. También incluye las versiones paralelas (por paralelismo de datos) rhoPar y confBiasUpdatePar.

pruebas.sc: Contiene las pruebas de todas las funciones implementadas separadas en 4 partes:

1. Paquete Comete.
2. Elementos estáticos del modelo.
3. Elementos dinámicos del modelo.
4. Paralelización de funciones
*/






val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val polSec = rho(1.2, 1.2)
val polPar = rhoPar(1.2, 1.2)


// Compara los tiempos entre rho y rhoPar
val cmp1 = compararMedidasPol(sbms, likert5, polSec, polPar)
println(cmp1)


val i2_32768 = i2(32768) //2^15
val i2_65536 = i2(65536) //2^16
val i2_16384 = i2(16384) //2^14
val i2_262144 = i2(262144) //2^18
val i2_1048576 = i2(1048576) //2^20

// Compara los tiempos entre confBiasUpdate y confBiasUpdatePar
compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_16384, confBiasUpdate , confBiasUpdatePar )
compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_65536, confBiasUpdate , confBiasUpdatePar )
compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_262144, confBiasUpdate , confBiasUpdatePar )
compararFuncionesAct ( sbms.take ( sbms.length/ 2 ) ,i2_1048576, confBiasUpdate, confBiasUpdatePar )


// Simulaciones y generación de gráficas

val evolsSec = for {
    i <- 0 until sbms.length
  }yield simEvolucion ( Seq ( sbms ( i ) , sbes(i) , sbts ( i ) ) ,
    i2_32768 , 10 , polSec , confBiasUpdate , likert5 ,
    "Simulacion_Secuencial_" ++ i.toString ++ "-"++ sbms(i).length.toString)

val evolsPar = for {
    i <- 0 until sbms.length
  } yield simEvolucion(Seq(sbms(i), sbes(i), sbts(i)),
    i2_32768, 10, polPar, confBiasUpdatePar, likert5,
    "Simulacion Paralela " ++ i.toString ++ "-" ++ sbms(i).length.toString)


