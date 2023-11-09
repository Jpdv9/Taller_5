import Matrices._

val m1 = matrizAlAzar(16,2)
val m2 = matrizAlAzar(16,2)

multMatriz(m1, m2)
multMatrizPar(m1, m2)

multMatrizRec(m1, m2)
multMatrizRecPar(m1, m2)

multStrassen(m1, m2)
multStrassenPar(m1, m2)

val m3 =Vector(Vector(1, 0, 0, 1), Vector(1, 1, 0, 0), Vector(0, 0, 0, 0), Vector(1, 0, 0, 1))
val m4 = Vector(Vector(1, 0, 1, 1), Vector(0, 0, 1, 1), Vector(0, 0, 0, 1), Vector(0, 0, 0, 1))

multMatriz(m3, m4)

