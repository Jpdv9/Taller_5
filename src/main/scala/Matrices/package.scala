import common._

import scala.collection.parallel.immutable.ParVector
import scala.util.Random


package object Matrices {

  val random = new Random()

  type Matriz = Vector[Vector[Int]]


  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    //Crea un matriz de enteros cuadrada de long x long,
    //con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long){random.nextInt(vals)}
    v
  }

  def transpuesta(m:Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(1, 1)((i, j) => (i * j))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int ={
    (v1 zip v2).map({case (i, j) => (i * j)}).sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: Vector[Int]): Int ={
    // A ser usada en el punto 1.5
    (v1 zip v2).map({case (i, j) => (i * j)}).sum
  }


  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length)

    val n = m1.length

    // Función para calcular un elemento de la matriz resultante
    def compute(i: Int, j: Int): Int = {
      val fila = m1(i)
      val columna = transpuesta(m2)(j)
      prodPunto(fila, columna)
    }

    // Calcular la matriz resultante de forma secuencial
    val resultado = Vector.tabulate(n, n) { (i, j) =>
      compute(i, j)
    }

    resultado
  }


  //Ejercicio 1.1.2

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length)

    val n = m1.length

    // Función para calcular un elemento de la matriz resultante
    def compute(i: Int, j: Int): Int = {
      val fila = m1(i)
      val columna = transpuesta(m2)(j)
      prodPunto(fila, columna)
    }

    // Crear una lista de tareas para los cálculos
    val tareas = (0 until n).flatMap { i =>
      (0 until n).map { j =>
        task {
          compute(i, j)
        }
      }
    }

    // Obtener los resultados de las tareas paralelas
    val resultadoPar = tareas.map(_.join())

    // Organizar los resultados en una matriz
    val resultadoMatriz = resultadoPar.grouped(n).toVector.map(_.toVector)

    resultadoMatriz
  }

  //Ejercicio 1.2.1

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    // Crear una nueva matriz de dimensiones l x l a partir de la matriz m
    val submatriz = Vector.tabulate(l, l) { (row, col) =>
      m(i + row)(j + col)
    }

    submatriz
  }

  //Ejercicio 1.2.2

  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length, "Las matrices deben tener la misma dimensión")

    val n = m1.length

    // Crear una nueva matriz resultante sumando los elementos de m1 y m2
    val matrizSuma = Vector.tabulate(n, n) { (i, j) =>
      m1(i)(j) + m2(i)(j)
    }

    matrizSuma
  }

  //Ejercicio 1.2.3

  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length, "Las matrices deben tener la misma dimensión")

    val n = m1.length

    // Caso base: Si la dimensión es 1x1, simplemente multiplicamos los elementos
    if (n == 1) {
      val resultado = Vector.tabulate(1, 1) { (i, j) =>
        m1(0)(0) * m2(0)(0)
      }
      resultado
    } else {
      val l = n / 2

      // Dividir las matrices en submatrices
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Realizar cálculos recursivos
      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      // Combinar los resultados en una sola matriz
      val resultado = Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l) c12(i)(j - l)
        else if (j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }

      resultado
    }
  }

  //Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length, "Las matrices deben tener la misma dimensión")

    val n = m1.length

    // Umbral a partir del cual se realiza la paralelización
    val umbral = 64 // Puedes ajustar este umbral según tus necesidades

    // Caso base: Si la dimensión es menor o igual al umbral, se realiza el cálculo secuencial
    if (n <= umbral) {
      multMatrizRec(m1, m2)
    } else {
      val l = n / 2

      // Dividir las matrices en submatrices
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Realizar cálculos recursivos en paralelo
      val c11 = task(sumMatriz(multMatrizRecPar(a11, b11), multMatrizRecPar(a12, b21)))
      val c12 = task(sumMatriz(multMatrizRecPar(a11, b12), multMatrizRecPar(a12, b22)))
      val c21 = task(sumMatriz(multMatrizRecPar(a21, b11), multMatrizRecPar(a22, b21)))
      val c22 = task(sumMatriz(multMatrizRecPar(a21, b12), multMatrizRecPar(a22, b22)))

      // Combinar los resultados en una sola matriz
      val resultado = Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11.join()(i)(j)
        else if (i < l) c12.join()(i)(j - l)
        else if (j < l) c21.join()(i - l)(j)
        else c22.join()(i - l)(j - l)
      }

      resultado
    }
  }

  //Ejercicio 1.3.1

  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length)

    val n = m1.length

    // Crear una matriz para el resultado
    val resultado = Vector.tabulate(n, n) { (i, j) =>
      m1(i)(j) - m2(i)(j)
    }

    resultado
  }

  //Ejercicio 1.3.2

  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length, "Las matrices deben tener la misma dimensión")

    val n = m1.length

    // Umbral a partir del cual se realiza la multiplicación de matrices de forma directa
    val umbral = 64 // Puedes ajustar este umbral según tus necesidades

    if (n <= umbral) {
      // Caso base: Multiplicación de matrices de forma directa
      multMatrizRec(m1, m2)
    } else {
      val l = n / 2

      // Dividir las matrices en submatrices
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Calcular los productos necesarios
      val p1 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p2 = multStrassen(sumMatriz(a21, a22), b11)
      val p3 = multStrassen(a11, restaMatriz(b12, b22))
      val p4 = multStrassen(a22, restaMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a12), b22)
      val p6 = multStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12))
      val p7 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))

      // Calcular las submatrices del resultado
      val c11 = restaMatriz(sumMatriz(sumMatriz(p1, p4), p5), p7)
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = restaMatriz(sumMatriz(p1, p3), sumMatriz(p2, p4))

      // Combinar los resultados en una sola matriz
      val resultado = Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l) c12(i)(j - l)
        else if (j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }

      resultado
    }
  }


  //Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    require(m1.length == m2.length, "Las matrices deben tener la misma dimensión")

    val n = m1.length

    // Umbral a partir del cual se realiza la paralelización
    val umbral = 64 // Puedes ajustar este umbral según tus necesidades

    if (n <= umbral) {
      // Caso base: Multiplicación de matrices de forma directa
      multMatrizRec(m1, m2)
    } else {
      val l = n / 2

      // Dividir las matrices en submatrices
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Realizar cálculos recursivos en paralelo
      val p1 = task(multStrassenPar(sumMatriz(a11, a22), sumMatriz(b11, b22)))
      val p2 = task(multStrassenPar(sumMatriz(a21, a22), b11))
      val p3 = task(multStrassenPar(a11, restaMatriz(b12, b22)))
      val p4 = task(multStrassenPar(a22, restaMatriz(b21, b11)))
      val p5 = task(multStrassenPar(sumMatriz(a11, a12), b22))
      val p6 = task(multStrassenPar(restaMatriz(a21, a11), sumMatriz(b11, b12)))
      val p7 = task(multStrassenPar(restaMatriz(a12, a22), sumMatriz(b21, b22)))


      // Obtener resultados utilizando join
      val p1Result = p1.join()
      val p2Result = p2.join()
      val p3Result = p3.join()
      val p4Result = p4.join()
      val p5Result = p5.join()
      val p6Result = p6.join()
      val p7Result = p7.join()

      // Calcular submatrices resultantes
      val c11 = restaMatriz(sumMatriz(sumMatriz(p1Result, p4Result), p5Result), p7Result)
      val c12 = sumMatriz(p3Result, p5Result)
      val c21 = sumMatriz(p2Result, p4Result)
      val c22 = restaMatriz(sumMatriz(p1Result, p3Result), sumMatriz(p2Result, p4Result))

      // Componer la matriz resultante
      val resultado = Vector.tabulate(n, n) { (i, j) =>
        if (i < l && j < l) c11(i)(j)
        else if (i < l) c12(i)(j - l)
        else if (j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      }

      resultado
    }
  }



}
