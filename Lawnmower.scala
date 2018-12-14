//package MowerPackage

object Lawnmower extends App {

  println("""Veuillez entrer ligne par ligne au format indiqué :
    La taille de la carte - format : x_Max y_Max
    Position de Tondeuse 1 - format : x y Orientation
    Mouvements de Tondeuse 1 - format : AAAAAA
    Position de Tondeuse 2 - format : x y Orientation
    Mouvements de Tondeuse 2 - format : AAAAAA
    """)

  //Initialisation des variables
  val sizeMap : String = scala.io.StdIn.readLine()
  val position1 : String = scala.io.StdIn.readLine()
  val movements1 : String = scala.io.StdIn.readLine()
  val position2 : String = scala.io.StdIn.readLine()
  val movements2 : String = scala.io.StdIn.readLine()

  val MowItNow1 : Mower = new Mower(sizeMap, position1, movements1, 1)
  MowItNow1.positionCalculation()

  val MowItNow2 : Mower = new Mower(sizeMap, position2, movements2, 2)
  MowItNow2.positionCalculation()
}