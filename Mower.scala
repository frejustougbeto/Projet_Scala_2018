class Mower(var _sizeMap : String, var _position : String, var _instruction : String, nb : Int) {

  val directions = Array("N", "E", "W", "S") //les 4 directions possibles
  val actions = Array("D", "G", "A") //les 3 actions possibles

  //On vérifie que les variables sont bien déclarés lors de la création de la classe
  def sizeMap : String = sizeMap_(_sizeMap)
  def position : String = position_(_position)
  def instruction : String = instruction_(_instruction)

  //bornes de la carte
  val xMin : Int = 0
  val yMin : Int = 0
  var xMax : Int = _sizeMap.split(" ")(0).toInt
  var yMax : Int = _sizeMap.split(" ")(1).toInt

  var thePosition : Array[Int] = Array(0,0) //Array(Colonne,Ligne) <=> Array(x, y)
  var theDirection : String = ""

  //fonction permettant de vérifier que la taille de la carte est bien renseignée lors de l'initialisation de la classe
  def sizeMap_(sizeMap1 : String) : String = {
    if (sizeMap1.split(" ")(0).toInt > 0) { //on vérifie que la ligne > 0
      if (sizeMap1.split(" ")(1).toInt > 0) { //on vérifie que la colonne > 0
        if (sizeMap1.split(" ").length == 2) { //on vérifie que l'on a 2 nombres uniquement
          _sizeMap = sizeMap1
        } else {
          //on a plus de 2 nombres
          println("Erreur !\nVous avez rentré plus de 2 nombres.\nLe programme va s'arrêter.")
          sys.exit(0) //arrêt du programme
        }
      } else {
        //le numéro de la colonne est inférieur à 0
        println("Erreur !\nLa colonne est inférieure à 0.\nLe programme va s'arrêter.")
        sys.exit(0) //arrêt du programme
      }
    } else {
      //le numéro de la ligne est inférieur à 0
      println("Erreur!\nLa ligne est inférieure à 0.\nLe programme va s'arrêter.")
      sys.exit(0) //arrêt du programme
    }
    _sizeMap
  }

  //fonction permettant de vérifier que la position est bien renseignée lors de l'initialisation de la classe
  def position_(position1 : String) = {
    if (position1.split(" ")(0).toInt > 0) { //on vérifie que le premier nombre est supérieur à 0
      if (position1.split(" ")(0).toInt <= xMax) { //on vérifie que la tondeuse n'est pas en dehors de la carte
        if (position1.split(" ")(1).toInt > 0) { //on vérifie que le second nombre est supérieur à 0
          if (position1.split(" ")(0).toInt <= yMax) { //on vérifie que la tondeuse n'est pas en dehors de la carte
            if (directions.contains(position1.split(" ")(2)) == true) { //on vérifie que l'on a les bonnes lettres
              if (position1.split(" ").length == 3) { //on vérifie que l'on a que 3 trucs
                _position = position1
              } else {
                //il y a trop de caractères
                println("Erreur !\nVous avez entré trop de caractères.\nLe programme va s'arrêter.")
                sys.exit(0) //le programme s'arrête
              }
            } else {
              //la lettre n'est pas une direction
              println(position1.split(" ")(2))
              //la lettre n'est pas une direction
              println("Erreur !\nCertaines lettres écrites ne sont pas des directions.\nLe programme va s'arrêter.")
              sys.exit(0) //le programme s'arrête
            }
          } else {
            //la position de la tondeuse est en dehors de la carte
            println("Erreur !\nLa position initiale de la tondeuse est en dehors de la carte.\nLe programme va s'arrêter.")
            sys.exit(0) //le programme s'arrête
          }
        } else {
          //le second chiffre est négatif
          println("Erreur !\nLa position initiale de la tondeuse est en dehors de la carte.\nLe programme va s'arrêter.")
          sys.exit(0) //le programme s'arrête
        }
      } else {
        //la position de la tondeuse est en dehors de la carte
        println("Erreur !\nLa position initiale de la tondeuse est en dehors de la carte.\nLe programme va s'arrêter.")
        sys.exit(0) //le programme s'arrête
      }
    } else {
      //le premier chiffre est négatif
      println("Erreur !\nLa position initiale de la tondeuse est en dehors de la carte.\nLe programme va s'arrêter.")
      sys.exit(0) //le programme s'arrête
    }
    _position
  }

  //fonction permettant de vérifier que les instructions sont bien écrit lors de l'initialisation de la classe
  def instruction_(instruction1 : String) = {
    instruction1.split("").foreach(orientation => {
      if (actions.contains(orientation) == false) {
        println("Erreur !\nCertaines lettres ne sont pas des actions.\nLe programme va s'arrêter")
        sys.exit(0) //le programme s'arrête
      } else {
        _instruction = instruction1
      }
    }
    )
    _instruction
  }

  //fonction permettant de déplacer la tondeuse & l'orienter en lisant les instructions
  def positionCalculation(): Unit = {
    //val steps : Array[String] = _instruction.split("") //les différentes instructions ########
    val steps : Array[String] = instruction.split("") //les différentes instructions
    //var lastPosition : Array[Int] = Array(_position.split(" ")(0).toInt,_position.split(" ")(1).toInt) //la dernière position
    var lastPosition : Array[Int] = Array(position.split(" ")(0).toInt, position.split(" ")(1).toInt) //la dernière position
    //var lastOrientation = _position.split(" ")(2) //la dernière direction
    var lastOrientation = position.split(" ")(2) //la dernière direction
    for (step <- steps) {
      step match {
        case "D" => lastOrientation = turnRight(lastOrientation)
        case "G" => lastOrientation = turnLeft(lastOrientation)
        case "A" => lastPosition = move(lastPosition, lastOrientation)
      }
    }
    println(f"Tondeuse $nb : ${lastPosition(0)} ${lastPosition(1)} $lastOrientation") //tondeuse X : position
  }

  //fonction permettant d'actualiser la position de la tondeuse
  def move(position : Array[Int], orientation : String) : Array[Int] = {
    if (orientation == "N") { //la tondeuse est orientée vers le nord
      if (position(1) < yMax) { //on vérifie que la tondeuse n'est pas à la frontière nord
        thePosition(0) = position(0)
        thePosition(1) = position(1) + 1 //on monte d'une ligne
      } else {
        thePosition(0) = position(0)
        thePosition(1) = position(1)
      }
    } else if (orientation == "W") { //la tondeuse est orientée vers l'ouest
      if (position(0) > 0) { //on vérifie que la tondeuse n'est pas à la frontière ouest
        thePosition(0) = position(0) - 1 //on bouge d'une colonne vers la gauche
        thePosition(1) = position(1)
      } else {
        thePosition(0) = position(0)
        thePosition(1) = position(1)
      }
    } else if (orientation == "S") { //la tondeuse est orientée vers le sud
      if (position(1) > 0) { //on vérifie que la tondeuse n'est pas à la frontière sud
        thePosition(0) = position(0)
        thePosition(1) = position(1) - 1 //on descend d'une ligne
      } else {
        thePosition(0) = position(0)
        thePosition(1) = position(1)
      }
    } else if (orientation == "E") { //la tondeuse est orientée vers le est
      if (position(0) < xMax) { //on vérifie que la tondeuse n'est pas à la frontière est
        thePosition(0) = position(0) + 1 //on bouge d'une colonne vers la droite
        thePosition(1) = position(1)
      } else {
        thePosition(0) = position(0)
        thePosition(1) = position(1)
      }
    }
    thePosition
  }

  //fonction pour indiquer la direction de la tondeuse lorsque l'on tourne à gauche
  def turnLeft(orientation : String) : String = {
    if (orientation == "N") {
      theDirection = "W"
    } else if (orientation == "E") {
      theDirection = "N"
    } else if (orientation == "S") {
      theDirection = "E"
    } else if (orientation == "W") {
      theDirection = "S"
    }
    theDirection
  }

  //fonction pour indiquer la direction lorsque l'on tourne à gauche
  def turnRight(orientation : String) : String = {
    if (orientation == "N") {
      theDirection = "E"
    } else if (orientation == "W") {
      theDirection = "N"
    } else if (orientation == "S") {
      theDirection = "W"
    } else if (orientation == "E") {
      theDirection = "S"
    }
    theDirection
  }
}