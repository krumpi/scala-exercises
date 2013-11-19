package simulations

import math.{random, min, max}

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = (for {id <- 0 until population} yield new Person(id)).toList
  persons.take(population / 100).foreach { p =>
    p.infected = true
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def move() {
      if (!dead) {
        val (newRow, newCol) = randomBelow(4) match {
          case 0 => (if (row == 0) roomRows else row - 1, col) // up
          case 1 => (if (row == roomRows) 0 else row + 1, col) // down
          case 2 => (row, if (col == 0) roomColumns else col - 1) // left
          case 3 => (row, if (col == roomColumns) 0 else col + 1) // right
        }
        // Check if it can move to the next cell
        val canMove = persons.forall { p =>
          (p.col != newCol && p.row != newRow) || (!p.dead && !p.sick)
        }
        if (canMove) {
          // Check if in the next cell is there anybody infected
          val anyInfectious = persons.exists { p =>
            p.col == newCol && p.row == newRow && (p.infected || p.immune)
          }
          if (!infected && anyInfectious) {
            infected = (randomBelow(100) + 1) < 40
          }

          row = newRow
          col = newCol
        }
        if (infected && !sick && !immune) {
          println("Get sice")
          afterDelay(6) {
            sick = true
          }
        }
        if (sick) {
          if (randomBelow(100) + 1 < 25) {
            afterDelay(14) {
              dead = true
            }
          } else {
            afterDelay(16) {
              immune = true
              sick = false
              afterDelay(2) {
                infected = false
                immune = false
                sick = false
              }
            }
          }
        }
        afterDelay(randomBelow(5) + 1) {
          move()
        }
      }
    }
  }
  persons.foreach { p =>
    afterDelay(1) {
      p.move()
    }
  }
}
