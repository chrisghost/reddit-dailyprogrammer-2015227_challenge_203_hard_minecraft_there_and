import scala.collection.mutable.ArrayBuffer

object Challenge203 extends App {
  val m = Map(20, 20, 10)
  m.generate

  m.printMap
}

case class Map(w: Int, h: Int, d: Int) {
  val AIR = "."
  val DIRT = "▓"
  val SAND = "░"
  val LAVA = "≈"
  val GOAL = "★"
  val possiblesBlocks = List(AIR, DIRT, SAND, LAVA)

  val _map = scala.collection.mutable.ArrayBuffer.fill(w, h, d)(AIR)

  def rndBlock = possiblesBlocks(scala.util.Random.nextInt(possiblesBlocks.length))
  def generate {
    for {
      x <- Range(0, w)
      y <- Range(0, h)
      z <- Range(1, d)
    } yield {
      _map(x)(y)(z) = rndBlock
    }

    _map(0)(0)(1) = DIRT
    _map(w-1)(h-1)(d-1) = GOAL
  }

  def printMap {
    _map.map { slice =>
      Range(0, d).map { z =>
        Range(0, h).map { y =>
          print( slice(y)(z) )
        }
        println()
      }
      println("----------------------------")
    }
  }
}


