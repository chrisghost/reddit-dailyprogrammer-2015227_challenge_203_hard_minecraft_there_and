import scala.collection.mutable.ArrayBuffer

object Challenge203 extends App {
  val m = Map(100, 100, 10)
  m.generate

  m.printMap

  while(m.applyGravity) {
    m.printMap
    Console.readLine
  }
}

case class Map(w: Int, h: Int, d: Int) {
  val AIR = "."
  val DIRT = "▒"
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

  def isInBounds(x: Int, y: Int, z: Int) = x >= 0 && x < w && y >= 0 && y < h && z >= 0 && z < d
  def isAir(x: Int, y: Int, z: Int) = isInBounds(x, y, z) && _map(x)(y)(z) == AIR
  def applyGravity = {
    (for {
      x <- Range(0, w)
      y <- Range(0, h)
      z <- Range(0, d)
    } yield {
      _map(x)(y)(z) match {
        case e @ (LAVA | SAND) if(isAir(x, y, z+1)) => {
          _map(x)(y)(z) = AIR
          _map(x)(y)(z+1) = e
          true
        }
        case _ => false
      }
    }).toList.contains(true)
  }

  def printMap {
    _map.map { slice =>
      Range(0, d).map { z =>
        print(Console.BLACK)
        Range(0, h).map { y =>
          slice(y)(z) match {
            case AIR => print(Console.CYAN_B)
            case DIRT => print(Console.GREEN_B)
            case SAND => print(Console.YELLOW_B)
            case LAVA => print(Console.RED_B)
            case GOAL => print(Console.BLUE_B)
          }
          print(slice(y)(z))
        }
        print(Console.RESET)
        println()
      }
      println("----------------------------")
    }
  }
}


