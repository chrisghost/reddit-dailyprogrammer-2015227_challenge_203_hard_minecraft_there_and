import scala.collection.mutable.ArrayBuffer

object Challenge203 extends App {
  val size = P3(1, 10, 10)

  val m = World(size)
  m.generate

  m.printWorld

  while(m.applyGravity) {
  }
  m.printWorld

  val path = Dijkstra.findPath(m, Node(P3(0,0,0)), Node(P3(size.x-1,size.y-1,size.z-1)))
  println(path)
  m.printWorld(path)
}

case class World(size: P3) {
  val AIR = "-"
  val DIRT = "▒"
  val SAND = "░"
  val LAVA = "≈"
  val GOAL = "★"
  val possiblesBlocks = List(AIR, DIRT, SAND, LAVA) ++ List(DIRT, SAND) ++ List(DIRT, SAND) ++ List(DIRT, SAND)
  //val possiblesBlocks = List(DIRT, SAND, LAVA) ++ List(DIRT, SAND)

  val P3(w, h, d) = size

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

  def get(p: P3) = if(isInBounds(p.x, p.y, p.z)) Some(_map(p.x)(p.y)(p.z)) else None

  def isInBounds(p: P3): Boolean = isInBounds(p.x, p.y, p.z)
  def isInBounds(x: Int, y: Int, z: Int) = x >= 0 && x < w && y >= 0 && y < h && z >= 0 && z < d

  def isAir(p: P3): Boolean = isAir(p.x, p.y, p.z)
  def isAir(x: Int, y: Int, z: Int) = isInBounds(x, y, z) && _map(x)(y)(z) == AIR
  def isTraversable(p: P3) =  isAir(p) || get(p).map(e => e == SAND || e == DIRT || e == GOAL).getOrElse(false)
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

  def getNodes: List[Node] = {
    (for {
      x <- Range(0, w)
      y <- Range(0, h)
      z <- Range(1, d)
    } yield {
      Node(P3(x,y,z))
    }).toList
  }

  def printWorld: Unit= printWorld(List())
  def printWorld(path: List[P3]) {
    var sbuf = new scala.collection.mutable.StringBuilder("")
    Range(0, w).map { x =>
      Range(0, d).map { z =>
        Range(0, h).map { y =>
          sbuf ++= (Console.BLACK)

          path.find(_ == P3(x, y, z)).map(e => sbuf ++= (Console.WHITE))
          //.getOrElse {
            _map(x)(y)(z) match {
              case AIR => sbuf ++= (Console.CYAN_B)
              case DIRT => sbuf ++= (Console.GREEN_B)
              case SAND => sbuf ++= (Console.YELLOW_B)
              case LAVA => sbuf ++= (Console.RED_B)
              case GOAL => sbuf ++= (Console.BLUE_B)
            }
          //}
          sbuf ++= (_map(x)(y)(z))
        }
        sbuf ++= (Console.RESET)
        sbuf ++= "\n";
      }
      //println("----------------------------")
    }

    println(sbuf)
    Thread.sleep(50)
  }
}


