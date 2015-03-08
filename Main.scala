import scala.collection.mutable.ArrayBuffer

object Challenge203 extends App {
  type MapArray = ArrayBuffer[ArrayBuffer[ArrayBuffer[String]]]
  val AIR = "-"
  val DIRT = "▒"
  val SAND = "░"
  val LAVA = "≈"
  val GOAL = "★"

  val size = P3(3, 40, 20)

  val m = World(size)
  var e= ""
  println("y to continue , regenerate otherwise")
  do
  {
    m.generate

    m.printWorld
  }
  while(Console.readLine != "y")

  val nmap = m.copy(_map=m.applyGravity)
  nmap.printWorld

    val path : (List[P3], Option[MapArray])= RecFinder.findPath(nmap, P3(0,0,0), P3(size.x-1,size.y-1,size.z-1))
    println(path)

    Range(0, path._1.length).map { idx =>
      path._2.map { finalMap =>
        nmap.printWorld(path._1.take(idx), finalMap)
      }
    }

  case class World(
    size: P3,
    _map: MapArray = scala.collection.mutable.ArrayBuffer.fill(size.x, size.y, size.z)(AIR))
  {

    val possiblesBlocks = List(AIR, DIRT, SAND, LAVA) ++ List(DIRT, SAND) ++ List(DIRT, SAND) ++ List(DIRT, SAND) ++ List(DIRT, SAND) ++ List(DIRT, SAND) ++ List(DIRT, SAND)++ List(DIRT, SAND)

    val P3(w, h, d) = size

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

    def isAir(p: P3): Boolean = isAir(p, _map)
    def isAir(p: P3, buf: MapArray): Boolean = isAir(p.x, p.y, p.z, buf)
    def isAir(x: Int, y: Int, z: Int, buf: MapArray = _map) = isInBounds(x, y, z) && buf(x)(y)(z) == AIR
    def isTraversable(p: P3, buf: MapArray = _map) =  isAir(p, buf) || get(p).map(e => e == SAND || e == DIRT || e == GOAL).getOrElse(false)
    def isMineable(p: P3): Boolean = get(p).map(isMineable(_)).getOrElse(false)
    def isMineable(s: String): Boolean = s == SAND || s == DIRT || s == GOAL
    def applyGravity: MapArray = {
      applyGravity(_map)
    }

    def under(p: P3) = get(p+P3(0,0,1))
    def is(p: P3, a: String) = get(p).map(_ == a).getOrElse(false)

    def applyGravity(buf: MapArray): MapArray= {
      def applyGravityHelper(buf: MapArray) = {
        val modified = (for {
          x <- Range(0, w)
          y <- Range(0, h)
          z <- Range(0, d)
        } yield {
          buf(x)(y)(z) match {
            case e @ (LAVA | SAND) if(isAir(x, y, z+1, buf)) => {
              buf(x)(y)(z) = AIR
              buf(x)(y)(z+1) = e
              true
            }
            case _ => false
          }
        }).toList.contains(true)
        (modified, buf)
      }
      applyGravityHelper(buf) match {
        case (true, nmap) => applyGravity(nmap)
        case (false, nmap) => nmap
      }
    }

    def mine(p: P3): MapArray = {
      var map = _map.map(_.map(_.clone))
      while(isMineable(map(p.x)(p.y)(p.z))) {
        map(p.x)(p.y)(p.z) = AIR
        map = applyGravity(map)
      }
      map
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
    def printWorld(path: List[P3], buf : MapArray = _map) {
      var sbuf = new scala.collection.mutable.StringBuilder("")
      Range(0, d).map { z =>
        Range(0, w).map { x =>
          Range(0, h).map { y =>
            sbuf ++= (Console.BLACK)

              buf(x)(y)(z) match {
                case AIR => sbuf ++= (Console.CYAN_B)
                case DIRT => sbuf ++= (Console.GREEN_B)
                case SAND => sbuf ++= (Console.YELLOW_B)
                case LAVA => sbuf ++= (Console.RED_B)
                case GOAL => sbuf ++= (Console.BLUE_B)
              }
            path.find(_ == P3(x, y, z)).map(e => sbuf ++= (Console.WHITE))
            sbuf ++= (buf(x)(y)(z))
          }
          sbuf ++= (Console.RESET)
        sbuf ++= "\t";
        }
        sbuf ++= "\n";
      }

      println(sbuf)
      Thread.sleep(100)
    }
  }


}
