import Challenge203._

object RecFinder {
  def findPath(world: World, start: P3, end: P3): (List[P3], Option[MapArray]) = {

    def recSolve(world: World, pos: P3, end: P3, wasHere: List[P3]): Option[RNode] = {
      val (nPos, nWasHere) =
        if(nmap.get(pos+P3(0,0,1)).map(_ == AIR).getOrElse(false))
          (pos+P3(0,0,1), wasHere ::: List(pos, pos+P3(0,0,1)))
        else
          (pos, wasHere)

      nmap.printWorld(nWasHere)

      if(nPos == end) return Some(RNode(nPos, None, Some(world._map)))

      neighbors(nPos, world, nWasHere).sortBy(_.dst(end))
        .map { nei =>
          val nmap = world.copy(_map=world.mine(nei))
          (nmap.isTraversable(nei), canGoBack(nWasHere :+ nei, nmap)) match { 
            case (true, true) => {

              recSolve(nmap, nei, end, nWasHere:+nei).map { found =>
                return Some(RNode(nPos,Some(found)))
              }
            }
            case _ =>
          }
        }
      return None
    }

    def canGoBack(path: List[P3], world: World) = {
      path.reverse.sliding(2, 1).map {
        case (a :: b) => {
          if(b.head.z != a.z && world.isAir(a+P3(0,0,1))
            || (world.under(a).map(_ == AIR).getOrElse(false)
              && world.under(b.head).map(_ == AIR).getOrElse(false)
              )
            )

            false
          else
            true
        }
        case _ => true
      }.reduce((a, b) => a && b)

    }

    def unroll(n: RNode) : (List[P3], Option[MapArray]) = {
      n match {
        case RNode(pos, Some(previous), _) => {
          val r = unroll(previous)
          (r._1 ::: List(pos), r._2)
        }
        case RNode(pos, None, Some(map)) => (List(pos), Some(map))
      }
    }

    recSolve(world, start, end, List(start)).map { result =>
      unroll(result)
    }.getOrElse((List(), None))
  }

  def neighbors(p: P3, map: World, wasHere: List[P3]) : List[P3] = {
    val poss = List(
      P3( 1,  0,  0),
      P3(-1,  0,  0),
      P3( 0,  1,  0),
      P3( 0, -1,  0),
      P3( 0,  0,  1),
      P3( 0,  0, -1)
    ).map ( _+p )
    .filter(map.isInBounds(_))
    .filter(map.isTraversable(_))

    poss.filterNot { e =>
      //Check for LAVA under position
      map.get(e+P3(0,0,1)).map(e => e == LAVA).getOrElse(false)
    }.filterNot { e =>
      //Check for AIR under position
      map.get(e+P3(0,0,1)) match {
        case Some(AIR) => map.get(e+P3(0,0,2)).map(e => e == AIR || e == LAVA).getOrElse(false)
        case _ => false
      }
    }.filterNot{wasHere.contains(_)}
  }
  case class RNode(pos: P3, prev: Option[RNode] = None, map: Option[MapArray] = None)
  {

  }
}
