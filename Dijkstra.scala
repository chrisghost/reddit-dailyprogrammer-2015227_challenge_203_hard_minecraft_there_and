import Challenge203._

object Dijkstra {
  def findPath(map: World, strt: Node, end: Node) = {

    val wMap = map.getNodes groupBy {
      e => map.get(e.pos).map{i: String =>
        i == LAVA
      }.getOrElse(false)
    }

    var nodes = wMap(false)
    var lavas = wMap(true).map(_.pos)

    println(nodes.length)
    println(lavas.length)

    val start = strt.copy(dist=0)
    var notSeenYet = nodes.filterNot(_.pos == start.pos) ++ List(start)

    while(!notSeenYet.isEmpty) {
      //val n1 = notSeenYet.sortBy(_.dist).head
      val n1::newNotSeenYet = notSeenYet.sortBy(_.dist)

      val diffs = neighbors(n1, map, nodes, lavas).map { nei =>
        if(nei.dist > n1.dist + 1)
          Node(nei.pos, n1.dist + 1, Some(n1))
        else
          nei
      }

      notSeenYet = newNotSeenYet.filterNot(e => diffs.map(_.pos).contains(e.pos) ) ++ diffs.filter(e => newNotSeenYet.map(_.pos).contains(e.pos))

      //println("NOT SEEN YET")
      //notSeenYet map println

      nodes = nodes.filterNot(e => diffs.map(_.pos).contains(e.pos)) ++ diffs.filter(e => nodes.map(_.pos).contains(e.pos))

      //println("---------------nodes")
      //nodes map println
      //println()

      //println("SteP")
      map.printWorld(notSeenYet.map(_.pos))
      //Console.readLine
    }


    var path = List[P3]()
    var n = nodes.find{_.pos == end.pos}

    while(!n.isEmpty)
    {
      path = n.get.pos :: path
      n = n.get.prev
      //println("NEST : ", n)
    }

    path
  }

  def neighbors(n: Node, map: World, nodes: List[Node], lavas: List[P3]) : List[Node] = {
    val poss = List(
      P3( 1,  0,  0),
      P3(-1,  0,  0),
      P3( 0,  1,  0),
      P3( 0, -1,  0),
      P3( 0,  0,  1),
      P3( 0,  0, -1)
    ).map ( _+n.pos )
    .filter(map.isInBounds(_))
    .filter(map.isTraversable(_))

    nodes
      .filter { n => poss.contains(n.pos) }
      //.filterNot { e => lavas.contains(e.pos) }
      .filterNot { e => map.get(e.pos+P3(0,0,1)).map(e => e == AIR || e == LAVA).getOrElse(false) }
  }
}

case class P3(x: Int, y: Int, z: Int) {
  def +(p:P3) = P3(x+p.x, y+p.y, z+p.z)
  def -(p:P3) = P3(x+p.x, y+p.y, z+p.z)
  def dst(p:P3) = Math.sqrt(
    Math.pow(x-p.x,2) +
    Math.pow(y-p.y,2) +
    Math.pow(z-p.z,2)
  )
}
case class Node(pos: P3, dist: Int = Int.MaxValue, prev: Option[Node] = None)
{

}
