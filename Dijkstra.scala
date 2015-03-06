object Dijkstra {
  def findPath(map: Map, strt: Node, end: Node) = {

    val nodes = map.getNodes
    val start = strt.copy(dist=0)
    val notSeenYet = scala.collection.mutable.ListBuffer(nodes)

    while(!notSeenYet.isEmpty) {
      val n1 = notSeenYet.toList.sortBy(_.dist).head
      notSeenYet-=n1
    }
  }
}

case class P3(x: Int, y: Int, z: Int) {
  def +(p:P3) = P3(x+p.x, y+p.y, z+p.z)
  def -(p:P3) = P3(x+p.x, y+p.y, z+p.z)
}
case class Node(pos: P3, dist: Int = Int.MaxValue, prev: Option[Node] = None)
{

}
