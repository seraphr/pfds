package jp.seraphr.pfds

/* 課題3.7 */
object ExplicitMin {
  def apply(heap: Heap) = new ExplicitMin {
    override val h: heap.type = heap
  }
}

abstract class ExplicitMin extends Heap {
  val h: jp.seraphr.pfds.Heap
  override val elem: h.elem.type = h.elem

  sealed trait Heap
  case object E extends Heap
  case class NE(min: elem.T, heap: h.Heap) extends Heap
  private val UnderlyingEmpty = h.empty
  override val empty: Heap = E
  override val insert: (elem.T, Heap) => Heap = {
    case (v, E)                             => NE(v, h.empty)
    case (v, NE(m, heap)) if elem.leq(v, m) => NE(v, h.insert(m, heap))
    case (v, NE(m, heap))                   => NE(m, h.insert(v, heap))
  }
  override val deleteMin: (Heap) => Heap = {
    case E                      => throw new RuntimeException("empty heap")
    case NE(_, UnderlyingEmpty) => E
    case NE(_, heap)            => NE(h.findMin(heap), h.deleteMin(heap))
  }
  override val merge: (Heap, Heap) => Heap = {
    case (E, h2) => h2
    case (h1, E) => h1
    case (NE(m1, h1), NE(m2, h2)) =>
      val (m, nm) = if (elem.leq(m1, m2)) (m1, m2) else (m2, m1)
      NE(m, h.insert(nm, h.merge(h1, h2)))
  }
  override val fromList: (List[elem.T]) => Heap = {
    case Nil      => E
    case v :: Nil => NE(v, UnderlyingEmpty)
    case xs =>
      val y :: ys = xs.sortWith(elem.lt)
      NE(y, h.fromList(ys))
  }
  override val isEmpty: (Heap) => Boolean = _ == E
  override val findMin: (Heap) => elem.T = {
    case E        => throw new RuntimeException("empty heap")
    case NE(m, _) => m
  }
}
