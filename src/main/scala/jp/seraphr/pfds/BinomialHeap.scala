package jp.seraphr.pfds

object BinomialHeap {
  def apply(aElem: PfdsOrdered) = new BinomialHeap {
    val elem: aElem.type = aElem
  }
}

/**
 */
abstract class BinomialHeap extends Heap {
  case class Tree(rank: Int, v: elem.T, children: List[Tree])

  override type Heap = List[Tree]

  private val link: (Tree, Tree) => Tree = {
    case (t1 @ Tree(r, x1, c1), t2 @ Tree(_, x2, c2)) =>
      if (elem.leq(x1, x2)) Tree(r + 1, x1, t2 :: c1)
      else Tree(r + 1, x2, t1 :: c2)
  }

  private val rank: Tree => Int = _.rank
  private val root: Tree => elem.T = _.v
  private val insTree: (Tree, Heap) => Heap = {
    case (t, Nil) => List(t)
    case (t, ts @ t_ :: ts_) =>
      if (rank(t) < rank(t_)) t :: ts else insTree(link(t, t_), ts_)
  }

  override val insert: (elem.T, Heap) => Heap = (x, ts) => insTree(Tree(0, x, Nil), ts)
  override val merge: (Heap, Heap) => Heap = {
    case (ts1, Nil) => ts1
    case (Nil, ts2) => ts2
    case (ts1 @ t1 :: ts1_, ts2 @ t2 :: ts2_) =>
      if (rank(t1) < rank(t2)) t1 :: merge(ts1_, ts2)
      else if (rank(t2) < rank(t1)) t2 :: merge(ts1, ts2_)
      else insTree(link(t1, t2), merge(ts1_, ts2_))
  }

  private val removeMinTree: Heap => (Tree, Heap) = {
    case Nil      => throw new RuntimeException("empty heap")
    case t :: Nil => (t, Nil)
    case t :: ts =>
      val (t_, ts_) = removeMinTree(ts)
      if (elem.leq(root(t), root(t_))) (t, ts) else (t_, t :: ts_)
  }

  override val findMin: (Heap) => elem.T = removeMinTree(_)._1.v
  /* 課題 3.5 */
  val findMin2: Heap => elem.T = {
    case Nil      => throw new RuntimeException("empty heap")
    case t :: Nil => t.v
    case Tree(_, m1, _) :: ts =>
      val m2 = findMin2(ts)
      if (elem.leq(m1, m2)) m1 else m2
  }

  override val deleteMin: (Heap) => Heap = { ts =>
    val (Tree(_, _, ts1), ts2) = removeMinTree(ts)
    merge(ts1.reverse, ts2)
  }
  override val empty: Heap = Nil
  override val isEmpty: (Heap) => Boolean = _.isEmpty

  private val single: elem.T => Heap = insert(_, empty)
  override val fromList: (List[elem.T]) => Heap = _.map(single).fold(empty)(merge)
}
