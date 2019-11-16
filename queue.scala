trait Queue[T] {
  /** Checks if the queue is empty */
  def isEmpty: Boolean

  /** Returns a new Queue with the given element added at the end */
  def enQueue(element: T): Queue[T]

  /** Returns a tuple containing the first element of the queue and a new Queue without this element */
  def deQueue(): (Option[T], Queue[T])

  /** Returns the first element of the Queue */
  def head: Option[T]

  /** Returns all the elements of the Queue except the first one */
  def tail: Queue[T]
}

object Queue {
  /** An empty FIFO queue of type T */
  def empty[T]: Queue[T] = new FifoQueue[T]()
}

/**
 * Immutable FIFO queue.
 *
 * @constructor Creates a queue with the given elements.
 * @param args The queue's elements
 */
class FifoQueue[T](args: T*) extends Queue[T] {
  /** The collection of elements contained in the queue */
  private val _elements: List[T] = args.toList

  override def isEmpty: Boolean = _elements.isEmpty

  override def enQueue(element: T): FifoQueue[T] = {
    new FifoQueue[T]((_elements :+ element): _*)
  }

  override def deQueue: (Option[T], FifoQueue[T]) = {
    (head, tail)
  }

  override def head: Option[T] = {
    _elements.headOption
  }

  override def tail: FifoQueue[T] = {
    new FifoQueue[T](_elements.tail: _*)
  }
}
