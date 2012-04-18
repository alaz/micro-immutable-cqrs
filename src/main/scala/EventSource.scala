package com.osinka.cqrs

/**
 * Event source trait
 *
 * It should be able to apply events to self. As a reward, it becomes possible to
 * make modifications from an event history.
 *
 * @see http://blog.zilverline.com/2011/02/10/towards-an-immutable-domain-model-monads-part-5/
 * @see https://github.com/erikrozendaal/scala-event-sourcing-example
 */
trait EventSource[AR <: EventSource[AR]] {
  def loadFrom(history: Iterable[AnyRef]): AR =
    (applyEvent(history.head) /: history.tail) {_ applyEvent _}

  // helper method in case of the only return type.
  protected def record(ev: AnyRef) = Transaction.record(ev)(applyEvent)

  protected def abort(ev: AnyRef, msg: String) = Transaction.abort(ev, msg)

  protected def applyEvent: PartialFunction[AnyRef, AR]
}
