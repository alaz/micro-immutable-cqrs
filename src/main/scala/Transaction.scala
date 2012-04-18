package com.osinka.cqrs

/**
 * Event log
 *
 * A history of events implemented as a Writer monad
 *
 * @see http://blog.tmorris.net/the-writer-monad-using-scala-example/
 */
sealed trait EventLog[+A] { self =>
  def map[B](f: A => B): EventLog[B]
  def flatMap[B](f: A => EventLog[B]): EventLog[B]
  def filter(p: A => Boolean): EventLog[A] = this
  def foreach[U](f: A => U) {}

  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): EventLog[B] = self.filter(p).map(f)
    def flatMap[B](f: A => EventLog[B]): EventLog[B] = self.filter(p).flatMap(f)
    def foreach[U](f: A => U) { self.filter(p).foreach(f) }
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  def isDefined: Boolean = false
  def isEmpty: Boolean = true

  def history: List[AnyRef]
  def result: A

  def toEither: Either[String, A] = Right(result)

  protected[cqrs] def prepend(events: List[AnyRef]): EventLog[A]
}

/**
 * A transaction
 *
 * Transaction object contains the resulting object and a history of events
 * the led to this object.
 */
final case class Transaction[+A](override val history: List[AnyRef], override val result: A) extends EventLog[A] {
  override def map[B](f: A => B): EventLog[B] = new Transaction(history, f(result))

  override def flatMap[B](f: A => EventLog[B]): EventLog[B] = f(result).prepend(history)

  override def filter(p: A => Boolean): EventLog[A] =
    if (p(result))
      this
    else
      TransactionAborted(history, "%s does not pass filter".format(result))

  override def foreach[U](f: A => U) {
    f(result)
  }

  override def isDefined: Boolean = true
  override def isEmpty: Boolean = false

  override def prepend(events: List[AnyRef]) = copy(history = events ::: history)
}

/**
 * Aborted transaction
 */
final case class TransactionAborted(override val history: List[AnyRef], val message: String) extends EventLog[Nothing] {
  override def map[B](f: Nothing => B): EventLog[B] = this

  override def flatMap[B](f: Nothing => EventLog[B]): EventLog[B] = this

  override def prepend(events: List[AnyRef]) = copy(history = events ::: history)

  override def result: Nothing = throw new IllegalAccessException("Aborted: %s" format message)

  override def toEither: Either[String,Nothing] = Left(message)
}

object Transaction {
  def init[T](obj: T): EventLog[T] = Transaction(Nil, obj)

  /**
   * Record an event.
   *
   * Event handler is a {{PartialFunction}}
   */
  def record[E <: AnyRef, R](event: E)(pf: PartialFunction[E, R]): EventLog[R] =
    pf.lift(event) map {x =>
       Transaction(event :: Nil, x)
     } getOrElse { abort(event, "handler %s does not support event %s".format(pf, event)) }

  /**
   * Abort a transaction.
   *
   * The event that led to an abort.
   */
  def abort[E <: AnyRef](event: E, msg: String) = TransactionAborted(event :: Nil, msg)
}