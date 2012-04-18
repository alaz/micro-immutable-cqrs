package com.osinka

/**
 * Traits to exhibit CQRS & DDD patterns.
 *
 * Bibliography:
 * - Immutable domain based on events and writer monad:
 *   http://blog.zilverline.com/2011/02/10/towards-an-immutable-domain-model-monads-part-5/
 * - Writer monad for logging
 *   http://blog.tmorris.net/the-writer-monad-using-scala-example/
 * - CQRS with Akka and functional domain models
 *   http://debasishg.blogspot.com/2011/01/cqrs-with-akka-actors-and-functional.html
 */
package object cqrs {
  /**
   * This implicit allows to mix AggregateRoot methods and external methods
   * returning Either in one for-comprehension session.
   */
  implicit def eitherToLog[T](e: Either[String,T]) =
    e fold( TransactionAborted(Nil, _),
            Transaction(Nil, _) )
}