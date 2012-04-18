package com.osinka.cqrs

import org.scalatest.FunSpec
import org.scalatest.matchers.MustMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class cqrsSpec extends FunSpec with MustMatchers {
  val id = UserFactory.idSequence.next
  val email = "a@email.com"
  val actKey = UserFactory.keyGenerator.next

  describe("Transaction") {
    import Transaction.{record, abort}

    it("records") {
      val r = record('Event) { case 'Event => true}
      r must equal( Transaction('Event :: Nil, true))
      r must be('defined)
      r.toEither must be('right)
    }
    it("aborts explicitly") {
      abort('Event, "message") must equal( TransactionAborted('Event :: Nil, "message") )
    }
    it("aborts by PF") {
      val r = record('Event)(Undefined)
      r must be('empty)
      r.toEither must be('left)
    }
    it("commits") {
      record('Event)({ case 'Event => true}) must equal( Transaction('Event :: Nil, true) )
      record('Event)(Undefined) must be('empty)
    }
    it("supports for comprehension") {
      (for {_ <- record('Event1) {case 'Event1 => true}
            r <- record('Event2) {case 'Event2 => 34} }
       yield r) must equal( Transaction('Event1 :: 'Event2 :: Nil, 34))

      (for {_ <- record('Event1) {case 'Event1 => true}
            r <- record('Event2) {case 'Event2 => 34} if r < 30}
       yield r) must equal( TransactionAborted('Event1 :: 'Event2 :: Nil, "%s does not pass filter" format 34))
    }
  }

  val createEvent = CreateUserEvent(email)
  def activateEvent(key: String) = ActivateUserEvent(key)
  val activateEvent: ActivateUserEvent = activateEvent(actKey)

  val newU = NewUser(id, email, actKey)
  val activeU = ActiveUser(id, email)

  describe("Factory") {
    it("creates a user") {
      UserFactory.create(email) must equal( Transaction(createEvent :: Nil, newU) )
    }
    it("loads from a history of events") {
      UserFactory.loadFrom(createEvent :: activateEvent :: Nil) must equal(activeU)
    }
    it("breaks when loading a wrong sequence of events") {
      evaluating { UserFactory.loadFrom(activateEvent :: Nil) } must produce[MatchError]
    }
  }
  describe("AggregateRoot") {
    it("records events") {
      UserFactory.create(email).flatMap{ _.activate(actKey) } must equal( Transaction(createEvent :: activateEvent :: Nil, activeU) )
    }
    it("loads history of events") {
      val transaction = UserFactory.create(email) flatMap { _.activate(actKey) }
      transaction must be('defined)
      transaction.result.loadFrom(ChangeUserEmailEvent("new@email.com") :: Nil) must equal(ActiveUser(id, "new@email.com"))
    }
    it("aborts a transaction") {
      UserFactory.create(email).flatMap{ _.activate("wrong"+actKey) } must be('empty)
    }
  }
  describe("Either workflow") {
    def activate(e: Either[String,String]) =
      for {u <- Transaction.init(newU)
           key <- e
           activatedU <- u.activate(key)}
      yield activatedU

    it("succeeds") {
      val r = activate(Right(actKey))
      r must be('defined)
      r.toEither must equal(Right(activeU))
    }
    it("fails b/c of Either") {
      val r = activate(Left("failed to get key"))
      r must be('empty)
      r.toEither must be('left)
    }
    it("fails b/c of domain model") {
      val r = activate(Right("wrong key"))
      r must be('empty)
      r.toEither must be('left)
    }
  }

  object Undefined extends PartialFunction[Any, Nothing] {
    override def isDefinedAt(x: Any): Boolean = false
    override def apply(x: Any): Nothing = throw new MatchError("Undefined does not support argument %s" format x)
  }
}
