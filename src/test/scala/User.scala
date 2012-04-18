package com.osinka.cqrs

// Events are a public part of domain model
sealed trait UserEvent
case class CreateUserEvent(val email: String) extends UserEvent
case class ActivateUserEvent(val key: String) extends UserEvent
case class ChangeUserEmailEvent(val newEmail: String) extends UserEvent

sealed trait BaseUser extends AggregateRoot[BaseUser] {
  def id: Int
}

object UserFactory extends AggregateFactory[BaseUser] {
  val idSequence = Iterator.continually(1)
  val keyGenerator = Iterator.continually("key")

  // public interface, returns a transaction
  def create(email: String): EventLog[NewUser] =
    Transaction.record( CreateUserEvent(email) )(handleCreate)
  
  // private mutation implementation
  private lazy val handleCreate: PartialFunction[AnyRef, NewUser] = {
    case CreateUserEvent(email) =>
      NewUser(idSequence.next, email, keyGenerator.next)
  }
  
  // all event handlers
  override val applyEvent: PartialFunction[AnyRef, BaseUser] = handleCreate
}

case class NewUser(override val id: Int, val email: String, val activationKey: String) extends BaseUser {
  // public interface, returns a transaction
  // contains pre-condition logic
  def activate(key: String): EventLog[ActiveUser] =
    if (activationKey == key)
      Transaction.record(ActivateUserEvent(key))(handleActivate)
    else
      Transaction.abort(ActivateUserEvent(key), "Wrong activation key %s, must be %s".format(key, activationKey))
    //Transaction.record( ActivateUserEvent(key) )(handleActivate) filter {_ => activationKey == key}
  
  // private mutation implementation
  private lazy val handleActivate: PartialFunction[AnyRef, ActiveUser] = {
    case ActivateUserEvent(key) =>
      ActiveUser(id, email)
  }
  
  // all event handlers
  override val applyEvent: PartialFunction[AnyRef, BaseUser] = handleActivate
}

case class ActiveUser(override val id: Int, val email: String) extends BaseUser {
  // public interface, returns a transaction
  def changeEmail(email: String) =
    Transaction.record( ChangeUserEmailEvent(email) )(handleChangeEmail)

  // private mutation implementation
  private lazy val handleChangeEmail: PartialFunction[AnyRef, ActiveUser] = {
    case ChangeUserEmailEvent(newEmail) =>
      copy(email = newEmail)
  }
  
  // all event handlers
  override val applyEvent: PartialFunction[AnyRef, BaseUser] = handleChangeEmail
}