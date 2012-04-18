package com.osinka.cqrs

/**
 * Aggregate root with Event Sourcing.
 */
trait AggregateRoot[AR <: AggregateRoot[AR]] extends EventSource[AR]

/**
 * Factory of aggregates
 */
trait AggregateFactory[AR <: AggregateRoot[AR]] extends EventSource[AR]