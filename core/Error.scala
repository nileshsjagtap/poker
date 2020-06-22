package scala.com.example.poker.core

sealed abstract class Error

case object InvalidRankError extends Error
case object InvalidSuitError extends Error
case object InvalidDeckError extends Error
case object InvalidNumberOfCardsDistributedError extends Error
case object InvalidPlayerError extends Error
case object InvalidNumberOfPlayersError extends Error
case object HandsNotAssignedError extends Error