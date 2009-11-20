package com.mmakowski.dots

import scala.actors._
import scala.actors.Actor._

sealed trait PlayerRequest
final case class Exit() extends PlayerRequest
final case class Move(board: Board) extends PlayerRequest
final case class SetNextPlayer(player: Player) extends PlayerRequest
final case class SetPreviousPlayer(player: Player) extends PlayerRequest

abstract class Player(val id: Int) extends Actor {
	var nextPlayer: Player = null
	var previousPlayer: Player = null
   
	def act() {
		loop {
			receive {
			  	case Exit() => exit
			  	case Move(board) => move(board)
			  	case SetNextPlayer(player) => if (player == this) iWon() else nextPlayer = player
			  	case SetPreviousPlayer(player) => previousPlayer = player
			  	case m: Any => println("unexpected message: " + m)
			}
		}
	}
 
	def move(board: Board): Unit
 
	protected def move(board: Board, x: Int, y: Int) = {
		board.move(x, y, id)
		println("after player " + id + " move:\n" + board)
		nextPlayer ! Move(board)
	}
 
	protected def resign(board: Board) = {
		println("player " + id + " resigns")
		nextPlayer ! SetPreviousPlayer(previousPlayer)
		previousPlayer ! SetNextPlayer(nextPlayer)
		nextPlayer ! Move(board)
		exit
	} 
 
	protected def noMoreMoves(board: Board) = {
		println("there are no more valid moves:\n" + board)
		println("final score: " + board.score)
		var currNext = nextPlayer
		while (currNext != self) {
			val next = currNext.nextPlayer
			currNext ! Exit()
			currNext = next
		}
		exit
	}
 
	private def iWon() = {
		println("player " + id + " won!")
		exit
	}
}
