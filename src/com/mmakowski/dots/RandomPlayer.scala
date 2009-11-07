package com.mmakowski.dots

import scala.util.Random

class RandomPlayer(id: Int) extends Player(id) {
	val resignationProbability = 0.01
	val moveDistance = 2
	val rand = new Random()
 
	def move(board: Board) = {
		if (rand.nextDouble() < resignationProbability) resign(board)
		else if (board.isEmpty) makeMove(board)
		else makeMove(board, board.getCrossingsToConsider(moveDistance))  
	}
 
	private def makeMove(board: Board) {
		var retry = false
		try {
			move(board, rand.nextInt(board.sizeX), rand.nextInt(board.sizeY))
		} catch {
		  	case e => println(e); println(board); retry = true
		}
		if (retry) makeMove(board) 
	}

	private def makeMove(board: Board, crossingsToConsider: List[(Int, Int)]) {
		// println(crossingsToConsider)
		if (crossingsToConsider.isEmpty) noMoreMoves(board)
		val (x, y) = crossingsToConsider(rand.nextInt(crossingsToConsider.size))
		move(board, x, y)
	}

}
