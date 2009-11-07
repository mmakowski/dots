package com.mmakowski.dots

import scala.util.Random

class TreeMinimaxPlayer(id: Int) extends Player(id) {
	val resignationProbability = 0.25
	val rand = new Random()
 
	def move(board: Board) = {
		if (rand.nextDouble() < resignationProbability) resign(board)
		else {
			if (board.isEmpty) board.move(board.sizeX / 2, board.sizeY / 2, id)
			else move(board, 0, 0) // TODO: wiser move
		}
	}
}
