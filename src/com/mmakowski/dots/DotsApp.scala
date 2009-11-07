package com.mmakowski.dots;

object DotsApp {
    def main(args : Array[String]) : Unit = {
    	val sizeX = args(0).toInt
    	val sizeY = args(1).toInt
    	val playerCount = args(2).toInt
    	val players = new Array[Player](playerCount)
    	for (i <- 0 until playerCount) {
    		players(i) = new RandomPlayer(i + 1)
    		players(i).start()
    	}
    	for (i <- 0 until playerCount) {
    		players(i) ! SetNextPlayer(players((i + 1) % playerCount))
    		players(i) ! SetPreviousPlayer(players(if (i == 0) playerCount - 1 else i - 1))
    	}
    	val board = new Board(sizeX, sizeY)
    	players(0) ! Move(board)
    }
 
}
