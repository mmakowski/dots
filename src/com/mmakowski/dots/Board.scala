package com.mmakowski.dots

import scala.collection.mutable.{Map, HashMap, HashSet, Stack}
import scala.math._

sealed case class Direction(symbol: String, index: Int, dx: Int, dy: Int) {
	val oppositeIndex = (index + 4) % 8 
	override def toString() = symbol 
}

object N extends Direction("N", 0, 0, -1)
object NE extends Direction("NE", 1, 1, -1)
object E extends Direction("E", 2, 1, 0)
object SE extends Direction("SE", 3, 1, 1)
object S extends Direction("S", 4, 0, 1)
object SW extends Direction("SW", 5, -1, 1)
object W extends Direction("W", 6, -1, 0)
object NW extends Direction("NW", 7, -1, -1)

// TODO: introduce lightweight board that only holds the byte array

/**
 * Objects of this class represent the current situation on the game board and encapsulate the logic to interpret this situation, i.e.
 * determine when enemy dots have been captured.
 * 
 * TODO: move the capture logic to players
 */
class Board(val sizeX: Int, val sizeY: Int) {
	/**
     * each board crossing contains three pieces of information:
     * 1. id of the player whose dot is on the crossing (bits 1-3)
     * 2. id of the player who owns this crossing (bits 4-6)
     * 3. direction of the link in the cell to the bottom-right of this crossing (bits 7-8)
	 */
	// TODO: move these to object Board
	val DOT_MASK: Byte = 0xe0.asInstanceOf[Byte]
	val OWNER_MASK = 0x1c.asInstanceOf[Byte]
	val LINK_MASK = 0x03.asInstanceOf[Byte]
	val DOT_OFFSET = 5
	val OWNER_OFFSET = 2
	val LINK_OFFSET = 0
	val NONE = 0
	val DIRECTIONS = Array(N, NE, E, SE, S, SW, W, NW)
	val CROSS_DIRECTIONS = Set(NE, SE, SW, NW)
    // link directions
	val UP = 1.asInstanceOf[Byte]
	val DOWN = 2.asInstanceOf[Byte]
    
    // the state of the board
	val board: Array[Array[Byte]] = Array.ofDim(sizeX, sizeY)
	// current score: player -> opponent -> number of dots captured
	val score: Map[Int, Map[Int, Int]] = new HashMap[Int, Map[Int, Int]]
	// a helper array that contains potential links between dots
	private val potentialPaths: Array[Array[PotentialPathElement]] = Array.ofDim(sizeX, sizeY)
	var isEmpty = true
    private var nextStructureId = 0
 
	/**
	 * return all crossings where a player can legally move and which are within given distance of some other dots. 
	 */
	def getCrossingsToConsider(distance: Int) = for {
		x <- List.range(0, sizeX)
		y <- List.range(0, sizeY) 
		if isMoveLegal(x, y)._1 
		if dotsWithin(x, y, distance) 
	} yield (x, y)
 
	def move(x: Int, y: Int, playerId: Int) = {
	    val (legal, message) = isMoveLegal(x, y)
	    if (!legal) throw new Exception(message)
		board(x)(y) = (board(x)(y) | (playerId << DOT_OFFSET)).asInstanceOf[Byte]
		addToPotentialPaths(x, y)
		isEmpty = false
	}
	
    /**
     * checks if a move is legal and returns a pair: a boolean saying whether the move is legal and a string describing
     * why the move is illegal (or null if the move is legal)
     */
 	private def isMoveLegal(x: Int, y: Int): (Boolean, String) = {
 	  	val owner = getOwnerAt(x, y)
 	  	if (owner != NONE) return (false, "(" + x + ", " + y + ") is surrounded by player " + owner)
 	  	val dot = getDotAt(x, y)
 	  	if (dot != NONE) return (false, "(" + x + ", " + y + ") already contains a dot of player " + dot)
 	  	(true, null)
 	}
  
    private def dotsWithin(x: Int, y: Int, distance: Int): Boolean = {
    	// TODO: rewrite in a more idiomatic way
    	for (dy <- -distance until distance + 1; dx <- -distance until distance + 1)
   			if (!(x + dx < 0 || x + dx >= sizeX || y + dy < 0 || y + dy >= sizeY) && getDotAt(x + dx, y + dy) != NONE) return true
    	false
    }
 	
    private def addToPotentialPaths(x: Int, y: Int) = {
    	val dirsToLink = for (dir <- DIRECTIONS if canLink(x, y, dir.dx, dir.dy)) yield dir
    	val minStructId = dirsToLink.map(d => potentialPaths(x + d.dx)(y + d.dy).structureId).foldLeft(nextStructureId)(min) 
    	if (minStructId == nextStructureId) nextStructureId = nextStructureId + 1
    	potentialPaths(x)(y) = new PotentialPathElement(x, y)
    	println((x, y))
    	for (dir <- dirsToLink) { 
    		potentialPaths(x)(y).links(dir.index) = potentialPaths(x + dir.dx)(y + dir.dy)
			potentialPaths(x + dir.dx)(y + dir.dy).links(dir.oppositeIndex) = potentialPaths(x)(y)
    		// println("added link in direction " + dir.index)
    	}
    	for (l <- cyclesClosed(x, y, dirsToLink)) {
    		val cycle = findCycle(x, y, l)
    		findAndMarkCycleInterior(x, y, cycle) match {
    			case Some(_) => materialiseLinks(x, y, cycle)
    			case None => println("no enemy dots in the cycle!")
    		}
    	}
    	potentialPaths(x)(y).updateStructureId(minStructId)
    	println("set structure id to " + minStructId)
    }

    /**
     * find the interior and return it if it contains any enemy dots. If not, return None
     */
    private def findAndMarkCycleInterior(x: Int, y: Int, cycle: List[Direction]): Option[Set[(Int, Int)]] = {
    	var interior = Set[(Int, Int)]()
    	val currentPlayer = getDotAt(x, y)
    	val border = cycleToBorder((x, y), cycle)
    	def addReachablePointsToInterior(origin: (Int, Int)): Unit = {
    		println("adding all points reachable from " + origin + " to interior")
    		val (x, y) = origin
    		interior = interior + origin
    		for {
    			dir <- DIRECTIONS
    			cx = x + dir.dx
    			cy = y + dir.dy
    			if !(border contains (cx, cy))
    			if !(interior contains (cx, cy))
    			if !(CROSS_DIRECTIONS.contains(dir) && border.contains((x + dir.dx, y)) && border.contains((x, y + dir.dy)))
    		} addReachablePointsToInterior((cx, cy))
    		println("interior = " + interior)
    	}
    	// FIXME: do this for all points in the cycle
    	val firstDirIndexToCheck = (cycle.last.oppositeIndex + 8 - 1) % 8
    	val dirsToCheck = for (i <- firstDirIndexToCheck.until(cycle.head.index, -1)) yield DIRECTIONS(i)
    	for {
    		dir <- dirsToCheck
    		currentPoint = (x + dir.dx, y + dir.dy)
    		if !(border contains currentPoint)
    		if !(interior contains currentPoint)
   			if !(CROSS_DIRECTIONS.contains(dir) && border.contains((x + dir.dx, y)) && border.contains((x, y + dir.dy)))
    	} addReachablePointsToInterior(currentPoint)
    	val playersSurrounded = interior.map((point) => getDotAt(point._1, point._2))
    	println("players surrounded: " + playersSurrounded)
    	if (!playersSurrounded.exists(!List(NONE, currentPlayer).contains(_))) return None
    	updateScores(interior, currentPlayer)
    	println("scores: " + score)
    	markInterior(interior, currentPlayer)
    	Some(interior)
    }

    /**
     * translate cycle given as the origin and a list of directions to the list of points forming the cycle
     */
    private def cycleToBorder(origin: (Int, Int), cycle: List[Direction]) = {
		var curr = origin
		var border = List(origin)
		for (dir <- cycle) {
			curr = (curr._1 + dir.dx, curr._2 + dir.dy)
			border = border ++ List(curr)
		}
		border
    }
    
    private def updateScores(interior: Set[(Int, Int)], newOwner: Int) = interior.foreach((point) => {
    	println("checking " + point)
    	val (x, y) = point
    	val dot = getDotAt(x, y)
    	if (dot != NONE) {
    		println("dot: " + dot)
    		val owner = getOwnerAt(x, y)
    		println("owner: " + owner)
    		if (owner != NONE && owner != dot) score(owner)(dot) -= 1
   			if (dot != newOwner) {
   				if (!score.keysIterator.contains(newOwner)) score(newOwner) = new HashMap[Int, Int]
   				score(newOwner)(dot) = score(newOwner).getOrElseUpdate(dot, 0) + 1
   			}
    	}
    })
    
    /**
     * set the owner for all crossings in the given list of points
     */
    private def markInterior(interior: Set[(Int, Int)], owner: Int) = interior.foreach((point) => setOwner(point._1, point._2, owner.asInstanceOf[Byte])) 
    
    /**
     * find the maximal (i.e. surrounding the largest area) cycle closed by the dot at (x, y) within the structure to which links in dirs belong
     */
    private def findCycle(x: Int, y: Int, dirs: List[Direction]): List[Direction] = {
    	println("looking for cycle from " + (x, y) + " in directions " + dirs)
    	def findCycle(startX: Int, startY: Int, x: Int, y: Int, pathDirs: Stack[Direction], pathPoints: Stack[(Int, Int)], noThroughRoad: HashSet[(Int, Int)], rotation: Int): Option[List[Direction]] = {
    		// checking that rotation is positive (i.e. we've made a turn to the right) ensures that the cycle is maximal, because we always try leftmost
    	    // branches first
    		if (x == startX && y == startY) return if (rotation > 0 && pathDirs.length > 3) Some(pathDirs.toList.reverse) else None
   			pathPoints.push((x, y))
    		// try all links from this dot (except for the one through which we just arrived) starting from the leftmost and proceeding to the right
    		val dirsToTry = for {
    			j <- 1 until 8
    			i = (pathDirs.top.oppositeIndex + j) % 8
    			dir = DIRECTIONS(i) 
                if potentialPaths(x)(y).links(i) != null 
                if !pathPoints.contains((x + dir.dx, y + dir.dy)) 
                if !noThroughRoad.contains((x + dir.dx, y + dir.dy)) 
    		} yield dir
    		for (dir <- dirsToTry) {
    			val rawRotation = dir.index - pathDirs.top.index
    			val currRotation = if (rawRotation > 4) rawRotation - 8 else if (rawRotation < -4) rawRotation + 8 else rawRotation 
    			pathDirs.push(dir)
    			val cycle = findCycle(startX, startY, x + dir.dx, y + dir.dy, pathDirs, pathPoints, noThroughRoad, rotation + currRotation)
    			if (cycle != None) return cycle
    			pathDirs.pop
    		}
    		pathPoints.pop
    		// mark this point as one that doesn't lead to the right place so that it is not attempted any more
    		noThroughRoad += ((x, y))
    		println("backing out from " + pathDirs)
    		None
    	}
    	// TODO: nicer control flow
    	for (dir <- dirs) {
	    	val initialDirs = new Stack[Direction]()
	    	initialDirs.push(dir)
	    	val initialPoints = new Stack[(Int, Int)]()
	    	val cycle = findCycle(x, y, x + dir.dx, y + dir.dy, initialDirs, initialPoints, new HashSet[(Int, Int)], 0)
	    	if (cycle != None) {
	    		println("cycle to materialise: " + cycle)
	    		return cycle.get
	    	}
    	}
    	val msg = "point " + (x, y) + " should have closed a cycle but no cycle found!"
    	println(msg)
    	throw new Exception(msg)
    }
    
    /**
     * create all links in a given cycle 
     */
    private def materialiseLinks(startX: Int, startY: Int, cycle: List[Direction]) = {
        var x = startX
        var y = startY
        for (dir <- cycle) {
        	if (dir.dx != 0 && dir.dy != 0) {
        		val (linkX, linkY, linkDir) = getCrossLinkStorageCoordsAndDirection(x, y, dir.dx, dir.dy, false)
        		setCrossLink(linkX, linkY, linkDir)
        		removePotentialOppositeCrossLink(linkX, linkY, linkDir)
        	}
        	x = x + dir.dx
        	y = y + dir.dy
        }
    }   
    
    /**
     * break a potential link that goes across the link specified by parameters.
     */
    private def removePotentialOppositeCrossLink(x: Int, y: Int, linkDir: Byte): Unit = {
    	val ((sx, sy), dir) = if (linkDir == UP) ((x, y), SE) else ((x + 1, y), SW)
    	val sourceElement = potentialPaths(sx)(sy)
    	if (sourceElement == null) return
    	val destElement = sourceElement.links(dir.index)
    	if (destElement != null) {
    		// there is actually a potential link to remove
    		sourceElement.links(dir.index) = null
    		destElement.links(dir.oppositeIndex) = null
    		nextStructureId = nextStructureId + 1
    		// it's safe to update the structure id for one of the disconnected elements. If the structure was split into two, each part will have a different id after that.
    		// if the elements remain connected through another path entire structure will get a new id 
			destElement.updateStructureId(nextStructureId)
    	}
    }
    
    /**
     * return a list of lists where each list contains links belonging to a cycle closed by new dot at (x, y) 
     */
    private def cyclesClosed(x: Int, y: Int, dirsToLink: Array[Direction]) = {
    	val pairs = for (dir <- dirsToLink) yield (potentialPaths(x + dir.dx)(y + dir.dy).structureId, dir)
    	def addToMap(map: HashMap[Int, List[Direction]], id: Int, dir: Direction) = {
    		map(id) = map.get(id) match {
    			case Some(list) => list ::: List(dir)
    			case None => List(dir)
    		}
    		map
    	}
    	val idToLinksMap = pairs.foldLeft(new HashMap[Int, List[Direction]]())((map, pair) => addToMap(map, pair._1, pair._2))
    	// check if there are dots in the same structure that are not neighbouring. Assumes there are at least 2 directions in the list  
    	def hasGap(dirs: List[Direction]): Boolean = {
    		//println("dirs to check for gaps: " + dirs)
    		for (i <- 0 until dirs.length) {
    			val curr = dirs(i)
    			val prev = dirs((dirs.length + i - 1) % dirs.length)
    			val next = dirs((i + 1) % dirs.length)
    			val requiredGap = if (curr.index % 2 == 0) 3 else 2
    			val gapBefore = if (prev.index < curr.index) curr.index - prev.index else curr.index + 8 - prev.index 
    			val gapAfter = if (next.index > curr.index) next.index - curr.index else next.index + 8 - curr.index
    			if (gapBefore >= requiredGap && gapAfter >= requiredGap) {
    				//println("found a good gap: " + curr + " " + (gapBefore, gapAfter))
    				return true
    			}
    		}
    		false
    	}
    	idToLinksMap.valuesIterator.filter(dl => dl.length > 1 && hasGap(dl)).toList
    }
      
    private def canLink(x: Int, y: Int, dx: Int, dy: Int) =
    	(dx != 0 || dy != 0) &&
    	(0 until sizeX).contains(x + dx) &&
    	(0 until sizeY).contains(y + dy) &&
    	getDotAt(x, y) == getDotAt(x + dx, y + dy) &&
    	!linkAcross(x, y, dx, dy)
    
    /**
     * return true if there is a cross-link between (x, y) and (x + dx, y + dy)
     */
    private def linkAcross(x: Int, y: Int, dx: Int, dy: Int) =
    	if (dx == 0 || dy == 0) false
    	else {
    		val (cx, cy, dir) = getCrossLinkStorageCoordsAndDirection(x, y, dx, dy, true)
    		getCrossLinkAt(cx, cy) == dir
    	}
    
    /**
     * based on origin (x, y) and offset (dx, dy) calculates a triple (cx, cy, dir) where cx, cy are the coordingates of cell which stores cross-link info
     * and dir is the direction code of the cross link. If invert is true then the dir is inverted.
     */
    private def getCrossLinkStorageCoordsAndDirection(x: Int, y: Int, dx: Int, dy: Int, invert: Boolean) = (
		if (dx < 0) x + dx else x,
		if (dy < 0) y + dy else y,
		(if (dx * dy == 1) {
		  if (invert) 1 else 2 
		} else {  
		  if (invert) 2 else 1
		}).asInstanceOf[Byte]
    ) 
      
  	def getDotAt(x: Int, y: Int) = (board(x)(y) & DOT_MASK) >> DOT_OFFSET
   
  	def getOwnerAt(x: Int, y: Int) = (board(x)(y) & OWNER_MASK) >> OWNER_OFFSET
   
  	def getCrossLinkAt(x: Int, y: Int) = (board(x)(y) & LINK_MASK) >> LINK_OFFSET

  	private def setCrossLink(x: Int, y: Int, dir: Byte) = board(x)(y) = (board(x)(y) | (dir << LINK_OFFSET)).asInstanceOf[Byte]

	private def setOwner(x: Int, y: Int, owner: Byte) = {
		board(x)(y) = (board(x)(y) & (OWNER_MASK ^ 0xff)).asInstanceOf[Byte]
		board(x)(y) = (board(x)(y) | (owner << OWNER_OFFSET)).asInstanceOf[Byte]
	}
   
	// no links: override def toString() = ("" /: board)((s, row) => s + ("" /: row)((s, cell) => s + playerSymbol(cell)) + "\n")
	override def toString() = {
  		val builder = new StringBuilder
  		for (y <- 0 until sizeY) {
  			// dots/horizontal links
  			var prevDot = NONE
  			for (x <- 0 until sizeX) {
  				val currDot = getDotAt(x, y)
  				if (x > 0) builder.append(if (currDot != NONE && prevDot == currDot) "-" else " ")
  				builder.append(playerFieldSymbol(getOwnerAt(x, y)))
  				builder.append(playerDotSymbol(currDot))
  				prevDot = currDot
  			}
  			builder.append("\n")
  			// vertical/cross links
  			if (y < sizeY - 1) {
  				builder.append(" ")
	  			for (x <- 0 until sizeX) {
	  				// vertical link
	  				val dotAbove = getDotAt(x, y)
	  				val dotBelow = getDotAt(x, y + 1)
	  				builder.append(if (dotAbove == dotBelow && dotAbove != NONE) "|" else " ")
	  				// cross link
	  				if (x < sizeX - 1) builder.append(crossLinkSymbol(getCrossLinkAt(x, y)))
	  			}
	  			builder.append("\n")
  			}
  		}
  		builder.toString
	}
   
	private def playerFieldSymbol(playerId: Int) = playerId match {
		case 0 => " "
		case _ => playerId.toString()
	}
	
	private def playerDotSymbol(playerId: Int) = playerId match {
	  case 0 => "."
	  case _ => playerId.toString()
	} 
 
  	private def crossLinkSymbol(linkType: Int) = linkType match {
  	  	case 0 => "  "
  	  	case 1 => "/ "
     	case 2 => "\\ "
  	}
    
}

class PotentialPathElement(val x: Int, val y: Int) {
	val links = new Array[PotentialPathElement](8)
	var structureId = -1 // unset
 
	def updateStructureId(newStructureId: Int): Unit = {
		if (structureId != newStructureId) {
			structureId = newStructureId
			for (elem <- links if elem != null) elem.updateStructureId(newStructureId)
		}
	}
}