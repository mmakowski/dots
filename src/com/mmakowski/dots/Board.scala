package com.mmakowski.dots

import scala.collection.mutable.{Map, HashMap, Stack}

sealed case class Direction(val index: Int, val dx: Int, val dy: Int) {
	val oppositeIndex = (index + 4) % 8 
}

case object N extends Direction(0, 0, -1)
case object NE extends Direction(1, 1, -1)
case object E extends Direction(2, 1, 0)
case object SE extends Direction(3, 1, 1)
case object S extends Direction(4, 0, 1)
case object SW extends Direction(5, -1, 1)
case object W extends Direction(6, -1, 0)
case object NW extends Direction(7, -1, -1)

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
    // link directions
    val UP = 1.asInstanceOf[Byte]
    val DOWN = 2.asInstanceOf[Byte]
    
    // the state of the board
    val board = new Array[Array[Byte]](sizeX, sizeY)
    // a helper array that contains potential links between dots
	val potentialPaths = new Array[Array[PotentialPathElement]](sizeX, sizeY)
	var isEmpty = true
    var nextStructureId = 0
 
	/**
	 * return all crossings where a player can legally move and which are within given distance of some other dots. 
	 */
	def getCrossingsToConsider(distance: Int) = 
    	for (x <- List.range(0, sizeX); y <- List.range(0, sizeY) if isMoveLegal(x, y)._1 && dotsWithin(x, y, distance)) yield (x, y)
 
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
    	for (dy <- -distance until distance + 1) {
    		for (dx <- -distance until distance + 1)
    			if (!(x + dx < 0 || x + dx >= sizeX || y + dy < 0 || y + dy >= sizeY) && getDotAt(x + dx, y + dy) != NONE) return true
    	}
    	false
    }
    
    def addToPotentialPaths(x: Int, y: Int) = {
    	val dirsToLink = for (dir <- DIRECTIONS if canLink(x, y, dir.dx, dir.dy)) yield dir
    	val minStructId = dirsToLink.map(d => potentialPaths(x + d.dx)(y + d.dy).structureId).foldLeft(nextStructureId)(Math.min) 
    	if (minStructId == nextStructureId) nextStructureId = nextStructureId + 1
    	potentialPaths(x)(y) = new PotentialPathElement(x, y)
    	println((x, y))
    	for (dir <- dirsToLink) { 
    		potentialPaths(x)(y).links(dir.index) = potentialPaths(x + dir.dx)(y + dir.dy)
			potentialPaths(x + dir.dx)(y + dir.dy).links(dir.oppositeIndex) = potentialPaths(x)(y)
    		println("added link in direction " + dir.index)
    	}
    	// TODO: the below materialises all links in the structures! Instead, it should
    	// 1. find the non-empty cycles
    	// 2. materialise the links that form these cycles
    	for (l <- cyclesClosed(x, y, dirsToLink)) materialiseLinks(x, y, findCycle(x, y, l))
    	potentialPaths(x)(y).updateStructureId(minStructId)
    	println("set structure id to " + minStructId)
    }

    // TODO: temporary
    def findCycle(x: Int, y: Int, dirs: List[Direction]) = {
    	def findCycle(startX: Int, startY: Int, x: Int, y: Int, pathDirs: Stack[Direction], pathPoints: Stack[(Int, Int)]): List[Direction] = {
    		if (x == startX && y == startY) return pathDirs.toList
    		val dirsToTry = for (j <- 1 until 8; i = (pathDirs.top.oppositeIndex + j) % 8; dir = DIRECTIONS(i) 
                                 if potentialPaths(x)(y).links(i) != null && !pathPoints.contains((x + dir.dx, y + dir.dy))) 
                            	yield dir
   			pathPoints.push((x, y))
    		for (dir <- dirsToTry) {
    			pathDirs.push(dir)
    			val cycle = findCycle(startX, startY, x + dir.dx, y + dir.dy, pathDirs, pathPoints)
    			if (cycle != null) return cycle
    			pathDirs.pop
    		}
    		pathPoints.pop
    		null
    	}
    	val initialDirs = new Stack[Direction]()
    	initialDirs.push(dirs(0))
    	val initialPoints = new Stack[(Int, Int)]()
    	val cycle = findCycle(x, y, x + dirs(0).dx, y + dirs(0).dy, initialDirs, initialPoints)
    	println("cycle to materialise: " + cycle)
    	cycle
    }
    
    /**
     * create all links in a given cycle 
     */
    def materialiseLinks(startX: Int, startY: Int, cycle: List[Direction]) = {
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
    def removePotentialOppositeCrossLink(x: Int, y: Int, linkDir: Byte): Unit = {
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
    def cyclesClosed(x: Int, y: Int, dirsToLink: Array[Direction]) = {
    	val pairs = for (dir <- dirsToLink) yield (potentialPaths(x + dir.dx)(y + dir.dy).structureId, dir)
    	def addToMap(map: HashMap[Int, List[Direction]], id: Int, dir: Direction) = {
    		if (map.contains(id)) map(id) = map(id) ::: List(dir)
    		else map(id) = List(dir)
    		map
    	}
    	val idToLinksMap = pairs.foldLeft(new HashMap[Int, List[Direction]]())((map, pair) => addToMap(map, pair._1, pair._2))
    	// FIXME: hasGap doesn't work; N E has gap of 2 but can link directly
    	def hasGap(dirs: List[Direction]) = 
    		dirs.foldLeft((List[Int](), dirs(0)))((listAndPrev, dir) => (listAndPrev._1 ::: List(dir.index - listAndPrev._2.index), dir))._1.exists(diff => diff > 1 && diff < 7)
    	idToLinksMap.values.filter(hasGap(_)).toList
    }
      
    def canLink(x: Int, y: Int, dx: Int, dy: Int) =
    	(dx != 0 || dy != 0) &&
    	(0 until sizeX).contains(x + dx) &&
    	(0 until sizeY).contains(y + dy) &&
    	getDotAt(x, y) == getDotAt(x + dx, y + dy) &&
    	!linkAcross(x, y, dx, dy)
    
    /**
     * return true if there is a cross-link between (x, y) and (x + dx, y + dy)
     */
    def linkAcross(x: Int, y: Int, dx: Int, dy: Int) =
    	if (dx == 0 || dy == 0) false
    	else {
    		val (cx, cy, dir) = getCrossLinkStorageCoordsAndDirection(x, y, dx, dy, true)
    		getCrossLinkAt(cx, cy) == dir
    	}
    
    /**
     * based on origin (x, y) and offset (dx, dy) calculates a triple (cx, cy, dir) where cx, cy are the coordingates of cell which stores cross-link info
     * and dir is the direction code of the cross link. If invert is true then the dir is inverted.
     */
    def getCrossLinkStorageCoordsAndDirection(x: Int, y: Int, dx: Int, dy: Int, invert: Boolean) = (
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

  	def setCrossLink(x: Int, y: Int, dir: Byte) = board(x)(y) = (board(x)(y) | (dir << LINK_OFFSET)).asInstanceOf[Byte]

   
	// no links: override def toString() = ("" /: board)((s, row) => s + ("" /: row)((s, cell) => s + playerSymbol(cell)) + "\n")
	override def toString() = {
  		val builder = new StringBuilder
  		for (y <- 0 until sizeY) {
  			// dots
  			var prevDot = NONE
  			for (x <- 0 until sizeX) {
  				val currDot = getDotAt(x, y)
  				if (x > 0) builder.append(if (currDot != NONE && prevDot == currDot) "-" else " ")
  				builder.append(playerSymbol(currDot))
  				prevDot = currDot
  			}
  			builder.append("\n")
  			// vertical/cross links
  			if (y < sizeY - 1) {
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
   
	def playerSymbol(playerId: Int) = playerId match {
	  case 0 => "."
	  case _ => playerId.toString()
	} 
 
  	def crossLinkSymbol(linkType: Int) = linkType match {
  	  	case 0 => " "
  	  	case 1 => "/"
     	case 2 => "\\"
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