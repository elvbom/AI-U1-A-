carReady = function(roads, car, packages) {
  
  hroads = roads$hroads # horizontal road costs
  vroads = roads$vroads # vertical road costs
  
  currX = car$x # current x position
  currY = car$y # current y position
  load = car$load # current package load
  mem = car$mem # list with info saved between turns
  nextMove = 0 # what the car will do next move (2 down, 4 left, 6 right, 8 up, 5 stay still)
  
# (3) A matrix containing information about the packages. This contains five columns and a row for each package. 
  # The first two columns give x and y coordinates about where the package should be picked up from. 
  # The next two columns give x and y coordinates about where the package should be delivered to. 
  # The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).

  toGo = 0
  offset = 0
  
  if (load == 0) { # no package onboard
    # will find the nearest package with A*
    pack = closestPack(car, packages)[1]
    toGo = which(packages[,5]==0)[1]
  } else {
    toGo = load  
    offset = 2
  }
  
  if (currX < packages[toGo, 1+offset]) {
    nextMove = 6 # right
  } else if (currX > packages[toGo, 1+offset]) {
    nextMove = 4 # left
  } else if (currY < packages[toGo, 2+offset]) {
    nextMove = 8 # up
  } else if (currY > packages[toGo, 2+offset]) {
    nextMove = 2 # down
  } else {
    nextMove = 5
  }
  
  car$nextMove = nextMove
  car$mem = list()
  return(car)
}

aStar = function(car, goal, roads) { # A* algorithm
}

heuristic = function(currX, currY, goal) { # Calculate heuristic function
} 

closestPack = function(car, packages) {
  # calculates which unpicked package is closest to the car, using A*
  packLeft = which(packages[, 5] == 0) # finds unpicked packages
  origin = array(car$x, car$y)
  print(car$x)
  print(car$y)
  pack = packLeft
  #return(pack)
}

inFrontier = function(currNode, nodes) { # is this node in the frontier?
}

neighbours = function(node, grid) { # find neighbouring nodes
}

visited = function(currNode, nodes) { # have we been here before?
}

cost = function(currX, currY, neighbours, roads) { # find cheapest way forward
}

goal = function(currNode, goal) { # are we at goal?
}