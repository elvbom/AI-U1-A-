carReady = function(roads, car, packages) {
  
  hroads = roads$hroads # horizontal road costs
  vroads = roads$vroads # vertical road costs
  
  x = car$x # current x position
  y = car$y # current y position
  load = car$load # current package load
  mem = car$mem # list with info saved between turns
  nextMove = 0 # what the car will do next move (2 down, 4 left, 6 right, 8 up, 5 stay still)
  
  toGo = 0
  offset = 0
  
  if (load == 0) { # no package onboard
    toGo = closestPack(car, packages)
  } else {
    toGo = aStar(roads, car, packages) # toGo = index i packages för paket som ska levereras
    offset = 2
  }
  
  # FIXME
  if (x < packages[toGo, 1+offset]) { # denna är utan a* bara vad som är närmast utan att tänka på kostnad
    nextMove = 6 # right
  } else if (x > packages[toGo, 1+offset]) {
    nextMove = 4 # left
  } else if (y < packages[toGo, 2+offset]) {
    nextMove = 8 # up
  } else if (y > packages[toGo, 2+offset]) {
    nextMove = 2 # down
  } else {
    nextMove = 5
  }
  
  car$nextMove = nextMove
  return(car)
}

aStar = function(roads, car, packages) { # A* algorithm
  vroads = roads$vroads
  hroads = roads$hroads
  grid = ncol(roads$vroads)
  x = car$x
  y = car$y
  
  load = car$load
  goal = list(x = packages[load, 3], y = packages[load, 4])
  
  nodes = list() # make graph with all nodes
  
  for (i in 1:grid) { # give nodes initial values
    nodes[[i]] = list()
    for (j in 1:grid) {
      nodes[[i]][[j]] = list(x = i, y = j, h = heuristic(i, j, goal), cost = 10^6, totalCost = 10^6, frontier = FALSE, visited = FALSE)
    }
  }
  
  frontier = 1 # the node we're on, to be explored
  nodes[[x]][[y]]$cost = nodes[[x]][[y]]$h # it's our starter node, so its cost = its heuristic
  currX = x
  currY = y
  
  while (frontier > 0) { # FIXME bara så den inte blir evig?
    nodes[[currX]][[currY]]$visited = TRUE # we have visited the node we're on
    nodes[[currX]][[currY]]$frontier = FALSE
    frontier = frontier - 1

    nb = neighbours(grid, currX, currY, vroads, hroads) # find its neighbours
    for (i in 1:length(nb)) {
      nx = nb[[i]]$x
      ny = nb[[i]]$y
      nr = nb[[i]]$r 

      if (nodes[[nx]][[ny]]$visited == TRUE || nodes[[nx]][[ny]]$frontier == TRUE) { # can we find a new cheaper way to visited/frontier node?
        if ((node$h + nr) < node$cost) {
          nodes[[nx]][[ny]]$cost = node$h + nb$r
        }
      } else { # find travel cost to node
        nodes[[nx]][[ny]]$cost = node$h + nb$r
        nodes[[nx]][[ny]]$frontier = TRUE
        frontier = frontier + 1
      }
      
      
      
      # nb[i] = här behöver jag fixa så att grannen får ngn sorts kostnad tsms med koordinater så att den går att jämföra
      # så här behöver jag kolla vilken granne som har lägst kostnad, och gå mot den
      # jag behöver också lägga in i totalCost så allt räknas ihop
    }
  }
    
    # behöver uppdatera vilken nod vi står på sen genom att se vart minsta konstaden är och hoppa dit
    
      # else{
      #   # calculate known cost of getting to that node, given current traffic
      #   cost = nodes[[current$x]][[current$y]]$realCost + TravelCost(current, neighbourNode, roads)
      #   # if the node isn't yet in the frontier, or there is a copy of it in the frontier but this one has lower cost
      #   if ((!IsInFrontier(neighbourNode, nodes)) || (cost < nodes[[neighbourNode$x]][[neighbourNode$y]]$realCost)) {  
      #     # add it to the frontier and set its attributes
      #     nodes[[neighbourNode$x]][[neighbourNode$y]]$parent = c(current$x, current$y)
      #     nodes[[neighbourNode$x]][[neighbourNode$y]]$realCost = cost
      #     nodes[[neighbourNode$x]][[neighbourNode$y]]$f = cost + neighbourNode$h
      #     
      #     # if that node wasn't already in the frontier, add it, and update the size of the frontier
      #     if (!IsInFrontier(neighbourNode, nodes)) {
      #       nodes[[neighbourNode$x]][[neighbourNode$y]]$inFrontier = TRUE 
      #       frontierSize =  frontierSize + 1
      #     }
      #   }
      #   i = i + 1
      # }
  

  return(car$load)
}

heuristic = function(x, y, goal) { # Calculate heuristic function, number of steps between start and goal
  return(abs(goal$x - x) + abs(goal$y - y))
}

neighbours = function(grid, x, y, vroads, hroads) { # Find neighbours of node
  nb = list()
  i = 1
  
  if (y < grid) { # up
    nb[[i]] = list(x = x, y = y+1, r = vroads[x, y+1]) # r = cost from v-/hroads
    i = i + 1
  }
  if (y > 1) { # down
    nb[[i]] = list(x = x, y = y-1, r = vroads[x, y])
    i = i + 1
  }
  if (x < grid) { # right
    nb[[i]] = list(x = x+1, y = y, r = hroads[x, y])
    i = i + 1
  }
  if (x > 1) { # left
    nb[[i]] = list(x = x-1, y = y, r = hroads[x-1, y])
    i = i + 1
  }
  
  return(nb)
}

closestPack = function(car, packages) { # finds index of unpicked package closest to the car
  # detta hjärtebarn ska ersättas av att a* för de fem olika paketen
  distance = matrix(nrow = nrow(packages), ncol = 1)
  for (i in 1:nrow(packages)) {
    if (packages[i, 5] == 0) {
      distance[i] = sqrt(abs((car$x - packages[i, 1])^2 + (car$y - packages[i, 2])^2))
    } else {
      distance[i] = 10^6
    }
  } 
  return((which(distance == min(distance))))
}