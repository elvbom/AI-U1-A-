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
  goal = list(x = packages[load, 1], y = packages[load, 2])
  
  nodes = list() # make graph with all nodes
  
  for (i in 1:grid) { # give nodes initial values
    nodes[[i]] = list()
    for (j in 1:grid) {
      nodes[[i]][[j]] = list(x = i, y = j, h = heuristic(i, j, goal), cost = 10^6, totalCost = 10^6, frontier = FALSE, visited = FALSE)
    }
  }
  
  frontier = 1 # the node we're on, to be explored
  currX = x
  currY = y
  
  while (frontier > 0) { # FIXME bara så den inte blir evig?
    node[[currX]][[currY]]$visited = TRUE # we have visited the node we're on
    node[[currX]][[currY]]$frontier = FALSE
    frontier = frontier - 1
    # fixme flytta på begynnelsevillkor? tänk igenom loop bättre
    node[[currX]][[currY]]$cost = node[[currX]][[currY]]$h # it's our starter node, so its cost = its heuristic
    
    nb = neighbours(grid, currX, currY) # find its neighbours
    for (i in 1:length(nb)) {
      node = nodes[[nb[i]$x]][[nb[i]$y]] # fetch neighbour from nodes list
      
      if (node$visited == TRUE || node$frontier == TRUE) { # can we find a new cheaper way to visited/frontier node?
        if ((node$h + nb$r) < node$cost) {
          node$cost = node$h + nb$r
        }
      } else { # find travel cost to node
        node$cost = node$h + nb$r
        node$frontier = TRUE
        frontier = frontier + 1
      }
      # nb[i] = här behöver jag fixa så att grannen får ngn sorts kostnad tsms med koordinater så att den går att jämföra
      # så här behöver jag kolla vilken granne som har lägst kostnad, och gå mot den
      # jag behöver också lägga in i totalCost så allt räknas ihop
    }
  }
    
    # behöver uppdatera vilken nod vi står på sen genom att se vart minsta konstaden är och hoppa dit
    
  }

  return(car$load)
}

heuristic = function(x, y, goal) { # Calculate heuristic function, number of steps between start and goal
  return(abs(goal$x - x) + abs(goal$y - y))
}

neighbours = function(grid, x, y) { # Find neighbours of node
  nb = list()
  i = 1
  
  if (y < grid) { # up
    nb[i] = list(x = x, y = y+1, r = vroads[x][y+1]) # r = cost from v-/hroads
    i = i + 1
  }
  if (y > 1) { # down
    nb[i] = list(x = x, y = y-1, r = vroads[x][y])
    i = i + 1
  }
  if (x < grid) { # right
    nb[i] = list(x = x+1, y = y, r = hroads[x][y])
    i = i + 1
  }
  if (x > 1) { # left
    nb[i] = list(x = x-1, y = y, r = hroads[x-1][y])
    i = i + 1
  }
  
  return(nb)
}

findCost = function(grid, x, y, goal, vroads, hroads) {
  cost = heuristic(x, y, goal) + vroads[x, y]
  
  #i = 1
  # # FIXME check how vroads hroads actually works
  # if (y < grid) { # up
  #   cost[i] = heuristic(x, y+1, goal) + vroads[x, y+1]
  #   i = i + 1
  # }
  # if (y > 1) { # down
  #   cost[i] = heuristic(x, y-1, goal) + vroads[x, y-1]
  #   i = i + 1
  # }
  # if (x < grid) { # right
  #   cost[i] = heuristic(x+1, y, goal) + hroads[x+1, y]
  #   i = i + 1
  # }
  # if (x > 1) { # left
  #   cost[i] = heuristic(x-1, y, goal) + hroads[x-1, y]
  #   i = i + 1
  # }
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