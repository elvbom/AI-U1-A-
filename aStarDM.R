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
  print(nodes[[x]][[y]]$h)
  
  currX = x
  currY = y
  
  while (frontier > 0) { # FIXME bara så den inte blir evig?
    nodes[[currX]][[currY]]$visited = TRUE # we have visited the node we're on
    nodes[[currX]][[currY]]$frontier = FALSE
    frontier = frontier - 1

    nb = neighbours(grid, currX, currY, vroads, hroads) # find its neighbours
    smallest = list() # to keep track of "cheapest" neighbour
    
    for (i in 1:length(nb)) {
      nx = nb[[i]]$x
      ny = nb[[i]]$y
      nr = nb[[i]]$r 
      print(nodes[[nx]][[ny]]$cost)

      if (nodes[[nx]][[ny]]$visited == TRUE || nodes[[nx]][[ny]]$frontier == TRUE) { # can we find a new cheaper way to visited/frontier node?
        print(nr)
        print(nodes[[nx]][[ny]]$h)
        print((nodes[[nx]][[ny]]$h + nr))
        print('<')
        print(nodes[[nx]][[ny]]$cost)
        if ((nodes[[nx]][[ny]]$h + nr) < nodes[[nx]][[ny]]$cost) { # FIXME hitta varför dessa är samma
          nodes[[nx]][[ny]]$cost = nodes[[nx]][[ny]]$h + nr
          smallest[i] = nodes[[nx]][[ny]]$cost
          
          print('TRUE small')
          print(smallest[i])
        }
      } else { # find travel cost to node
        nodes[[nx]][[ny]]$cost = nodes[[nx]][[ny]]$h + nr
        smallest[i] = nodes[[nx]][[ny]]$cost
        
        print('else small')
        print(smallest[i])
        
        nodes[[nx]][[ny]]$frontier = TRUE
        frontier = frontier + 1
      }
      print(smallest)
    }
    # nb[i] = här behöver jag fixa så att grannen får ngn sorts kostnad tsms med koordinater så att den går att jämföra
    # så här behöver jag kolla vilken granne som har lägst kostnad, och gå mot den
    # jag behöver också lägga in i totalCost så allt räknas ihop
    
  }
  return(car$load)
}

heuristic = function(x, y, goal) { # Calculate heuristic function, number of steps between start and goal
  return(abs(goal$x - x) + abs(goal$y - y))
}

neighbours = function(grid, x, y, vroads, hroads) { # Find neighbours of node
  nb = list()
  i = 1
  
  if (y < grid) { # up
    nb[[i]] = list(x = x, y = y+1, r = vroads[y, x]) # r = cost from v-/hroads
    i = i + 1
  }
  if (y > 1) { # down
    nb[[i]] = list(x = x, y = y-1, r = vroads[y-1, x])
    i = i + 1
  }
  if (x < grid) { # right
    nb[[i]] = list(x = x+1, y = y, r = hroads[y, x])
    i = i + 1
  }
  if (x > 1) { # left
    nb[[i]] = list(x = x-1, y = y, r = hroads[y, x-1])
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