turtles-own [ num-member age1 next-state neighbor-state count-resident neighbor-points ]  ;; also need to set age2
patches-own [ age-bld use-type ]
globals [ central area demand-house demand-apart age-limit time-step]

to setup
  clear-all
  set age-limit 30
  set time-step 5
  ;;set env 8
  ;;set aging-rate 3time-step
  setup-turtles
  setup-patches
  set central []
  foreach (range (- 8) 9) [ n ->
      foreach (range (- 8) 9) [ k ->
        set central sentence central (list (list n k))
      ]
    ]
  set area sqrt (length central)
  patch-draw
  turtle-draw
  reset-ticks
end

to setup-turtles
  create-turtles 100 [ setxy random-xcor random-ycor ]
  ask turtles [
    ifelse random 100 < aging-rate [ 
      set age1 random-in-range 60 95 time-step 
    ] [
      set age1 random-in-range 25 60 time-step
    ]
    ifelse age1 <= 55 [
      set num-member random-in-range 2 7 1
    ] [
      set num-member random-in-range 1 time-step 1
    ]
    set-state
    set neighbor-points []
    foreach (range (- 3) 4) [ n ->
      foreach (range (- 3) 4) [ k ->
        set neighbor-points sentence neighbor-points (list (list n k))
      ]
    ]
    set neighbor-points remove [0 0] neighbor-points
    set neighbor-state 0
    set count-resident 0
  ]
end

to setup-patches
  ask patches [
    ifelse count (turtles-at 0 0) > 0 [
      set use-type 1
      set age-bld (random-in-range 0 age-limit time-step)
    ] [
      ifelse random 100 < 3 [  ;; unused house at 3% of probability
        set use-type 0
        set age-bld (random-in-range 0 age-limit time-step)
      ] [
      set use-type -1
      set age-bld -1
    ]
  ]
  ]  
end

to go
  if ticks >= 100 / time-step [ stop ]  ;; 100 yers
  ask patches [ set age-bld age-bld + time-step ]
  ask turtles [ 
    set age1 age1 + time-step
    population-shift
    independent
    set-state  
    search-neighbors
    set-neighbor-state
  ]
  
  ask patches [
    if age-bld = age-limit [
      set use-type -1             ;; Deconstruction
    ]
  ]
  ask turtles [
    build-stay
    build-central
  ]
  ask patches [
    if age-bld = age-limit [
      set age-bld -1        ;; turn into an unused land
    ]
  ]
  patch-draw
  turtle-draw
  tick
end

to set-state
    ifelse [age-bld] of patch-here = age-limit [
      ifelse age1 <= 50 and num-member >= 3 [
        set next-state 1
      ] [
        set next-state 2
      ]
    ] [
      set next-state 0
      ]
end

to search-neighbors
  ifelse next-state = 2 [
    foreach (neighbor-points) [ l ->
      if [next-state] of (turtles at-points (list (l))) = 2 [
        set neighbor-state neighbor-state + 1
      ] 
    ]
  ] [
    ifelse next-state = 1 [
      foreach (neighbor-points) [ l ->
        if [next-state] of (turtles at-points (list (l))) >= 0 [
          set neighbor-state neighbor-state + 1
        ]
      ]
    ] [ ]
  ]
end

to set-neighbor-state
  if neighbor-state < env [
    set next-state next-state + 2
  ]
end
  
to build-stay
  ;;let neighboring-families []
  let patch-build patch-here
  ;;foreach (neighbor-points) [ loc ->
  ;;  set neighboring-families sentence neighboring-families (turtles at-points loc)
  ;;]
  ifelse next-state = 1 [
    ask patch-here [
      set use-type 1
      set age-bld 0
    ]
    set next-state 0
  ] [
    ifelse next-state = 2 [
      ask patch-here [
        set use-type 2
        set age-bld 0
      ]
      set count-resident 1
      while [((any? turtles at-points neighbor-points with [next-state = 2]) or (any? turtles at-points neighbor-points with [next-state = 4])) and (count-resident < 4)] [
      ;;ask item 0 (turtles at-points (list (one-of neighbor-points))) [
      ask turtles at-points (list (one-of neighbor-points)) [ 
        if next-state = 2 or next-state = 4 [  ;; even if the neighbor was thinking of moving tothe central
          set next-state 0                       ;; if he had been counted by other families as a neighbor in future
          move-to patch-build                    ;; let them stay and not to migrate to the central
          set count-resident count-resident + 1
        ]
      ]
    ]
    set next-state 0
      ;;set-state-all  ;; Do not reflect the result of migration of other families
    ] [ ] 
  ]
end

to central-set
  foreach (central) [ loc ->
    if item 0 ([use-type] of (patches at-points (list (loc)))) >= 0 [
      set central remove loc central
    ]
    ]
end

to build-central-cycle
  ifelse length central >= (demand-house + demand-apart) [
    build-central
  ] [
    area-broaden
    build-central-cycle
    ]
end

to build-central
  central-set
  set demand-house count turtles with [next-state = 3]
  set demand-apart 0
  ifelse remainder (count turtles with [next-state = 4]) 4 > 0 [
    set demand-apart (floor (count turtles with [next-state = 4] / 4) + 1)
  ] [ set demand-apart (floor (count turtles with [next-state = 4] / 4)) ]

  build-central-cycle

  foreach (n-of demand-house central) [ loc ->
      ask patches at-points (list (loc)) [
        set use-type 1
        set age-bld 0
      ]
      ask one-of turtles with [next-state = 3] [
        let x3 item 0 loc
        let y3 item 1 loc
        move-to patch x3 y3
        set next-state 0
      ]
  ]
  central-set
  foreach (n-of demand-apart central) [ loc ->
    ask patches at-points (list (loc)) [
      set use-type 1
      set age-bld 0
    ]
    foreach (range 1 5) [
      ask one-of turtles with [next-state = 4] [
        let x4 item 0 loc
        let y4 item 1 loc
        move-to patch x4 y4
        set next-state 0
      ]
    ]
    central-set
  ]
end

to area-broaden
  set area area + 1
  foreach (range (- area) (- area + 1)) [ k ->
    set central sentence central (list (list area k) (list k area) (list (- area) k) (list k (- area)))
  ]
end

to population-shift
  ifelse age1 >= 65 [
    if random 100 < 50 [ set num-member num-member - 1 ]
  ] [
    ifelse age1 < 65 and age1 > 40 [
      if random 100 < 20 [ 
        set num-member num-member - 1
      ]
      if random 100 < 15 [ 
        set num-member num-member + 1
      ]
    ] [
      if random 100 < time-step [ set num-member num-member - 1 ]
      if random 100 < 30 [ set num-member num-member + 1 ]
    ]
  ]
  if num-member = 0 [ 
    ask patch-here [ set use-type 0 ]
    die
  ]
end

to independent
  if age1 <= 75 and age1 >= 40 and num-member >= 4 [
    if random 100 < 50 [
      set num-member num-member - 2
      hatch 1 [
        set num-member 2
        set age1 25
      ]
    ]
  ]
end

to turtle-draw
  ask turtles [
    ifelse num-member = 1 and age1 > 70 [ set color red ] [
      ifelse num-member > 2 and age1 < 65 [ set color blue ] [
        set color yellow ]
    ]
  ]
end

to patch-draw
  ask patches [
    ifelse use-type = 0 [ set pcolor black ] [
      ifelse use-type = -1 [ set pcolor white ] [
        ifelse use-type = 1 [ set pcolor brown ] [
          set pcolor green ]
      ]
    ]
  ]
end  

to-report random-in-range [low high step]
 report low + one-of (range low high step)
end






