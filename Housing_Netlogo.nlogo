turtles-own [ num-member age1 next-state neighbor-points ]  ;; also need to set age2
patches-own [ age-bld use-type ]
globals [ aging-rate env central area demand-house demand-apart]

to setup
  clear-all
  setup-turtles
  setup-patches
  reset-ticks
  set env 8
  set aging-rate 35
  set central []
  foreach (range -8 9) [ n ->
      foreach (range -8 9) [ k ->
        set central sentence central (list (list n k))
      ]
    ]
  set area sqrt (length central)
  patch-draw
  turtle-draw
end

to setup-turtles
  create-turtles 100 [ setxy random-xcor random-ycor ]
  ask turtles [
    ifelse random 100 < aging-rate [ 
      set age1 random-in-range 60 95 5 
    ] [
      set age1 random-in-range 25 60 5
    ]
    ifelse age1 <= 55 [
      set num-member random-in-range 2 7 1
    ] [
      set num-member random-in-range 1 5 1
    ]
    set-state
    set neighbor-points []
    foreach (range -4 5) [ n ->
      foreach (range -4 5) [ k ->
        set neighbor-points sentence neighbor-points (list (list n k))
      ]
    ]
    set neighbor-points remove [0 0] neighbor-points
  ]
end

to setup-patches
  ask patches [
    ifelse any? turtles [
      set use-type 1
      set age-bld (random-in-range 0 45 5)
    ] [
      ifelse random 100 < 3 [  ;; adjust here
        set use-type 0
        set age-bld (random-in-range 0 45 5)
      ] [
      set use-type -1
      set age-bld -1
    ]
  ]
  ]  
end

to go
  ask turtles [ set age1 age1 + 5 ]
  ask patches [ set age-bld age-bld + 5 ]
  setup-turtles
  setup-patches
  search-neighbors
  build
  population-shift
  independent
  patch-draw
  turtle-draw
  tick
end

to set-state
  ask turtles [
    ifelse [age-bld] of patch-here > 30 [
      ifelse age1 > 40 and num-member >= 3 [
        set next-state 1
      ] [
        set next-state 2
      ]
    ] [
      set next-state 0
      ]
    ]
end

to search-neighbors
  let count-state 0
  ask turtles [
    ifelse next-state = 2 [
      foreach (neighbor-points) [ l ->
        if [next-state] of (turtles at-points l) = 2 [
        set count-state count-state + 1
      ] 
    ]
    ] [
        ifelse next-state = 1 [
          foreach (neighbor-points) [ l ->
          if [next-state] of (turtles at-points l) >= 0 [
            set count-state count-state + 1
          ]
        ]
      ] [ ]
    ]
    if count-state < env [
      set next-state next-state + 2
    ]
]
end

to build
  central-set
  set demand-house count turtles with [next-state = 3]
  set demand-apart 0
  ifelse remainder (count turtles with [next-state = 4]) 4 > 0 [
    set demand-apart (floor (count turtles with [next-state = 4] / 4) + 1)
  ] [ set demand-apart (floor (count turtles with [next-state = 4] / 4)) ]

  central-move-cycle

  let patch-build patch-here
  ask patches [
      ifelse [next-state] of turtles-here = 1 [
        set use-type 1
        set age-bld 0
        set [next-state] of turtles-here 0
      ] [
        ifelse [next-state] of turtles-here = 2 [
          set use-type 2
          set age-bld 0
          ask turtles [
            foreach (n-of 3 neighbor-points) [ l ->
              set [next-state] of ( turtles at-points l ) 0
              move-to patch-build
          ]
        ]
          set [next-state] of turtles-here 0
      ] [ ] 
    ]
  ]
end

to central-set
  foreach (central) [ loc ->
    if [use-type] of (patches at-points loc) >= 0 [
      set central remove loc central
    ]
    ]
end

to central-move-cycle
  ifelse length central >= (demand-house + demand-apart) [
    central-move
  ] [
    area-broaden
    central-move-cycle
    ]
end

to central-move
  foreach (n-of demand-house central) [ loc ->
      ask patches at-points loc [
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
      ask patches at-points loc [
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
  ask turtles [
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
        if random 100 < 5 [ set num-member num-member - 1 ]
        if random 100 < 30 [ set num-member num-member + 1 ]
      ]
    ]
    if num-member = 0 [ die ]
  ]
end

to independent
  ask turtles [
    if age1 <= 75 and age1 >= 40 and num-member >= 4 [
      if random 100 < 50 [
        set num-member num-member - 2
        hatch 1 [
          set num-member 2
          set age1 25
        ]
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






