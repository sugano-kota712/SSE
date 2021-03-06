turtles-own [ num-member age1 age2 age3 age4 flag-central next-state neighbor-state count-resident neighbor-points ]  ;; also need to set age2
patches-own [ age-bld use-type ]
globals [ singles central demand-house demand-apart age-limit time-step capacity]

to setup
  clear-all
  set age-limit 50
  set time-step 5
  set capacity 4
  ;;set env 8
  ;;set aging-rate 3time-step
  create-turtles 100 [ setxy random-xcor random-ycor ]
  ask turtles [
    set-age
    setup-turtle
  ]
  set singles sort turtles with [num-member = 1 and age3 = [] and age4 = []]
  setup-patches
  set central []
  foreach (range (- len) (len + 1)) [ n ->
    foreach (range (- len) (len + 1)) [ k ->
      set central sentence central (list (list n k))
    ]
  ]
  patch-draw
  turtle-draw
  reset-ticks
end

to set-age
  set age1 [] set age2 [] set age3 [] set age4 []
  ifelse random 100 < aging-rate [
    ifelse random 100 < 50 [ set age4 random-in-range 75 100 time-step 2 ]
    [ set age4 random-in-range 75 100 time-step 1 ]
  ]
  [
    ifelse random 100 < 50 [ set age3 random-in-range 50 75 time-step 2]
    [ set age2 random-in-range 25 50 time-step 2
      ifelse random 100 < 35 [ set age1 random-in-range 0 25 time-step 2 ]
      [if random 100 < 35 [ set age1 random-in-range 0 25 time-step 1 ]]
    ]
  ]
end

to setup-turtle
  set num-member 0
  set flag-central 0
  set next-state 0
  ;    ifelse age1 <= 55 [
  ;      set num-member item 0 (random-in-range 2 7 1 1)
  ;    ] [
  ;      set num-member item 0 (random-in-range 1 time-step 1 1)
  ;    ]
  set-num-member
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
end

to set-num-member
  set num-member ((length age1) + (length age2) + (length age3) + (length age4))
end

to setup-patches
  ask patches [
    let families (count (turtles-at 0 0))
    ifelse families = 1 [
      set use-type 1
      set age-bld (item 0 random-in-range 0 age-limit time-step 1)
    ] [
      ifelse families > 1 [
        if families > 4 [ ask n-of (families - 4) (turtles-at 0 0) [ die ] ]    ;;;;; less than 4 families at each apartment
        set use-type 2
        set age-bld (item 0 random-in-range 0 age-limit time-step 1)
        ask turtles-at 0 0 [ set flag-central 1 ]
      ]
      [
        ifelse random 100 < 3 [  ;; unused house at 3% of probability
          set use-type -1
          set age-bld (item 0 random-in-range 0 age-limit time-step 1)
        ] [
          set use-type 0
          set age-bld -1
        ]
      ]
    ]
  ]
end

to go
  if ticks >= 200 / time-step [ stop ]  ;; 100 yers
  ask patches with [use-type > 0] [ set age-bld age-bld + time-step ]
  ask turtles [
;    foreach [age1 age2 age3 age4] [ age-list ->      ;; Why needs literal not age1 ?
;      foreach age-list [ age ->
;        set age age + time-step
;      ]
;    ]
    age-shift
    population-shift
    independent
    marry
  ]
  ask turtles [
    set-state
    search-neighbors
    set-neighbor-state
    set-num-member
  ]

  ask patches [
    if age-bld = age-limit [
      set use-type 0             ;; Deconstruction
    ]
  ]
  ask turtles [
    build-stay ]
    build-central
  die-central
  ask patches [
    if age-bld = age-limit [
      set age-bld -1        ;; turn into an unused land
    ]
  ]
  patch-draw
  turtle-draw

  ;ask patches with [pcolor = brown];;;;;;;;;;;;;;;Just in Case
  tick
end

to set-state
  ifelse (random 100 > 4 * ([age-bld] of patch-here - age-limit)) [
    ifelse age2 != [] and num-member >= 3 [
      ifelse random 100 < 75 [ set next-state 1 ]
      [ set next-state 2 ]
    ] [
      ifelse color = red [
        ifelse random 100 < 75 [set next-state 4]
          [set next-state 2]
      ] [
        ifelse random 100 < 75 [ set next-state 2 ]
        [ set next-state 1 ]
      ]
    ]
  ] [ set next-state 0 ]
end

to search-neighbors
  ifelse next-state = 2 [
    foreach (neighbor-points) [ l ->
      let states [next-state] of (turtles at-points (list (l)))
      set neighbor-state neighbor-state + (length filter [i -> i = 2] states)
    ]
  ] [
    ifelse next-state = 1 [
      foreach (neighbor-points) [ l ->
        let states [next-state] of (turtles at-points (list (l)))
        set neighbor-state neighbor-state + (length filter [i -> i > 0] states)
      ]
    ] [ ]
  ]
end

to set-neighbor-state
  if next-state < 3 [
    if neighbor-state < env [
      set next-state next-state + 2
    ]
  ]
end

;to buy-secondhand
;  let build-cent patches with [use-type < 0]
;  let b-c count build-avl at
;;  while [(p-s < count patches with [use-type < 0]) and (t-s < count turtles with [next-state > 0 and next-state < 3]] [
;  ask

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
;    set next-state 0
  ] [
    ifelse next-state = 2 [
      ask patch-here [
        set use-type 2
        set age-bld 0
      ]
      set count-resident 1
      while [((any? turtles at-points neighbor-points with [next-state = 2]) or (any? turtles at-points neighbor-points with [next-state = 4])) and (count-resident < capacity)] [
      ;;ask item 0 (turtles at-points (list (one-of neighbor-points))) [
      ask turtles at-points (list (one-of neighbor-points)) [
        if next-state = 2 or next-state = 4 [  ;; even if the neighbor was thinking of moving tothe central
          set next-state 0                       ;; if he had been counted by other families as a neighbor in future
            ask patch-here [ set use-type (-1 * use-type) ]
            move-to patch-build                    ;; let them stay and not to migrate to the central
          set count-resident count-resident + 1
        ]
      ]
    ]
;    set next-state 0
      ;;set-state-all  ;; Do not reflect the result of migration of other families
    ] [ ]
  ]
end

to central-set
  foreach (central) [ loc ->
    if item 0 ([use-type] of (patches at-points (list (loc)))) > 0 [
      set central remove loc central
      set central remove-duplicates central
    ]
  ]
end

;to build-central-cycle  ;; no need any more
;  [
;    len-broaden
;    build-central-cycle
;  ]
;end

to build-central  
  foreach (central) [ loc ->
    ask patches at-points (list (loc)) [
    let x0 item 0 loc
    let y0 item 1 loc
    if [use-type] of patch-at x0 y0 = -2 [
      if any? turtles with [ next-state = 4 ] [
        ask one-of turtles with [next-state = 4] [
          ask patch xcor ycor [ set use-type (use-type * -1) ]
          move-to patch x0 y0
          ask patch x0 y0 [ set use-type 2 ]
          set flag-central 1
          set next-state 0
        ]
      ]
    ]
    if [use-type] of patch-at x0 y0 = -1 [
      if any? turtles with [ next-state = 3 ] [
        ask one-of turtles with [next-state = 3] [
          ask patch xcor ycor [ set use-type (use-type * -1) ]
          move-to patch x0 y0
          ask patch-here [ set use-type 1 ]
          set flag-central 1
          set next-state 0
        ]
      ]
    ]
  ]
  ]
  
  central-set
  set demand-house count turtles with [next-state = 3]
  set demand-apart 0
  ifelse remainder (count turtles with [next-state = 4]) 4 > 0 [
    set demand-apart (floor (count turtles with [next-state = 4] / 4) + 1)
  ] [ set demand-apart (floor (count turtles with [next-state = 4] / 4)) ]
  
  while [length central < (demand-house + demand-apart)] [ len-broaden ]

  foreach (n-of demand-house central) [ loc ->
    ask patches at-points (list (loc)) [
      ifelse use-type = -1 [ set use-type 1 ]
      [
        if use-type = 0 [
        set use-type 1
        set age-bld 0
        ]
      ]
    ]
    ask one-of turtles with [next-state = 3] [
      ask patch-here [ set use-type (use-type * -1) ]
      let x3 item 0 loc
      let y3 item 1 loc
      move-to patch x3 y3
      set flag-central 1
      ;        set next-state 0
    ]
    central-set
  ]

  foreach (n-of demand-apart central) [ loc ->
    ask patches at-points (list (loc)) [
      ifelse use-type = 0 [
        set use-type 2
        set age-bld 0
      ] [
        if use-type = -2 [ set use-type 2 ]
      ]
    ]
    foreach range capacity [
      if any? turtles with [next-state = 4] [
        ask one-of turtles with [next-state = 4] [
          let x4 item 0 loc
          let y4 item 1 loc
          ask patch-here [ set use-type (use-type * -1) ]
          move-to patch x4 y4
          set flag-central 1
          ;          set next-state 0
        ]
      ]
    ]
    central-set
  ]
end

to len-broaden
  set len len + 1
  foreach (range (- len) len) [ k ->
    set central sentence central (list (list len k) (list k len) (list (- len) k) (list k (- len)))
  ]
  central-set
end

;to age-shift
;  let n 1
;  let ages (list age1 age2 age3 age4)
;  foreach ages [ age-list ->
;    foreach age-list [ age ->
;      set age age + time-step
;      if age >= n * 25 [
;        set (item n ages) (sentence age (item n ages))
;        set age-list remove age age-list
;      ]
;    ]
;    set n n + 1
;  ]
;end

to age-shift
  foreach age1 [ age ->
    set age age + time-step
    if age >= 25 [
      set age2 sentence age2 age
      set age1 remove age age1
    ]
  ]
  foreach age2 [
    age -> set age age + time-step
    if age >= 50 [
      set age3 sentence age3 age
      set age2 remove age age2
    ]
  ]
  foreach age3 [
    age -> set age age + time-step
    if age >= 75 [
      set age4 sentence age4 age
      set age3 remove age age3
    ]
  ]
  foreach age4 [
    age -> set age age + time-step
  ]
end

to population-shift

  if random 100 < 15 [ if age4 != [] [ set age4 remove (one-of age4) age4 ] ]  ;;;;;;;;;;;;;;;;
  if random 100 < 7 [ if age3 != [] [ set age3 remove (one-of age3) age3 ] ]
  if random 100 < 5 [ if age2 != [] [ set age2 remove (one-of age2) age2 ] ]
  if random 100 < 5 [ if age1 != [] [ set age1 remove (one-of age1) age1 ] ]

  if length age2 >= 2 and random 100 > sum age2 [                       ;; give birth
    ifelse (sum age2 <= 60) and (random 100 > sum age2) [set age1 (sentence 0 time-step age1) ]
    [ set age1 sentence 0 age1 ]
  ]
  set-num-member
  if num-member = 0 [
    ask patch-here [ set use-type (-1 * use-type) ]
    die
  ]
end

to independent
  if age1 != [] [
;  if num-member >= 3 [
    foreach age1 [ age ->
      if age >= 20[
        if random 100 < 80 [
          set age1 remove age age1
          hatch 1 [
            setup-turtle
            set age1 sentence age1 age
            set next-state 2     ;; will live in an apartment
            set-num-member
          ]
        ]
      ]
    ]
;    foreach age2 [ age ->
;      if age <= 40 [
;        if random 100 < 40 [
;          set age2 remove age age2
;          hatch 1 [
;            setup-turtle
;            set age2 sentence age2 age
;            set next-state 2     ;; will live in an apartment
;            set-num-member
;          ]
;        ]
;      ]
;    ]
  ]
end

to marry
;  ;let singles [self] of turtles with [num-member = 1 and age3 = [] and age4 = []]
;  ;  let singles-list (list singles)
;  set singles sort turtles with [num-member = 1 and age3 = [] and age4 = []]
;  if length singles >= 2[
;  foreach (singles) [ single1 ->   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    let age-1 item 0 (sentence ([age1] of single1) ([age2] of single1))
;    if length singles >= 2 [
;      if random 100 > (floor (age-1) * 2 - 10) [
;        ;set singles remove single1 singles
;        let single2 one-of singles
;        if single2 != single1 [
;          let age-2 item 0 (sentence ([age1] of single2) ([age2] of single2))
;          if (random 100 > (floor (age-2) * 2 - 10)) and (random 100 > (age-1 - age-2) * 3) [
;            ask single1 [
;              set-num-member
;              set age1 (sentence ([age1] of single1) ([age1] of single2))
;              set age2 (sentence ([age2] of single1) ([age2] of single2))
;            ]
;            ask single2 [ die ]
;          ]
;        ]
;      ]
;    ]
;  ]
;  ]
  set singles turtles with [num-member = 1 and age2 != []]
  let n 0
  while [n < floor (count singles / 2)] [
    if count singles >= 2 [
    if random 100 > 50 [
      let couple n-of 2 singles
      hatch 1 [
        setup-turtle
        set age2 (reduce sentence ([age2] of couple))    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        set next-state 2     ;; will live in an apartment
        set-num-member
      ]
      ask couple [ die ]
    ]
    set n n + 1
    ]
  ]
end

to turtle-draw
  ask turtles [
    ifelse (num-member <= 2 and age4 != []) [ set color red ] [ ;; aged family
      ifelse (age1 != []) [ set color yellow ]                  ;; with kids
      [set color blue ]
    ]
  ]
end

to patch-draw
  ask patches with [ pcolor = brown ] [
    if not any? turtles-here [ set use-type -1 ]
  ]
  ask patches with [ pcolor = green ] [
    if not any? turtles-here [ set use-type -2 ]
  ]
  ask patches [
    ifelse use-type = -2 [ set pcolor 104 ] [
      ifelse use-type = -1 [ set pcolor black ] [
        ifelse use-type = 0 [ set pcolor white ] [
          ifelse use-type = 1 [ set pcolor brown ] [
            set pcolor green ]
        ]
      ]
    ]
  ]
end

to-report random-in-range [low high step num]
;  ifelse num [
    let result []
    let n 0
    while [n < num] [
      set result sentence (one-of (range low high step)) result
      set n n + 1
    ]
    report result
;  ] [
;    report low + one-of (range low high step)
;  ]
end

to die-central
  ask turtles-on patches with [use-type = 0] [
    let num-resi count turtles-here
    if num-resi >= 1 [
      foreach range (num-resi) [
        ask one-of turtles-here [ move-to one-of patches with [ use-type < 0 ] ]
      ]
    ]
  ]
  ask turtles-on patches with [use-type = -2 or use-type = -1] [
;    let num-resi count turtles-here
;    if num-resi >= 1 [
;;      foreach range (num-resi) [
;        ask one-of turtles-here [ move-to one-of patches with [ use-type < 0 ]
;        ask patch-here [ set use-type (-1 * use-type) ] ]
;    ]
;  ]
    if [use-type] of patch-here < 0 [
      ask patch-here [ set use-type (-1 * use-type) ]
    ]
  ]
  ask turtles-on patches with [use-type = 1] [
    let num-resi count turtles-here
    if num-resi > 1 [
      foreach range (num-resi - 1) [
        ask one-of turtles-here [ move-to one-of patches with [ use-type < 0 ]
          ask patch-here [ set use-type (-1 * use-type) ] ]
      ]
    ]
  ]
end
