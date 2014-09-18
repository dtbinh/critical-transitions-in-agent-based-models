;;-----------------------------
;;-----------------------------
;; PREAMBLE
;;-----------------------------
;;-----------------------------

breed [prey a-prey]
breed [adults an-adult]
breed [juveniles juvenile]
breed [fishermen a-fisherman]

globals [
  number-of-islands
  mean-island-area
  sd-island-area
  adult-breeding-ground
  prey-breeding-ground
  breeding-ground-area
  sd-breeding-ground-area
  number-of-ports
  initial-number-prey
  initial-number-adults
  initial-number-juveniles
  initial-number-fishermen
  cannibal-deaths
  fishing-deaths-adults
  fishing-deaths-prey
  fishing-deaths-juveniles
  natural-deaths-adults
  natural-deaths-juveniles
  natural-deaths-prey
  prey-deaths-eaten
  prey-total-deaths
  adult-total-deaths
  juvenile-total-deaths
  starvation-deaths
  fishing-mortality
  total-catch
  adults-caught
  prey-caught
  juveniles-caught
  clock
  carrying-capacity
  time-to-breed
  time-to-fish
  fish-time
  sexy-time
]

patches-own [
  island?
  breeding-ground-adults?
  breeding-ground-prey?
  port?
    ]

turtles-own [
  age
  flockmates
  nearest-neighbor
  catch
  metabolism
]


;;-----------------------------
;;-----------------------------
;; SETUP 
;;-----------------------------
;;-----------------------------

to setup
  clear-all
  define-parameters
  config-ocean
  setup-prey
  setup-adults
  setup-juveniles
  if fishery? [
  setup-fishermen
  ]
  reset-ticks
end


;;-----------------------------
;;-----------------------------
;; SETUP PROCEDURES
;;-----------------------------
;;-----------------------------


to define-parameters
;;-----------------------------
;; OCEAN PARAMETERS
;;-----------------------------
  set number-of-islands 2
  set mean-island-area 3
  set sd-island-area 1.5
  set adult-breeding-ground 1
  set prey-breeding-ground 1
  set number-of-ports 1
;;-----------------------------
;; INITIAL SEEDING
;;-----------------------------
  set initial-number-prey 200
  set initial-number-adults 100
  set initial-number-juveniles 100
  set initial-number-fishermen 20
;;-----------------------------
;; INITIAL MONITOR COUNTS
;;-----------------------------  
  set cannibal-deaths 0
  set natural-deaths-adults 0
  set natural-deaths-juveniles 0
  set natural-deaths-prey 0
  set prey-deaths-eaten 0 
  set fishing-deaths-adults 0
  set fishing-deaths-prey 0
  set fishing-deaths-juveniles 0
  set fishing-mortality 0
  set total-catch 0
  set adults-caught 0
  set prey-caught 0
  set juveniles-caught 0
  set prey-total-deaths 0
  set adult-total-deaths 0
  set juvenile-total-deaths 0
;;-----------------------------
;; INITIAL MISC.
;;-----------------------------  
  set carrying-capacity 2500
  set sexy-time 0
  set clock 1
end


;;-----------------------------
;; BUILDS OCEAN 
;;-----------------------------


to config-ocean
   ask patches
   [
     set island? false
     set breeding-ground-adults? false
     set breeding-ground-prey? false
     set port? false
     set pcolor blue - 2
   ]
   ask n-of number-of-islands patches
   [
     set island? true
     set breeding-ground-adults? false
     set breeding-ground-prey? false
     set port? false
     set pcolor grey - 2
   ]
   ask patches with[island?]
   [
     let p self
     let a max list 1 round (random-normal mean-island-area ( mean-island-area * sd-island-area))
   ask patches with[ distance p <= a ]
   [
     set island? true
     set breeding-ground-adults? false
     set breeding-ground-prey? false
     set port? false
     set pcolor grey - 2
   ]
   ]
   ask n-of adult-breeding-ground patches with [ pcolor != grey - 2 and pcolor != pink]
   [
    set island? false
    set breeding-ground-adults? true
    set breeding-ground-prey? false
    set port? false
    set pcolor red
  ]
  ask patches with[breeding-ground-adults?]
  [
    let q self
    let b max list 1 round 4
    ask patches with [distance q <= b ]
  [
    set island? false
    set breeding-ground-adults? true
    set breeding-ground-prey? false
    set port? false
    set pcolor red
  ]
  ]
  ask n-of prey-breeding-ground patches with [ pcolor != grey - 2 and pcolor != red ]
  [
    set island? false
    set breeding-ground-adults? false
    set breeding-ground-prey? true
    set port? false
    set pcolor pink
  ]
  ask patches with[breeding-ground-prey?]
  [
    let r self
    let c max list 1 round 4
    ask patches with [distance r <= c ]
  [
    set island? false
    set breeding-ground-adults? false
    set breeding-ground-prey? true
    set port? false
    set pcolor pink
  ]
  ]
  ask n-of number-of-ports patches
  [
    set island? false
    set breeding-ground-adults? false
    set breeding-ground-prey? false
    set port? true
    set pcolor lime
  ]
  if fishery? [
  ask patches with[port?]
  [
    let s self
    let d max list 1 round 4
    ask patches with[ distance s <= d ]
  [
    set island? false
    set breeding-ground-adults? false
    set breeding-ground-prey? false
    set port? true
    set pcolor lime
   ]
  ]
  ]
end


;;-----------------------------
;; SETUP TURTLES
;;-----------------------------


to setup-prey
  create-prey initial-number-prey [
    set color yellow 
    set shape "fish 3"
    set size 1 ;+ random-float 2
    set age 1
    let x 0
    let y 0
    ask one-of patches with [pcolor = blue - 2]
    [ 
      set x pxcor
      set y pycor
    ]
    setxy x y
  ]
end

to setup-adults
  create-adults initial-number-adults [
    set color black
    set shape "fish"
    set size 1.75 ;+ random-float 2
    set age 300
    let x 0
    let y 0
    set metabolism 0
    ask one-of patches with [pcolor = blue - 2]
    [ 
      set x pxcor
      set y pycor
    ]
    setxy x y
  ]
end

to setup-juveniles
  create-juveniles initial-number-juveniles [
    set color black + 4
    set shape "fish"
    set size 1
    set age 1  
    let x 0
    let y 0
    ask one-of patches with [pcolor = blue - 2]
    [ 
      set x pxcor
      set y pycor
    ]    
    setxy x y
  ]
end

to setup-fishermen
  create-fishermen initial-number-fishermen [
    setxy random-xcor random-ycor
    set color orange 
    set shape "boat" 
    set size 2.5
    set catch 0
    let x 0
    let y 0
    ask one-of patches with [pcolor = lime]
    [
      set x pxcor
      set y pycor
    ]
    setxy x y
  ]
end
    

;;-----------------------------
;;-----------------------------
;; GO PROCEDURES
;;-----------------------------
;;-----------------------------
        
to go
  
  ask prey [
    set age age + 1
    swim-prey] 
  repeat 5 [ ask prey [fd 0.2] display ]
  ask prey [
    grim-reaper-prey
    reproduce-prey
  ]
  
  ask adults [
    set age age + 1
    set metabolism metabolism + 1
    swim-adults]
  repeat 5 [ ask adults [fd 0.2] display ]
  ask adults [
    hunt
    grim-reaper-adults
    reproduce-adults
  ]
  
  ask juveniles [
    set age age + 1
    swim-juveniles]
  repeat 5 [ask juveniles [fd 0.2] display ]
  ask juveniles [
    grim-reaper-juveniles
    resource-compete
    become-adult
  ]
  
  ask fishermen [
    check-season
  ]
 
;      boat-move
;    set fuel fuel - 1]
;  ask fishermen[
;    fish
;    check-catch-limit
;    check-fuel
;    check-mistakes
;  ]
  
  set clock clock + 1
  check-breeding ;-------------sets global time to breed
  tick
end

;;-----------------------------
;;-----------------------------
;; PREY PROCEDURES
;;-----------------------------
;;-----------------------------

to swim-prey
  let wall? false
  ask patch-ahead 2 [
    if (pcolor = grey - 2) 
    [
      set wall? true
    ]
    if (pcolor = lime)
    [ 
      set wall? true
    ]    
  ]
    ifelse (wall?)
    [
      right random 360
      swim-prey
    ]
    [
     school-prey 
      ]
end


;;-----------------------------
;; PREY SCHOOL FUNCTION
;;-----------------------------


to school-prey  
  find-flockmates
  if any? flockmates
    [ find-nearest-neighbor
      ifelse distance nearest-neighbor < 0.75
        [ separate ]
        [ align
          cohere ] ]
end

to find-flockmates  
  set flockmates other prey in-radius 2
end

to find-nearest-neighbor 
  set nearest-neighbor min-one-of flockmates [distance myself]
end


to separate 
  turn-away ([heading] of nearest-neighbor) 3
end

to align
  turn-towards average-flockmate-heading 15
end

to cohere 
  turn-towards average-heading-towards-flockmates 3
end

to turn-towards [new-heading max-turn]  
  turn-at-most (subtract-headings new-heading heading) max-turn
end

to turn-away [new-heading max-turn]  
  turn-at-most (subtract-headings heading new-heading) max-turn
end

to turn-at-most [turn max-turn] 
  ifelse abs turn > max-turn
    [ ifelse turn > 0
        [ rt max-turn ]
        [ lt max-turn ] ]
    [ rt turn ]
end

to-report average-flockmate-heading  
  let x-component sum [dx] of flockmates
  let y-component sum [dy] of flockmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

to-report average-heading-towards-flockmates  
  let x-component mean [sin (towards myself + 180)] of flockmates
  let y-component mean [cos (towards myself + 180)] of flockmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end



;;-----------------------------
;; PREY MORTALITY FUNCTION
;;-----------------------------

to grim-reaper-prey
  if clock mod 100 = 0 [
    if random-float 100 < 9 [ ;----------------------------------discrete-method natural mortality occurs at time step interval [everytime tick = 100]
        set natural-deaths-prey natural-deaths-prey + 1
        set prey-total-deaths prey-total-deaths + 1
        die
    ]
  ]     
;  ]
;  if age >= 500 [    ;-------------------------------------not necessary to fit discrete model, but biologically reasonable
;    if random-float 100 < 49 [die]
;    set natural-deaths-prey natural-deaths-prey + 1 
;  ]
end


;;-----------------------------
;; PREY REPRODUCE FUNCTION
;;-----------------------------

to reproduce-prey
  if time-to-breed = true [  ;;------------------------------will only breed at specified time interval between clock mod 80 = 0 and clock mod 100 = 0
    if age >= 100 [          ;;------------------------------sets minimum reproductive age so that newborns don't breed 
    let location false       ;;------------------------------every time fish want to reproduce, they have to move towards their reproductive sphere
    ask patch-here[
    if  pcolor = pink                
    [ set location true
    ]]
    ifelse location != false [
      if count(prey) <= carrying-capacity [
      hatch random 50 [ 
              set age 1
              set heading random 360 
              let wall? false
              ask patch-ahead 1[
                if(pcolor = grey - 2)
                [
                  set wall? true
                ]
                if (pcolor = lime)
                [ 
                  set wall? true
                ]     
              ] 
              ifelse (wall?)
              [
                rt random 360
                move
                move
              ]
              [ fd 1 ]
      ] ;;----------------------------------------------------end of hatch command
      set natural-deaths-prey natural-deaths-prey + 1
      set prey-total-deaths prey-total-deaths + 1
      die  ;;--------------------------------kills adult breeder (modeled on reproductive behavior of capelin, males die (hence 50%)
      ]  
    ]
    [trace-back]
    ]
    swim-prey
  ]
  swim-prey
end 

to trace-back

  let x 0                   ;;find the way back to the sources and reproduce
  let y 0
  let candidates one-of patches with [pcolor = pink]
  if candidates != nobody
    [ ask one-of patches with [pcolor = pink]
      [
        set x pxcor
        set y pycor
      ]
    facexy x y
    let wall? false
    ask patch-ahead 1[
      if(pcolor = grey - 2)
        [
          set wall? true
        ] 
      if (pcolor = lime)
        [ 
          set wall? true
        ]        
    ] 
    ifelse (wall?)
      [
        rt random 360          ;;if there are walls on them way, they would try to find another path
        move
        move

      ]
      [ fd 1
        ]]
end 




;;-----------------------------
;;-----------------------------
;; ADULT PROCEDURES
;;-----------------------------
;;-----------------------------

to swim-adults
  let wall? false
  ask patch-ahead 5 [
    if (pcolor = grey - 2)
    [
      set wall? true
    ]
    if (pcolor = lime)
    [ 
      set wall? true
    ]    
  ]
    ifelse (wall?)
    [
      right random 360
      swim-adults
    ]
    [
     school-adults
      ]
end

;;-----------------------------
;; ADULT SCHOOL FUNCTION
;;-----------------------------


to school-adults  
  find-flockmates-adults
  if any? flockmates
    [ find-nearest-neighbor
      ifelse distance nearest-neighbor < 0.85
        [ separate ]
        [ align
          cohere ] ]
end

to find-flockmates-adults 
  set flockmates other adults in-radius 3
end

;;----------------------------------
;; ADULT HUNT / CANNIBALIZE FUNCTION
;;----------------------------------

to hunt
  ifelse any? prey in-radius 1 [
    if random 100 <= 59 [
      let yummy count prey in-radius 1
      set prey-deaths-eaten prey-deaths-eaten + yummy
      set prey-total-deaths prey-total-deaths + yummy
      ask prey in-radius 1 [die]
      set metabolism 0
    ]
  ]
  [
  school-adults
  ;chase ;----------------removed for sake of simplicity
  cannibalize
  ]
end

;to chase                   
;  ifelse any? prey in-radius 2
;    [ let candidates one-of prey in-radius 2
;      let wall? false                 ;;if there are forage fishes nearby
;      ask patch-ahead 1[
;        if(pcolor = grey - 2)
;        [
;          set wall? true                ;;check if there are walls between them
;        ]
;      ] 
;      ifelse (wall?)
;      [
;        rt random 360       ;;if there are walls, random move
;        move
;      ]
;      
;      [
;        face candidates           ;;if there are no walls, face the forage fish and move
;        fd 1
;        swim-adults
;      ]   
;      
;    ]
;  
;    [ cannibalize ]
;end 


to cannibalize
  ifelse any? juveniles in-radius 1 [
    if random 100 <= 49 [  ;----------------------------------fit cannibalism rate to data from cod stomach analysis
      let yumyum count juveniles in-radius 1
      set cannibal-deaths cannibal-deaths + 1
      set juvenile-total-deaths juvenile-total-deaths + 1
      ask juveniles in-radius 1 [die]
      set metabolism 0
    ]
  ]
  [
    swim-adults
  ]
end

;;-----------------------------
;; ADULT MORTALITY FUNCTION
;;-----------------------------
  
to grim-reaper-adults
  if clock mod 10 = 0 [
    if random-float 100 <= 1 [
      set natural-deaths-adults natural-deaths-adults + 1     ;;-----------------------natural mortality (fixed at 20% of each recruitment interval [100 ticks])
      set adult-total-deaths adult-total-deaths + 1
      die
  ]  
  ]
  if metabolism >= 50 [
    set starvation-deaths starvation-deaths + 1
    die
  ]
;  if age >= 1400 [
;    set natural-deaths-adults natural-deaths-adults + 1]      ;;-----------------------age-bounded mortality. unused to fit discrete model
;    die
end

;;-----------------------------
;; ADULT REPRODUCE FUNCTION
;;-----------------------------
  
to reproduce-adults
  if time-to-breed = true [
    if age >= 400 [
  let location false                  ;;so every time fish want to reproduce, they have to move towards those sources
  ask patch-here[
    if  pcolor = red                 ;;and hatch their childeren out side the walls
    [ set location true
    ]]
    ifelse location != false [
      hatch-juveniles random 30 [ 
              set age 1
              set color black + 4
              set shape "fish"
              set size 1
              set heading random 360 
              let wall? false
              ask patch-ahead 1[
                if(pcolor = grey - 2)
                [
                  set wall? true
                ]
                if (pcolor = lime)
                [ 
                set wall? true
                ]
              ] 
              ifelse (wall?)
              [
                rt random 360
                move
                move
              ]
              [ fd 1 ]
      ] ; -- end of hatch command
      ]
    [trace-back-adults]
    ]
    swim-adults
  ]
  swim-adults  
end 

to trace-back-adults    
  let x 0                   ;;find the way back to the sources and reproduce
  let y 0
  let candidates one-of patches with [pcolor = red]
  if candidates != nobody
    [ ask one-of patches with [pcolor = red]
      [
        set x pxcor
        set y pycor
      ]
    facexy x y
    let wall? false
    ask patch-ahead 1[
      if(pcolor = grey - 2)
        [
          set wall? true
        ]
      if (pcolor = lime)
        [ 
          set wall? true
        ]   
    ] 
    ifelse (wall?)
      [
        rt random 360          ;;if there are walls on them way, they would try to find another path
        move

      ]
      [ fd 1
        fd 1
        ]]
end 

;;-----------------------------
;;-----------------------------
;; JUVENILE PROCEDURES
;;-----------------------------
;;-----------------------------

to swim-juveniles
  let wall? false
  ask patch-ahead 2 [
    if (pcolor = grey - 2)
    [
      set wall? true
    ]
    if (pcolor = lime)
    [ 
      set wall? true
    ]    
  ]
    ifelse (wall?)
    [
      right random 360
      swim-juveniles
    ]
    [
     school-juveniles
      ]
end

;;-----------------------------
;; JUVENILE SCHOOL FUNCTION
;;-----------------------------

to school-juveniles
  find-flockmates-juveniles
  if any? flockmates
    [ find-nearest-neighbor
      ifelse distance nearest-neighbor < 0.85
        [ separate ]
        [ align
          cohere ] ]
end

to find-flockmates-juveniles  
  set flockmates other juveniles in-radius 3
end

to grim-reaper-juveniles
  if clock mod 10 = 0 [
    if random-float 100 < 1 [
      set natural-deaths-juveniles natural-deaths-juveniles + 1
      set juvenile-total-deaths juvenile-total-deaths + 1
      die   ;-------------------natural mortality
    ]
  ]
end

to resource-compete
  ifelse any? juveniles in-radius 2 [
    let toomany count juveniles in-radius 2 
    if toomany > 20 [
      let loser one-of juveniles-here
      set juvenile-total-deaths juvenile-total-deaths + 1
      set natural-deaths-juveniles natural-deaths-juveniles + 1
      ask loser [die] 
  ]
  ]
  [swim-juveniles]
end
    
to become-adult
  if age >= 300 [
    if random 100 >= 20 [
    hatch-adults 1 [
      set color black
      set shape "fish"
      set size 1.75 ;+ random-float 2
      set age 300
      ]
    die
  ]
  ]
end

;;-----------------------------
;;-----------------------------
;; FISHERMEN PROCEDURES
;;-----------------------------
;;-----------------------------

;;-----------------------------
;; FISHERMEN "FISHING" FUNCTIONS
;;-----------------------------

to fish-adults                       
  if random 100 <= 90                  ;;----------modelled on trawling, very high success rate
    [
        if any? adults-here [
          let target-adult count adults in-radius 1
          ask adults in-radius 1 [
            die
          ]
          set fishing-deaths-adults fishing-deaths-adults + target-adult
          set total-catch total-catch + target-adult
          set catch catch + target-adult
          set adults-caught adults-caught + target-adult
          set adult-total-deaths adult-total-deaths + target-adult
        ]
    ]
    boat-move
end 

to fish-prey                       
  if random 100 <= 90                  ;;----------modelled on trawling, very high success rate
    [
        if any? prey-here [
          let target-prey count prey in-radius 1
          ask prey in-radius 1 [
            die
          ]
          set fishing-deaths-prey fishing-deaths-prey + target-prey
          set total-catch total-catch + target-prey
          set catch catch + target-prey
          set prey-caught prey-caught + target-prey
          set prey-total-deaths prey-total-deaths + 1
        ]
    ]
    boat-move
end 

to fish-juveniles
  if random 100 <= 60                  ;;----------modelled on trawling, very high success rate, but lower due to size
    [
        if any? juveniles-here [
          let target-juv count juveniles in-radius 1
          ask juveniles in-radius 1 [
            die
          ]
          set fishing-deaths-juveniles fishing-deaths-juveniles + target-juv
          set total-catch total-catch + target-juv
          set catch catch + target-juv
          set juveniles-caught juveniles-caught + target-juv
          set juvenile-total-deaths juvenile-total-deaths + 1
        ]
    ]
    boat-move
end

;;-----------------------------
;; FISHERMEN GO FUNCTIONS
;;-----------------------------

to fish
    if Adults-Only? [
      fish-adults ;------------this will make them just catch adults
      ]
    if Prey-Only? [
      fish-prey ;------------this will make them just catch prey
    ]
    if Adults-And-Prey? [
      fish-adults ;----------this will make them catch both prey and adults
      fish-prey
    ]
    if Fish-All? [
      fish-adults
      fish-prey
      fish-juveniles ;-------this will make them catch everything
    ]
  
end
    
  
to check-catch-limit ;------------------------checks if fisherman have hit their TAC
  if catch >= 50 [
    dropoff
  ]
  if Adults-Only? [
    fish-adults 
    ]
  if Prey-Only? [
    fish-prey
    ]
  if Adults-And-Prey? [
    fish-adults
    fish-prey
    ]
  if Fish-All? [
    fish-adults
    fish-prey
    fish-juveniles
    ]
end


to dropoff  
  let location false                  
  ask patch-here[
    if  pcolor = lime                 
    [ set location true
    ]]
    ifelse location != true [
      return-to-port
    ]
    [
      if Adults-Only? [
        fish-adults
      ]
      if Prey-Only? [
        fish-prey
      ]
      if Adults-And-Prey? [
        fish-adults
        fish-prey
      ]
      if Fish-All? [
        fish-adults
        fish-prey
        fish-juveniles
      ]
    ]    
end 

to return-to-port  ;;-------------------------------------instructions to have fisherman return to port
  let x 0                   
  let y 0
  let candidates one-of patches with [pcolor = lime]
  if candidates != nobody
    [ ask one-of patches with [pcolor = lime]
      [
        set x pxcor
        set y pycor
      ]
    facexy x y
    let wall? false
    ask patch-ahead 1[
      if(pcolor = grey - 2)
        [
          set wall? true
        ] 
    ] 
    ifelse (wall?)
      [
        rt random 360         
        boat-move

      ]
      [ fd 1
        ]]
    set catch 0  ;;--------------------------------------resets catch
end 

      
;;-----------------------------
;;-----------------------------
;; GENERAL GO PROCEDURES
;;-----------------------------
;;-----------------------------


to move            ;;--------------------general move procedure for all wildlife
  rt random 360
  
  let wall? false
  ask patch-ahead 1[
    if (pcolor = grey - 2)
    [
      set wall? true
    ]
    if (pcolor = lime)
    [ 
      set wall? true
    ]
  ] 
  ifelse (wall?)
  [
    rt random 360
    move
  ]
  
  [forward 1  ]
end 

to boat-move      ;;--------------------general move procedure for boats
  let wall? false
  ask patch-ahead 1 [
    if (pcolor = grey - 2)
    [
      set wall? true
    ]
  ]
  ifelse (wall?)
  [
    rt random 90
    boat-move
  ]
  
  [forward 1]
end

to check-breeding ;;--------------------check procedure for breeding period for fish
  if clock mod 100 = 0 [
    set time-to-breed true
    set sexy-time clock
  ]
  if clock = sexy-time + 15
  [ set time-to-breed false
  ]
end

to check-season ;;----------------------check procedure for harvesting season for boats
  if clock mod 100 = 0 [
    set time-to-fish true
    set fish-time clock
  ]
  if clock = fish-time + 50 [
    set time-to-fish false
  ]
  if time-to-fish = true [
    boat-move
    fish
    check-catch-limit
  ]
  if time-to-fish = false [
    return-to-port
  ]
end
    
@#$#@#$#@
GRAPHICS-WINDOW
588
10
1337
780
50
50
7.32
1
10
1
1
1
0
1
1
1
-50
50
-50
50
0
0
1
ticks
30.0

BUTTON
450
297
575
330
Create World
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
449
335
573
368
Start
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
5
392
372
571
Population Size
Time
Population Size
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Adults" 1.0 0 -2674135 true "" "plot count adults"
"Prey" 1.0 0 -13345367 true "" "plot count prey"
"Juveniles" 1.0 0 -7500403 true "" "plot count juveniles"

MONITOR
265
111
406
156
Juv. Deaths (Cannibal)
cannibal-deaths
17
1
11

MONITOR
5
59
116
104
Total # Adults
count adults
17
1
11

MONITOR
5
10
117
55
Total # Prey
count prey
17
1
11

MONITOR
5
109
115
154
Total # Juv.
count juveniles
17
1
11

MONITOR
124
60
258
105
Adult Deaths (Natural)
natural-deaths-adults
17
1
11

MONITOR
123
10
258
55
Prey Deaths (Natural)
natural-deaths-prey
17
1
11

MONITOR
266
10
409
55
Prey Deaths (Eaten)
prey-deaths-eaten
17
1
11

MONITOR
123
110
258
155
Juv. Deaths (Natural)
natural-deaths-juveniles
17
1
11

MONITOR
266
60
408
105
Adult Deaths (Fishing)
fishing-deaths-adults
17
1
11

MONITOR
7
280
140
325
Fishing Mort. (Adult)
(fishing-deaths-adults / count(adults))
17
1
11

MONITOR
139
233
280
278
Total Catch 
total-catch
17
1
11

SWITCH
376
378
557
411
Fishery?
Fishery?
1
1
-1000

PLOT
7
577
507
762
Death Rates
Time
Death Rate
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Natural Deaths (Prey)" 1.0 0 -2674135 true "" "plot natural-deaths-prey"
"Natural Deaths (Adults)" 1.0 0 -955883 true "" "plot natural-deaths-adults"
"Natural Deaths (Juv.)" 1.0 0 -11881837 true "" "plot natural-deaths-juveniles"
"Prey Death (Eaten)" 1.0 0 -13345367 true "" "plot prey-deaths-eaten"
"Juv. Deaths (Cannibalism)" 1.0 0 -8630108 true "" "plot cannibal-deaths"
"Adult Deaths (Fishing)" 1.0 0 -4079321 true "" "plot fishing-deaths-adults"
"Juv. Deaths (Fishing)" 1.0 0 -10402772 true "" "plot fishing-deaths-juveniles"
"Prey Deaths (Fishing)" 1.0 0 -1264960 true "" "plot fishing-deaths-prey"

SWITCH
375
417
556
450
Adults-Only?
Adults-Only?
0
1
-1000

SWITCH
374
456
556
489
Prey-Only?
Prey-Only?
1
1
-1000

SWITCH
375
496
557
529
Adults-And-Prey?
Adults-And-Prey?
1
1
-1000

SWITCH
376
536
558
569
Fish-All?
Fish-All?
0
1
-1000

MONITOR
416
10
555
55
Prey Deaths (Fishing)
fishing-deaths-prey
17
1
11

MONITOR
415
111
556
156
Juv. Deaths (Fishing)
fishing-deaths-juveniles
17
1
11

MONITOR
7
328
140
373
Total Catch (Adult)
adults-caught
17
1
11

MONITOR
143
328
272
373
Total Catch (Prey)
prey-caught
17
1
11

MONITOR
276
328
434
373
Total Catch (Juveniles)
juveniles-caught
17
1
11

MONITOR
142
281
272
326
Fishing Mort. (Prey)
(fishing-deaths-prey / count(prey))
17
1
11

MONITOR
276
280
434
325
Fishing Mort. (Juveniles)
(fishing-deaths-juveniles / count(juveniles))
17
1
11

MONITOR
415
60
556
105
Adult Deaths (Starvation)
starvation-deaths
17
1
11

MONITOR
124
161
258
206
Juv. Deaths (Total)
juvenile-total-deaths
17
1
11

MONITOR
265
161
405
206
Adult Deaths (Total)
adult-total-deaths
17
1
11

MONITOR
415
161
556
206
Prey Deaths (Total)
prey-total-deaths
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

boat
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 158 33 230 157 182 150 169 151 157 156
Polygon -7500403 true true 149 55 88 143 103 139 111 136 117 139 126 145 130 147 139 147 146 146 149 55

boat top
true
0
Polygon -7500403 true true 150 1 137 18 123 46 110 87 102 150 106 208 114 258 123 286 175 287 183 258 193 209 198 150 191 87 178 46 163 17
Rectangle -16777216 false false 129 92 170 178
Rectangle -16777216 false false 120 63 180 93
Rectangle -7500403 true true 133 89 165 165
Polygon -11221820 true false 150 60 105 105 150 90 195 105
Polygon -16777216 false false 150 60 105 105 150 90 195 105
Rectangle -16777216 false false 135 178 165 262
Polygon -16777216 false false 134 262 144 286 158 286 166 262
Line -16777216 false 129 149 171 149
Line -16777216 false 166 262 188 252
Line -16777216 false 134 262 112 252
Line -16777216 false 150 2 149 62

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

fish 3
false
0
Polygon -7500403 true true 137 105 124 83 103 76 77 75 53 104 47 136
Polygon -7500403 true true 226 194 223 229 207 243 178 237 169 203 167 175
Polygon -7500403 true true 137 195 124 217 103 224 77 225 53 196 47 164
Polygon -7500403 true true 40 123 32 109 16 108 0 130 0 151 7 182 23 190 40 179 47 145
Polygon -7500403 true true 45 120 90 105 195 90 275 120 294 152 285 165 293 171 270 195 210 210 150 210 45 180
Circle -1184463 true false 244 128 26
Circle -16777216 true false 248 135 14
Line -16777216 false 48 121 133 96
Line -16777216 false 48 179 133 204
Polygon -7500403 true true 241 106 241 77 217 71 190 75 167 99 182 125
Line -16777216 false 226 102 158 95
Line -16777216 false 171 208 225 205
Polygon -1 true false 252 111 232 103 213 132 210 165 223 193 229 204 247 201 237 170 236 137
Polygon -1 true false 135 98 140 137 135 204 154 210 167 209 170 176 160 156 163 126 171 117 156 96
Polygon -16777216 true false 192 117 171 118 162 126 158 148 160 165 168 175 188 183 211 186 217 185 206 181 172 171 164 156 166 133 174 121
Polygon -1 true false 40 121 46 147 42 163 37 179 56 178 65 159 67 128 59 116

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
