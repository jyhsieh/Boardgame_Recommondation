Board Game
================
Jui-Ying Hsieh, Li-Hsin Chen

1. Background
-------------

2. Problem Statement
--------------------

3. Method
---------

#### 3.1 Expand Mechanic and Category Variables

Variables <span style="color:red">Mechanic</span> and <span style="color:red">Category</span>

``` r
colnames(boardgame_cluster)[1:20]
```

    ##  [1] "Action / Movement Programming" "Co-operative Play"            
    ##  [3] "Grid Movement"                 "Hand Management"              
    ##  [5] "Modular Board"                 "Role Playing"                 
    ##  [7] "Simultaneous Action Selection" "Storytelling"                 
    ##  [9] "Variable Player Powers"        "Action Point Allowance System"
    ## [11] "Point to Point Movement"       "Set Collection"               
    ## [13] "Trading"                       "Auction/Bidding"              
    ## [15] "Card Drafting"                 "Area Control / Area Influence"
    ## [17] "Campaign / Battle Card Driven" "Dice Rolling"                 
    ## [19] "Tile Placement"                "Area Movement"

``` r
head(boardgame_cluster[,1:5])
```

    ##   Action / Movement Programming Co-operative Play Grid Movement
    ## 1                             1                 1             1
    ## 2                             0                 1             0
    ## 3                             0                 0             0
    ## 4                             0                 0             0
    ## 5                             0                 0             0
    ## 6                             0                 0             0
    ##   Hand Management Modular Board
    ## 1               1             1
    ## 2               1             0
    ## 3               0             0
    ## 4               1             0
    ## 5               1             0
    ## 6               1             0

4. Result
---------

5. Future Work
--------------
