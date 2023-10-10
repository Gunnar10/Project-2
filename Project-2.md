Project-2
================
Jacob Press
2023-10-09

# Intall Packages

Below I am installing all the necessary packages.

``` r
library(jsonlite)
library(tidyverse)
```

# Functions to Quiry API

Here I made a list of the `teamNames` corrisponding to their `teamID`.

``` r
outputAPI1 <- fromJSON("https://records.nhl.com/site/api/franchise-season-results")
  list <- outputAPI1$data  
  teamID<- list %>% select(teamId, teamName) %>% arrange(teamId) %>% distinct(teamId, teamName)
  teamID
```

    ##    teamId                teamName
    ## 1       1       New Jersey Devils
    ## 2       2      New York Islanders
    ## 3       3        New York Rangers
    ## 4       4     Philadelphia Flyers
    ## 5       5     Pittsburgh Penguins
    ## 6       6           Boston Bruins
    ## 7       7          Buffalo Sabres
    ## 8       8      Montréal Canadiens
    ## 9       9         Ottawa Senators
    ## 10     10     Toronto Maple Leafs
    ## 11     11       Atlanta Thrashers
    ## 12     12     Carolina Hurricanes
    ## 13     13        Florida Panthers
    ## 14     14     Tampa Bay Lightning
    ## 15     15     Washington Capitals
    ## 16     16      Chicago Blackhawks
    ## 17     17       Detroit Red Wings
    ## 18     18     Nashville Predators
    ## 19     19         St. Louis Blues
    ## 20     20          Calgary Flames
    ## 21     21      Colorado Avalanche
    ## 22     22         Edmonton Oilers
    ## 23     23       Vancouver Canucks
    ## 24     24           Anaheim Ducks
    ## 25     25            Dallas Stars
    ## 26     26       Los Angeles Kings
    ## 27     27         Phoenix Coyotes
    ## 28     28         San Jose Sharks
    ## 29     29   Columbus Blue Jackets
    ## 30     30          Minnesota Wild
    ## 31     31   Minnesota North Stars
    ## 32     32        Quebec Nordiques
    ## 33     33    Winnipeg Jets (1979)
    ## 34     34        Hartford Whalers
    ## 35     35        Colorado Rockies
    ## 36     36  Ottawa Senators (1917)
    ## 37     37         Hamilton Tigers
    ## 38     38      Pittsburgh Pirates
    ## 39     39    Philadelphia Quakers
    ## 40     40         Detroit Cougars
    ## 41     41      Montreal Wanderers
    ## 42     42         Quebec Bulldogs
    ## 43     43        Montreal Maroons
    ## 44     44      New York Americans
    ## 45     45        St. Louis Eagles
    ## 46     46           Oakland Seals
    ## 47     47          Atlanta Flames
    ## 48     48      Kansas City Scouts
    ## 49     49        Cleveland Barons
    ## 50     50         Detroit Falcons
    ## 51     51      Brooklyn Americans
    ## 52     52           Winnipeg Jets
    ## 53     53         Arizona Coyotes
    ## 54     54    Vegas Golden Knights
    ## 55     55          Seattle Kraken
    ## 56     56 California Golden Seals
    ## 57     57          Toronto Arenas
    ## 58     58    Toronto St. Patricks

Below is the list of the team 1 New Jersey Devils  
2 New York Islanders  
3 New York Rangers  
4 Philadelphia Flyers  
5 Pittsburgh Penguins  
6 Boston Bruins  
7 Buffalo Sabres  
8 Montréal Canadiens  
9 Ottawa Senators  
10 Toronto Maple Leafs  
11 Atlanta Thrashers  
12 Carolina Hurricanes  
13 Florida Panthers  
14 Tampa Bay Lightning  
15 Washington Capitals  
16 Chicago Blackhawks  
17 Detroit Red Wings  
18 Nashville Predators  
19 St. Louis Blues  
20 Calgary Flames  
21 Colorado Avalanche  
22 Edmonton Oilers  
23 Vancouver Canucks  
24 Anaheim Ducks  
25 Dallas Stars  
26 Los Angeles Kings  
27 Phoenix Coyotes  
28 San Jose Sharks  
29 Columbus Blue Jakets  
30 Minnesota Wild  
31 Minnesota NorthStars  
32 Quebec Nordiques  
33 Winnipeg Jets (1979)  
34 Hartford Whalers  
35 Colorado Rockies  
36 Ottawa Senators (1917)  
37 Hamilton Tigers  
38 Pittsburgh Pirates  
39 Philadelphia Quakers  
40 Detroit Cougars  
41 Montreal Wanderers  
42 Quebec Bulldogs  
43 Montreal Maroons  
44 New York Americans  
45 St. Louis Eagles  
46 Oakland Seals  
47 Atlanta Flames  
48 Kansas City Scouts  
49 Cleveland Barons  
50 Detroit Falcons  
51 Brooklyn Americans  
52 Winnipeg Jets  
53 Arizona Coyotes  
54 Vegas Golden Knights  
55 Seattle Kraken  
56 California Golden Seals  
57 Toronto Arenas  
58 Toronto St. Patricks

``` r
# total home wins and losses
allTeamTotals <- function(){
  output <- fromJSON("https://records.nhl.com/site/api/franchise-team-totals")
  return(output$data)
}

teamWinLossTotals <- function(team = 12, game = 2){
  baseURL <- "https://records.nhl.com/site/api/"
  filterURL1 <- "franchise-team-totals?cayenneExp=teamId="
  fitlerURL2 <- "%20and%20gameTypeId="
  fullURL <- paste0(baseURL, filterURL1, team, fitlerURL2, game)
  outputAPI <- fromJSON(fullURL)
    output <- outputAPI$data %>% select(teamName, contains("wins"), homeLosses, roadLosses, losses, gamesPlayed, gameWinPctg)
    return(output)
}
  
wlTotalBySeason <- function(){
  outputAPI <- fromJSON("https://records.nhl.com/site/api/franchise-season-results")
    output <- outputAPI$data %>% arrange(teamId)
    return(output)
}  

teamWLTotalBySeason <- function(team = 12, game = 2){
  baseURL <- "https://records.nhl.com/site/api/"
  filterURL1 <- "franchise-season-results?cayenneExp=teamId="
  fitlerURL2 <- "%20and%20gameTypeId="
  fullURL <- paste0(baseURL, filterURL1, team, fitlerURL2, game)
  outputAPI <- fromJSON(fullURL)
    output <- outputAPI$data %>% select(teamName, seasonId, contains("wins"), homeLosses, roadLosses, losses, gamesPlayed)
    return(output)
}
```

rmarkdown::render(input = “Project-2.Rmd”, output_format =
“github_document”, output_file = “README.md”)
