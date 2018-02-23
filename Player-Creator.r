# fill up teams with random players #
setUp <- function(){
  
# set up first names table
first_names <- read.csv("C:/Users/Tim/Documents/male-first-names.csv")
first_names$name <- as.character(first_names$name)
first_names$name <- paste(substr(first_names$name, 0, 1), 
                          substr(tolower(first_names$name), 2, 100), sep = "")
first_names$perc <- first_names$perc / 100

# set up last names table
last_names <- read.csv("C:/Users/Tim/Documents/last-names.csv")[,1:2]
names(last_names) <- c("name", "perc")
last_names$name <- as.character(last_names$name)
last_names$name <- paste(substr(last_names$name, 0, 1), 
                          substr(tolower(last_names$name), 2, 100), sep = "")
last_names$perc <- as.numeric(substr(last_names$perc, 0, 4)) / 100

# create teams
name <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia", "San Antonio",
          "San Diego", "Dallas", "San Jose", "Jacksonville", "San Francisco",
          "Indianapolis", "Columbus", "Charlotte", "Seattle", "Nashville", "El Paso", "Denver",
          "Washington", "Boston", "Detroit", "Portland", "Oklahoma City", "Las Vegas",
          "Baltimore", "Louisville", "Milwaukee", "Atlanta", "Miami")
cid <- 1:30
assign("city", data.frame(cid, name, stringsAsFactors = FALSE), envir = .GlobalEnv)

# fill teams with players
library(sn)
pid <- 1:360
first <- sample(first_names$name, prob = first_names$perc, size = 360)
last <- sample(last_names$name, prob = last_names$perc, size = 360)
overall <- rsn(n=360, xi=40, omega=17, alpha=5)
overall[overall>100]=100
heightInches <- rsn(n=360, xi = 83, omega = 3, alpha = -2)
player <- data.frame(pid, first, last, heightInches, overall)[order(-overall),]

cid <- c()
for(i in 1:6){
  cid <- append(1:30, sort(1:30, decreasing = TRUE))
}
player$cid <- cid
assign("player", player, envir = .GlobalEnv)
}

# view a team's roster
roster <- function(cid_){
  r <- subset(player, cid == cid_)
  View(r, city[cid==cid_,2])
}