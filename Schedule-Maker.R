# TO-DO LIST:
# - FIX THE GENGAME FUNCTION TO AUTOMATICALLY ASSIGN VALUES
# - GENERATE PLAYOFF GAMES
# - CHECK TO SEE IF AUTOMATIC PLAYOFF SCHEDULING OCCURS AT END OF SEASON

# generates the season, updating the schedule and standings
genSeason <- function(){
  # prepare for regular season
  genNewStandings()
  genNewSchedule()
  # simulate regular season
  l <- nrow(SPA_Schedule)
  for(i in 1:l){
    genGame()
  }
}

# TODO
# genPlayoffs <- function(){
#  SPA_Schedule <- genNewPlayoffSchedule()
#  for(i in )
#}

# generates the Eastern Conference standings
# -> data frame
genEastStandings <- function(){
  East_Standings <- SPA_Standings[SPA_Standings$Conference=='East',c(1,2,4,5,6,7,8,9)]
  East_Standings$Rank <- 1:8
  return (East_Standings)
}

# generates the Western Conference standings
# -> data frame
genWestStandings <- function(){
  West_Standings <- SPA_Standings[SPA_Standings$Conference=='West',c(1,2,4,5,6,7,8,9)]
  West_Standings$Rank <- 1:8
  return (West_Standings)
}

# generates the Northeast division standings
# -> data frame
genNortheastStandings <- function(){
  Northeast_Standings <- SPA_Standings[SPA_Standings$Division=='Northeast',c(1,2,5,6,7,8,9)]
  Northeast_Standings$Rank <- 1:4
  return (Northeast_Standings)
}

# generates the Southeast division standings
# -> data frame
genSoutheastStandings <- function(){
  Southeast_Standings <- SPA_Standings[SPA_Standings$Division=='Southeast',c(1,2,5,6,7,8,9)]
  Southeast_Standings$Rank <- 1:4
  return (Southeast_Standings)
}

# generates the Northwest division standings
# -> data frame
genNorthwestStandings <- function(){
  Northwest_Standings <- SPA_Standings[SPA_Standings$Division=='Northwest',c(1,2,5,6,7,8,9)]
  Northwest_Standings$Rank <- 1:4
  return (Northwest_Standings)
}

# generates the Southwest division standings
# -> data frame
genSouthwestStandings <- function(){
  Southwest_Standings <- SPA_Standings[SPA_Standings$Division=='Southwest',c(1,2,5,6,7,8,9)]
  Southwest_Standings$Rank <- 1:4
  return (Southwest_Standings)
}

Team <- c('Boston','New York','Philadelphia','Detroit','Miami','Tallahassee','Atlanta',
          'New Orleans','Denver','San Francisco','Seattle','Sioux Falls','Phoenix',
          'Los Angeles','San Diego','El Paso')

# generates the league's standings
# -> data frame
genNewStandings <- function(){
  Rank <- 1:16
  Conference <- append(rep('East', times = 8), rep('West', times = 8))
  Division <- append(append(append(rep('Northeast', times = 4), rep('Southeast', times = 4)),
                            rep('Northwest', times = 4)), rep('Southwest', times = 4))
  Wins <- rep(0, times = 16)
  Losses <- rep(0, times = 16)
  Win_Percentage <- rep(0, times = 16)
  Streak <- rep(0, times = 16)
  Rating <- rep(0, times = 16)
  standings <- data.frame(Rank, Team, Conference, Division, Wins, Losses, Win_Percentage,
                              Rating, Streak,stringsAsFactors = FALSE)
  assign('SPA_Standings',standings,envir=.GlobalEnv)
}

# determines the schedule
# -> data frame
genNewSchedule <- function(){
  Game <- sort(rep(1:10, times = 8), decreasing = FALSE)
  Home_Score <- rep('DNP', times = 80)
  Away_Score <- rep('DNP', timems = 80)
  Home <- c()
  Away <- c()
  
  for (i in 1:10){
    teams <- sample(Team)
    homeTeams <- teams[1:8]
    awayTeams <- teams[9:16]
    for(j in 1:8){
      Home <- append(Home,homeTeams[j])
      Away <- append(Away,awayTeams[j])
    }
  }
  
  schedule <- data.frame(Game,Home,Home_Score,Away,Away_Score,stringsAsFactors = FALSE)
  SPA_Schedule <<- schedule
  assign('SPA_Schedule',schedule,envir=.GlobalEnv)
}



# determines the playoff schedule
# -> data frame
genNewPlayoffSchedule <- function(){
  east8 <- genEastStandings()[1:4,]$Team
  west8 <- genWestStandings()[1:4,]$Team
  hom <- c()
  away <- c()
  home <- c()
  home_score <- c()
  away_score <- c()
  for(i in 1:2){
    hom <- append(hom,east8[1],after=length(hom))
    away <- append(away,east8[length(east8)],after=length(away))
    hom <- append(hom,west8[1],after=length(hom))
    away <- append(away,west8[length(west8)],after=length(away))
    
    east8 <- east8[-1]
    east8 <- east8[-(length(east8))]
    west8 <- west8[-1]
    west8 <- west8[-(length(west8))]
  }
  
  home <- append(hom,away)
  away <- append(away,hom)
  
  Home <- append(home, home)
  Away <- append(away, away)
  
  Game <- sort(rep(1:4, times = 4), decreasing = FALSE)
  Home_Score <- rep('DNP', times= 4*4)
  Away_Score <- rep('DNP', times= 4*4)
  schedule <- data.frame(Game,Home,Home_Score,Away,Away_Score)
  assign('Playoff.Teams',Home,envir = .GlobalEnv)
  return (schedule)
}


# generates the next game between two teams
# Team Team -> Integer
genGame <- function(){
  # if there are games to play
  if(any(SPA_Schedule$Home_Score=='DNP')){
    rows <- nrow(SPA_Schedule)
    # loop through each row of the schedule...
    for(i in 1:rows){
      # ...until you find one that is DNP
      if (SPA_Schedule$Home_Score[i]=='DNP'){
        # create varibles for the home and away team
        h <- SPA_Standings[SPA_Standings$Team==SPA_Schedule$Home[i],]$Team
        a <- SPA_Standings[SPA_Standings$Team==SPA_Schedule$Away[i],]$Team
        
        # change the schedule with the scores of the game
        SPA_Schedule$Home_Score[i] <- round(rnorm(1,105 + SPA_Standings[
          SPA_Standings$Team==h,]$Rating,12))
        SPA_Schedule$Away_Score[i] <- round(rnorm(1,105 + SPA_Standings[
          SPA_Standings$Team==a,]$Rating,12))
        
        # CHANGE THE STANDINGS
        
        # if the home team wins
        if (as.numeric(SPA_Schedule$Home_Score[i]) >= as.numeric(SPA_Schedule$Away_Score[i])){
          
          # add a win to the home team
          SPA_Standings[SPA_Standings$Team==h,]$Wins <- 
            SPA_Standings[SPA_Standings$Team==h,]$Wins + 1
          
          # add a loss to the away team
          SPA_Standings[SPA_Standings$Team==a,]$Losses <- 
            SPA_Standings[SPA_Standings$Team==a,]$Losses + 1
          
          # if the home team was on a win streak
          if (SPA_Standings[SPA_Standings$Team==h,]$Streak > 0){
            SPA_Standings[SPA_Standings$Team==h,]$Streak <- 
              SPA_Standings[SPA_Standings$Team==h,]$Streak + 1
          }
          
          # if the home team was on a losing streak
          else{
            SPA_Standings[SPA_Standings$Team==h,]$Streak <- 1
          }
          
          # if the away team was on a losing streak
          if (SPA_Standings[SPA_Standings$Team==a,]$Streak < 0){
            SPA_Standings[SPA_Standings$Team==a,]$Streak <- 
              SPA_Standings[SPA_Standings$Team==a,]$Streak - 1
          }
          
          # if the away team was on a win streak
          else{
            SPA_Standings[SPA_Standings$Team==a,]$Streak <- -1
          }
        }
        
        # if the away team wins
        else if(as.numeric(SPA_Schedule$Home_Score[i]) < 
                as.numeric(SPA_Schedule$Away_Score[i])){
          
          # add a win to the away team
          SPA_Standings[SPA_Standings$Team==a,]$Wins <- 
            SPA_Standings[SPA_Standings$Team==a,]$Wins + 1
          
          # add a loss to the home team
          SPA_Standings[SPA_Standings$Team==h,]$Losses <- 
            SPA_Standings[SPA_Standings$Team==h,]$Losses + 1
          
          # if the away team was on a win streak
          if (SPA_Standings[SPA_Standings$Team==a,]$Streak > 0){
            
            SPA_Standings[SPA_Standings$Team==a,]$Streak <- 
              SPA_Standings[SPA_Standings$Team==a,]$Streak + 1
          }
          
          # if the away team was on a losing streak
          else{
            SPA_Standings[SPA_Standings$Team==a,]$Streak <- 1
          }
          
          # if the home team was on a losing streak
          if (SPA_Standings[SPA_Standings$Team==h,]$Streak < 0){
            
            SPA_Standings[SPA_Standings$Team==h,]$Streak <-
              SPA_Standings[SPA_Standings$Team==h,]$Streak - 1
          }
          
          # if the home team was on a win streak
          else{
            SPA_Standings[SPA_Standings$Team==h,]$Streak <- -1
          }
        }
        
        # change both teams' win percentage
        SPA_Standings[SPA_Standings$Team==h,]$Win_Percentage <- 
          SPA_Standings[SPA_Standings$Team==h,]$Wins / 
          (SPA_Standings[SPA_Standings$Team==h,]$Losses + 
             SPA_Standings[SPA_Standings$Team==h,]$Wins)
        
        SPA_Standings[SPA_Standings$Team==a,]$Win_Percentage <- 
          SPA_Standings[SPA_Standings$Team==a,]$Wins / 
          (SPA_Standings[SPA_Standings$Team==a,]$Losses + 
             SPA_Standings[SPA_Standings$Team==a,]$Wins)
        
        # change both teams' rating
        SPA_Standings[SPA_Standings$Team==h,]$Rating <- 
          SPA_Standings[SPA_Standings$Team==h,]$Wins - 
          SPA_Standings[SPA_Standings$Team==h,]$Losses +
          SPA_Standings[SPA_Standings$Team==h,]$Streak
        
        SPA_Standings[SPA_Standings$Team==a,]$Rating <- 
          SPA_Standings[SPA_Standings$Team==a,]$Wins - 
          SPA_Standings[SPA_Standings$Team==a,]$Losses +
          SPA_Standings[SPA_Standings$Team==a,]$Streak
        
        # sort the updated standings
        SPA_Standings <- SPA_Standings[order(-SPA_Standings$Win_Percentage,
                                             -SPA_Standings$Wins,-SPA_Standings$Rating),]
        SPA_Standings$Rank <- 1:length(Team)
        
        # only simulate one game
        break()
      }
      
      # if its the end of the regular season
      if(i==rows-1){
        SPA_Schedule <- rbind(SPA_Schedule,genNewPlayoffSchedule())
      }
    }
  }
  assign('SPA_Schedule',SPA_Schedule,envir = .GlobalEnv)
  assign('SPA_Standings',SPA_Standings,envir = .GlobalEnv)
}

genPlayoffGame <- function(){
  if(any(SPA_Schedule$Home_Score=='DNP')){
    rows <- nrow(SPA_Schedule)
    playoffStart <- 10*length(Team)/2+1
    # loop through each row of the schedule...
    for(i in playoffStart:rows){
      # ...until you find one that is DNP
      if (SPA_Schedule$Home_Score[i]=='DNP'){
        # create varibles for the home and away team
        h <- SPA_Standings[SPA_Standings$Team==SPA_Schedule$Home[i],]$Team
        a <- SPA_Standings[SPA_Standings$Team==SPA_Schedule$Away[i],]$Team
        
        # change the schedule with the scores of the game
        SPA_Schedule$Home_Score[i] <- round(rnorm(1,105 + SPA_Standings[
          SPA_Standings$Team==h,]$Rating,12))
        SPA_Schedule$Away_Score[i] <- round(rnorm(1,105 + SPA_Standings[
          SPA_Standings$Team==a,]$Rating,12))
        
        # change the playoff standings
        
        # if the home team wins
        if(as.numeric(SPA_Schedule$Home_Score[i]) >= as.numeric(SPA_Schedule$Away_Score[i])){
          
          # if we are still in the first round
          hwins <- Playoff_Standings[Playoff_Standings$team==h,]$First.Round
          hwins2 <- Playoff_Standings[Playoff_Standings$team==h,]$Semi.Finals
          hwins3 <- Playoff_Standings[Playoff_Standings$team==h,]$Finals

          # update win totals
          if(as.numeric(hwins) < 4){
            Playoff_Standings[Playoff_Standings$team==h,]$First.Round <- hwins + 1
          }
          else if(as.numeric(hwins2) < 4){
            Playoff_Standings[Playoff_Standings$team==h,]$Semi.Finals <- hwins2 + 1 
          }
          else if(as.numeric(hwins3) < 4){
            Playoff_Standings[Playoff_Standings$team==h,]$Finals <- hwins3 + 1
          }
          
          # if more games need to be added to schedule, do so
          if(Playoff_Standings[Playoff_Standings$team==h,]$First.Round)...........
        }
        
        # if the away team wins
        if(as.numeric(SPA_Schedule$Home_Score[i]) < as.numeric(SPA_Schedule$Away_Score[i])){
          # if we are still in the first round
          awins <- Playoff_Standings[Playoff_Standings$team==a,]$First.Round
          awins2 <- Playoff_Standings[Playoff_Standings$team==a,]$Semi.Finals
          awins3 <- Playoff_Standings[Playoff_Standings$team==a,]$Finals

          if(as.numeric(awins) < 4){
            Playoff_Standings[Playoff_Standings$team==a,]$First.Round <- awins + 1
          }
          else if(as.numeric(awins2) < 4){
            Playoff_Standings[Playoff_Standings$team==a,]$Semi.Finals <- awins2 + 1 
          }
          else if(as.numeric(awins3) < 4){
            Playoff_Standings[Playoff_Standings$team==a,]$Finals <- awins3 + 1
          }
        }
        break()
      }
    }
  }
  assign('SPA_Schedule',SPA_Schedule,envir = .GlobalEnv)
  assign('Playoff_Standings',Playoff_Standings,envir = .GlobalEnv)
}

# creates the standings for the playoff games
genNewPlayoffStandings <- function(){
  team <- Playoff.Teams[1:(length(Team)/2)]
  First.Round <- rep(0,length(team))
  Semi.Finals <- First.Round
  Finals <- First.Round
  standings <- data.frame(team,First.Round,Semi.Finals,Finals)
  assign('Playoff_Standings',standings,envir = .GlobalEnv)
}