
#Use Hockey Reference to find total shots on a given date
library(rvest)
library(data.table)
library(tidyverse)



##### INPUTS ##############
Start_Date = "2018-10-03" 
End_Date   = "2019-04-07"   
###########################

#Create list of Dates between start_date and end_date
getDateList <- function(start_date, end_date){
  temp_list <- list()
  temp_dates <- seq(as.Date(start_date), as.Date(end_date), "days")
  
  temp_list <- list()
  for(i in 1:length(temp_dates)){
    temp_list[[i]] =
      data.frame(Year = substr(temp_dates[i], 1, 4),
                 Month = substr(temp_dates[i], 6, 7),
                 Day = substr(temp_dates[i], 9, 10))
    names(temp_list)[i] <- paste0(substr(temp_dates[i], 1, 4), 
                                  substr(temp_dates[i], 6, 7),
                                  substr(temp_dates[i], 9, 19) 
    )
  }
  return(temp_list)
}




#Write the link to boxscores of all games on each date between start_date and end_date
getDateLinks <- function(x){
  cbind(x, "Link" = paste0("https://www.basketball-reference.com/boxscores/?",
                           "year=",x[["Year"]],
                           "&month=",x[["Month"]],
                           "&day=",x[["Day"]])
  )
  
}



#get link for each boxscore of each game of each day between start_date and end_date
getGameLinks <- function(x){
  links_temp <-
    html_attr(html_nodes(read_html(as.character(x[["Link"]])), "a"), "href") %>% 
    as_tibble() %>% 
    filter(grepl(paste0("/boxscores/",x[["Year"]], 
                        x[["Month"]], 
                        x[["Day"]]), value)==TRUE) %>% 
    as.data.frame()
}


getStats <- function(x){
  boxscore = paste0("https://www.basketball-reference.com", x)
  
  #Get all tables
  players_extract <-
    boxscore %>% 
    read_html() %>% html_nodes(xpath = "//table")
  
  
  
  Away_Team <- toupper(gsub("_basic", "", gsub("box_", "", html_attrs(players_extract)[[1]][[2]])))
  Home_Team <- toupper(gsub("_basic", "", gsub("box_", "", html_attrs(players_extract)[[3]][[2]])))
  
  #Regular Stats
  stats_t1  <- as.data.table(html_table(players_extract[[1]]))
  colnames(stats_t1) <- gsub("Basic Box Score Stats", "", paste0(colnames(stats_t1), stats_t1[1,]))
  colnames(stats_t1)[1] <- "Players"
  stats_t1$Team <- Away_Team
  stats_t1$Game <- paste0(Away_Team, " @ ", Home_Team) 
  stats_t1 <-  stats_t1[!(Players %in% c("Reserves", "Starters", "Team Totals")),,]
  setkey(stats_t1, "Players", "MP", "Team", "Game")
  
  stats_t2  <- as.data.table(html_table(players_extract[[3]]))
  colnames(stats_t2) <- gsub("Basic Box Score Stats", "", paste0(colnames(stats_t2), stats_t2[1,]))
  colnames(stats_t2)[1] <- "Players"
  stats_t2$Team <- Home_Team
  stats_t2$Game <- paste0(Away_Team, " @ ", Home_Team)
  stats_t2 <-  stats_t2[!(Players %in% c("Reserves", "Starters", "Team Totals")),,]
  setkey(stats_t2, "Players", "MP", "Team", "Game")
  
  
  #Advanced Stats
  adv_stats_t1  <- as.data.table(html_table(players_extract[[2]]))
  colnames(adv_stats_t1) <- gsub("Advanced Box Score Stats", "", paste0(colnames(adv_stats_t1), adv_stats_t1[1,]))
  colnames(adv_stats_t1)[1] <- "Players"
  adv_stats_t1$Team <- Away_Team
  adv_stats_t1$Game <- paste0(Away_Team, " @ ", Home_Team)
  adv_stats_t1 <-  adv_stats_t1[!(Players %in% c("Reserves", "Starters", "Team Totals")),,]
  setkey(adv_stats_t1, "Players", "MP", "Team", "Game")
  
  adv_stats_t2  <- as.data.table(html_table(players_extract[[4]]))
  colnames(adv_stats_t2) <- gsub("Advanced Box Score Stats", "", paste0(colnames(adv_stats_t2), adv_stats_t2[1,]))
  colnames(adv_stats_t2)[1] <- "Players"
  adv_stats_t2$Team <- Home_Team
  adv_stats_t2$Game <- paste0(Away_Team, " @ ", Home_Team)
  adv_stats_t2 <-  adv_stats_t2[!(Players %in% c("Reserves", "Starters", "Team Totals")),,]
  setkey(adv_stats_t2, "Players", "MP", "Team", "Game")
  
  
  
  #Combine Stats
  t1 <- stats_t1[adv_stats_t1]
  t2 <- stats_t2[adv_stats_t2]
  
  
  stats_table <- rbind(t1, t2)
  
}


GetStatsTable <- function(){
  
  links_out <- lapply(lapply(getDateList(Start_Date, End_Date), getDateLinks), getGameLinks)
  
  final_list <- list()
  for(i in 1:length(links_out)){
    if(length(links_out[[i]][,]) ==0) next
    final_list[[i]] <- lapply(links_out[[i]][,], getStats)
    final_list[[i]] <- do.call(rbind, final_list[[i]])
    final_list[[i]]$Date <- names(links_out)[i]
    names(final_list)[i] <- names(links_out)[i]
  }
  
  final_shots <- do.call(rbind, final_list)
  return(final_shots)
}


Extract <- GetStatsTable()
