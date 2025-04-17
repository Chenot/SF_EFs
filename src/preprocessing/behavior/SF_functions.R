
library("stringr")
library("foreach")
library("bestNormalize")
library("SciViews")
library("ggplot2")
library("ggpubr")
library("dplyr")
library("broom")

process_file <- function(input_file, path, path_clean) {
  file_to_write <- read.table(input_file, header = TRUE, sep = "\t", dec = ".", fill = TRUE)
  
  # Extract subject ID and session from filename
  filename <- basename(input_file)
  parts <- strsplit(filename, "_")[[1]]
  id <- parts[3]  # e.g., "P180"
  timestamp <- paste(parts[4], parts[5], sep = "_")
  
  # Create output filename
  output_filename <- sprintf("SpaceFortress_sub-%s_%s.txt", id, timestamp)
  
  # Process the data
  file_to_save <- clean_file(file_to_write)
  file_to_save$Pseudo <- id
  file_to_save$Date <- date_hour(filename)
  file_to_save$Session <- get_day_session(filename, path)
  
  # Write to output file
  output_path <- file.path(path_clean, output_filename)
  write.table(file_to_save, output_path, 
              row.names = TRUE, col.names = TRUE,
              sep = "\t", dec = ".",
              quote = FALSE)
  
  return(id)
}

##CLEANING SF TXT FILES & POINT COMPUTATION
date <- function(file_name){
  return(as.Date(substr(file_name, 34, 43), format="%Y-%m-%d"))
}

#' Date Hour
#'
#' @param file_name the file from which date and hour are extracted
#' @param clean false if the data are raw and true if cleaned
#'
#' @return the date and hour when the game session was played
#' @export
#'
#' @examples date_hour("SpaceFortress_AC2302_D02P2.txt" TRUE)
date_hour<-function(file_name,clean=FALSE){
  if(clean==TRUE){
    return(as.POSIXct(str_sub(file_name,22,-8),format="%Y-%m-%d_%H-%M-%S"))
  }else{
    return(as.POSIXct(str_sub(file_name,34,-5),format="%Y-%m-%d_%H-%M-%S"))
  }
}

ID<- function(file_name,clean=FALSE){
  if(clean==TRUE){
    return(toupper(substr(file_name, 15,20)))
  }else{
    return(toupper(substr(file_name, 27, 32)))
  }
}

split_MTP<-function(file_to_split){
  row_split=which(file_to_split$e1=="display_game"&file_to_split$e2=="2", arr.ind=TRUE)
  game1_file=subset(file_to_split[c(1:(row_split-1)),])
  game2_file=subset(file_to_split[c(row_split:nrow(file_to_split)),])
  return(list(game1_file,game2_file))
}

clean_file <- function(file_to_clean){ 
  #remove all empty rows
  clean_file=subset(file_to_clean,(e3!="user"&e1!="release"&system_time!=""&system_time!="n"&system_time!="y"),select=c("system_time","e1","e2","e3"))
  #clean_file=subset(file_to_clean,file_to_clean$e1=="display_game"|file_to_clean$e1=="game"|file_to_clean$e1=="press"|file_to_clean$e1=="collide"|e1=="destroyed"|e1=="warp"|e1=="score+"|e1=="timeout"|e1=="pnts_bonus_capture"|e1=="pnts_bonus_failure"|e1=="shots_bonus_capture"|e1=="shots_bonus_failure",select=c("system_time","e1","e2","e3"))
  clean_file=clean_file[complete.cases(clean_file),]
  #time trunc
  #clean_file$system_time=trunc(as.numeric(as.character(clean_file$system_time)))
  clean_file$system_time=round(as.numeric(as.character(clean_file$system_time)),digits=3)
  #file begins at game start and ends game over
  row_begin=which(clean_file$e1 == "game"& clean_file$e2=="start", arr.ind=TRUE)
  row_end=which(clean_file$e1 == "game"& clean_file$e2=="over", arr.ind=TRUE)
  clean_file=clean_file[c(row_begin:row_end),]
  #PAUSE DELETION
  if(any(clean_file$e2=="pause")){
    row_pause=which(clean_file$e2=="pause", arr.ind=TRUE)
    row_unpause=which(clean_file$e2=="unpause", arr.ind=TRUE)
    time_pause=clean_file$system_time[row_pause]
    time_unpause=clean_file$system_time[row_unpause]
    pause_duration=time_unpause-time_pause+1
    clean_file=subset(clean_file[-c(row_pause:row_unpause),])
    clean_file[(clean_file$system_time)>=time_unpause,"system_time"]=clean_file[(clean_file$system_time)>=time_unpause,"system_time"]-pause_duration
  }
  return(clean_file)
}

#write files containing the clean dataframe of each session of each participant 
write_file <- function(file, path, path_clean) {
  file_to_write <- read.table(file, header = TRUE, sep = "\t", dec = ".", fill = TRUE)
  
  # Extract subject ID and session from filename
  filename <- basename(file)
  # Assuming filename format: SpaceFortress-5.1.0_beta1_P180_2023-11-8_17-15-13.txt
  parts <- strsplit(filename, "_")[[1]]
  id <- parts[3]  # e.g., "P180"
  timestamp <- paste(parts[4], parts[5], sep = "_")
  
  # Create output filename following BIDS-like format
  output_filename <- sprintf("SpaceFortress_sub-%s_%s.txt", id, timestamp)
  
  # Process the data
  session <- get_day_session(filename, path)
  file_to_save <- clean_file(file_to_write)
  file_to_save$Pseudo <- id
  file_to_save$Date <- date_hour(filename)
  file_to_save$Session <- session
  
  # Reorder columns and compute points
  file_to_save <- file_to_save[c(5,6,7,1,2,3,4)]
  file_to_save <- compute_points(file_to_save)
  
  # Write to output file
  output_path <- file.path(path_clean, output_filename)
  write.table(file_to_save, output_path, 
              append = FALSE, sep = "\t", dec = ".",
              row.names = TRUE, col.names = TRUE, 
              quote = FALSE)
  
  return(id)
}

#compute points of one session of one participant and return a dataframe (or the points only if point_only=TRUE)
compute_points<-function(file_to_read, point_only=FALSE,press=FALSE,prct=TRUE){# if df_return = False, return the total score
  e1=file_to_read$e1
  dp=subset(file_to_read,e1=="press"|e1=="new_mine"|e1=="bonus_available"|e1=="collide"|e1=="destroyed"|e1=="warp"|e1=="score+"|e1=="timeout"|e1=="pnts_bonus_capture"|e1=="pnts_bonus_failure"|e1=="shots_bonus_capture"|e1=="shots_bonus_failure",select=c("Date","Session","Pseudo","system_time","e1","e2","e3"))
  dp$Type=NA
  dp$Point=0
  dp$Group=NA
  #Ship collision
  dp$Type[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]="ShipDamage"
  dp$Point[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]=-50
  #Ship destruction
  dp$Type[dp$e1=="destroyed"& dp$e2=="ship"]="ShipDestruction"
  dp$Point[dp$e1=="destroyed"& dp$e2=="ship"]=-100
  #Border crossing
  dp$Type[dp$e1=="warp"]="BorderCrossing"
  dp$Point[dp$e1=="warp"]=-35
  #Fortress collision
  dp$Type[dp$e1=="collide"& dp$e2=="small_hex" & dp$e3=="ship"]="FortressCollision"
  dp$Point[dp$e1=="collide"& dp$e2=="small_hex" & dp$e3=="ship"]=-35
  #Fortress destruction
  dp$Type[dp$e1=="destroyed"&dp$e2=="fortress"]="FortressDestruction"
  dp$Point[dp$e1=="destroyed"&dp$e2=="fortress"]=250
  # #Friends Mines destruction (false version)
  # dp$Type[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="50"]="FriendMineDestruction"
  # dp$Point[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="50"]=60
  # #Foes Mines destruction
  # dp$Type[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="60"]="FoeMineDestruction"
  # dp$Point[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="60"]=50 #INVERSION?
  # #Mine disappears
  # dp$Type[dp$e1=="timeout"&dp$e2=="mine"]="MineExtinction"
  # dp$Point[dp$e1=="timeout"&dp$e2=="mine"]=-50
  #Friends Mines destruction
  dp$Type[dp$e1=="collide"&dp$e2=="friend_mine"]="FriendMineDestruction"
  dp$Point[dp$e1=="collide"&dp$e2=="friend_mine"]=50
  #Foes Mines destruction
  dp$Type[dp$e1=="collide"&dp$e2=="tagged_foe_mine"]="FoeMineDestruction"
  dp$Point[dp$e1=="collide"&dp$e2=="tagged_foe_mine"]=60 
  #Mine disappears
  dp$Type[dp$e1=="timeout"&dp$e2=="mine"]="MineExtinction"
  dp$Point[dp$e1=="timeout"&dp$e2=="mine"]=-50
  # #Collision with a mine
  # dp$Type[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]="MineCollision"
  # dp$Point
  #Points Bonus capture
  dp$Type[dp$e1=="pnts_bonus_capture"]="PointsBonusCapture"
  dp$Point[dp$e1=="pnts_bonus_capture"]=100
  #Shots Bonus capture
  dp$Type[dp$e1=="shots_bonus_capture"]="ShotsBonusCapture"
  dp$Point[dp$e1=="shots_bonus_capture"]=50
  #Bonus failure
  dp$Type[dp$e1=="pnts_bonus_failure"|dp$e1=="shots_bonus_failure"]="BonusFailure"
  dp$Point[dp$e1=="pnts_bonus_failure"|dp$e1=="shots_bonus_failure"]=-50
  #New Mine
  dp$Type[dp$e1=="new_mine"]="NewMine"
  dp$Point[dp$e1=="new_mine"]=0
  #New Bonus
  dp$Type[dp$e1=="bonus_available"]="NewBonus"
  dp$Point[dp$e1=="bonus_available"]=0
  #Missile collide Fortress
  dp$Type[dp$e1=="collide"&dp$e3=="fortress"]="FortressShot"
  dp$Point[dp$e1=="collide"&dp$e3=="fortress"]=0
  #Group of Scores
  dp$Group[dp$Type=="ShipDamage"|dp$Type=="ShipDestruction"|dp$Type=="BorderCrossing"|dp$Type=="FortressCollision"]="Flight"
  dp$Group[dp$Type=="FortressDestruction"|dp$Type=="FortressShot"]="Fortress"
  dp$Group[dp$Type=="FriendMineDestruction"|dp$Type=="FoeMineDestruction"|dp$Type=="MineExtinction"]="Mine"
  dp$Group[dp$Type=="PointsBonusCapture"|dp$Type=="ShotsBonusCapture"|dp$Type=="BonusFailure"]="Bonus"
  dp$Group[dp$Type=="FortressShot"]="Bonus"
  dp$Group[dp$e1=="press"]="Press"
  dp$Type[dp$e1=="press"]="Press"
  
  dp$Group[dp$e1=="new_mine"|dp$e1=="bonus_available"|dp$Type=="FortressShot"]="GameEvent"
  
  dp=subset(dp,!is.na(Type),select=c("Date","Session","Pseudo","system_time","e1","e2","e3","Type","Point","Group"))
  if(point_only==TRUE&press==FALSE){
    #return(dp$Point[e1!="press"]) #returns the points only
    return(subset(dp,e1!="press",select=c("Group","Point")))
  }else if(prct==TRUE){
    return(dp)
  }else{
    return(subset(dp,e1!="new_mine"|e1!="bonus_available"))
  }
}

#returns list of two vectors : ScM & APM for one session file of one participant
compute_APM_ScoreMin<-function(file_APM_ScM){  
  #All files system time are length()=1:599
  time_begin=file_APM_ScM$system_time[1] #TIME in sec for begin of the game
  #time_end=file_APM_ScM$system_time[nrow(file_APM_ScM)] #TIME in sec for end of the game
  #create the vectors that will contain the required information for this file
  scores_vector=list()
  score_flight_vector=list()
  score_bonus_vector=list()
  score_mine_vector=list()
  score_fortress_vector=list()
  apm_vector=list()
  for(i in time_begin:(time_begin+539)){ #until 60s before end
    window_data=subset(file_APM_ScM,(system_time>=i & system_time<(i+60)))
    #scores per min
    points_df=compute_points(window_data,point_only = TRUE)
    scores_minute=sum(points_df$Point) #compute_points returns a dataframe 
    score_flight=sum(points_df$Point[points_df$Group=="Flight"])
    score_bonus=sum(points_df$Point[points_df$Group=="Bonus"])
    score_mine=sum(points_df$Point[points_df$Group=="Mine"])
    score_fortress=sum(points_df$Point[points_df$Group=="Fortress"])
    #SCORES LISTS
    scores_vector[[length(scores_vector)+1]]=scores_minute
    score_flight_vector[[length(score_flight_vector)+1]]=score_flight
    score_bonus_vector[[length(score_bonus_vector)+1]]=score_bonus
    score_mine_vector[[length(score_mine_vector)+1]]=score_mine
    score_fortress_vector[[length(score_fortress_vector)+1]]=score_fortress
    #APM
    press_min = as.numeric(table(window_data$e1)["press"])
    apm_vector=append(apm_vector,press_min)
  }
  list_vect=list(scores_vector,score_flight_vector,score_bonus_vector,score_mine_vector,score_fortress_vector,apm_vector)
  # LIST ORDER : SCORE, FLIGHT, BONUS, MINE, FORTRESS, APM
  return(list_vect)
}

#write txt file with APM ScM for each session of each participant
write_APM_ScM <- function(clean_file_to_read){
  #df_APM_ScM=foreach(i=1:length(files_to_read),.combine=rbind)%do%{
  foreach(i=1:length(clean_file_to_read))%do%{
    file_APM_ScM=read.table(paste0(path_clean,"/",clean_file_to_read[i]), header=TRUE, sep="\t",dec=".",fill=TRUE)
    l_v=compute_APM_ScoreMin(file_APM_ScM)
    file_to_save=data.frame(unlist(l_v[1]),unlist(l_v[2]),unlist(l_v[3]),unlist(l_v[4]),unlist(l_v[5]),unlist(l_v[6]))
    file_to_save$TotalScore=sum(file_APM_ScM$Point)
    #SubScores
    file_to_save$Flight=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Flight"])
    file_to_save$Bonus=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Bonus"])
    file_to_save$Mine=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Mine"])
    file_to_save$Fortress=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Fortress"])
    file_to_save$Pseudo=file_APM_ScM$Pseudo[1]
    file_to_save$Date=file_APM_ScM$Date[1]
    file_to_save$Session=file_APM_ScM$Session[1]
    colnames(file_to_save)=c("ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM","TotalScore","Flight","Bonus","Mine","Fortress","Pseudo","Date","Session")
    write.table(file_to_save, paste0(path_ScM_APM,"ScM_APM_",clean_file_to_read[i]),append = FALSE, sep = "\t", dec = ".",row.names = TRUE, col.names = TRUE,quote=FALSE)
    #c(file_APM_ScM$Date[1],file_APM_ScM$Pseudo[1],l_v[1],l_v[2],sum(file_APM_ScM$Point),round(mean(unlist(l_v[1])),0))
  }
  return("write_APM_ScM done")
}

#read all txt file with APM and ScM
read_APM_ScM<-function(files_APM_ScM){
  df_APM_ScM=foreach(i=1:length(files_APM_ScM),.combine=rbind)%do%{
    file_APM_ScM=read.table(paste0(path_ScM_APM,files_APM_ScM[i]),header=TRUE,sep="\t")
    # list(Date=file_APM_ScM$Date[1],Session=file_APM_ScM$Session[1],Pseudo=file_APM_ScM$Pseudo[1]
    #   ,ScoresMin=file_APM_ScM$ScoresMin,FlightScore=file_APM_ScM$FlightScore,BonusScore=file_APM_ScM$BonusScore
    #   ,MineScore=file_APM_ScM$MineScore,FortressScore=file_APM_ScM$FortressScore,APM=file_APM_ScM$APM,
    #   TotalScore=file_APM_ScM$TotalScore[1],Flight=file_APM_ScM$Flight[1],Bonus=file_APM_ScM$Bonus[1],Mine=file_APM_ScM$Mine[1]
    #   ,Fortress=file_APM_ScM$Fortress[1],MeanScore=round(mean(unlist(file_APM_ScM$ScoresMin)),0))  
    list(Date=file_APM_ScM$Date[1],Session=file_APM_ScM$Session[1],Pseudo=file_APM_ScM$Pseudo[1]
         ,ScoresMin=file_APM_ScM$ScoresMin,FlightScore=file_APM_ScM$FlightScore,BonusScore=file_APM_ScM$BonusScore
         ,MineScore=file_APM_ScM$MineScore,FortressScore=file_APM_ScM$FortressScore,APM=file_APM_ScM$APM,
         TotalScore=file_APM_ScM$TotalScore[1],MeanScore=round(mean(unlist(file_APM_ScM$ScoresMin)),0))
  }
  #colnames(df_APM_ScM)=c("Date","Session","Pseudo","ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM","TotalScore","Flight","Bonus","Mine","Fortress","MeanScore")
  #rownames(df_APM_ScM)=1:nrow(df_APM_ScM)
  df_APM_ScM=as.data.frame(df_APM_ScM)
  df_APM_ScM$Date=unlist(df_APM_ScM$Date)
  df_APM_ScM$Session=unlist(df_APM_ScM$Session)
  df_APM_ScM$Pseudo=unlist(df_APM_ScM$Pseudo)
  df_APM_ScM$TotalScore=unlist(df_APM_ScM$TotalScore)
  # df_APM_ScM$Flight=unlist(df_APM_ScM$Flight)
  # df_APM_ScM$Bonus=unlist(df_APM_ScM$Bonus)
  # df_APM_ScM$Mine=unlist(df_APM_ScM$Mine)
  # df_APM_ScM$Fortress=unlist(df_APM_ScM$Fortress)
  df_APM_ScM$MeanScore=unlist(df_APM_ScM$MeanScore)
  
  return(df_APM_ScM)
}

get_day_session<-function(file_to_date,path_to_date,c=FALSE){
  list_file=list.files(path=path_to_date,recursive = T) 
  sublist=list_file[lapply(list_file,function(x,clean=c) ID(x,clean=c)==ID(file_to_date,clean=c))==TRUE]
  sublist=sublist[order(date_hour(sublist,clean=c))]#order the sublist by date
  i=match(sublist[date_hour(sublist,clean=c)==date_hour(file_to_date,clean=c)],sublist)
  if(length(sublist)==11){
    return(switch(i,"D01P1","D02P1","D02P2","D03P1","D03P2","D04P1","D04P2","D05P1","D05P2","D14P1","D14P2"))
  }else{
    return(switch(i,"G01","G02","G03","G04","G05","G06","G07","G08","G09","G10"))
  }
}

#CONCATENATION OF ScM & AMP /PARTICIPANT
concatenate<-function(df_to_concatenate){
  concatenate_df=data.frame(matrix(ncol=ncol(df_to_concatenate),nrow=length(unique(df_to_concatenate$Pseudo))))
  colnames(concatenate_df)=colnames(df_to_concatenate)
  i=1
  for(str_pseudo in unique(df_to_concatenate$Pseudo)){
    subdf=subset(df_to_concatenate,df_to_concatenate$Pseudo==str_pseudo)
    subdf=subdf[order(as.POSIXct(as.character(subdf$Date))),] #reorder (if necessary) the df by date
    unique_ScM=subdf$ScoresMin[1]
    unique_ScFlight=subdf$FlightScore[1]
    unique_ScBonus=subdf$BonusScore[1]
    unique_ScMine=subdf$MineScore[1]
    unique_ScFortress=subdf$FortressScore[1]
    unique_APM=subdf$APM[1]
    #unique_RollSD=subdf$RollSD[1]
    for(j in 2:11){
      unique_ScM=mapply(c,unique_ScM,subdf$ScoresMin[j],SIMPLIFY = FALSE)
      unique_ScFlight=mapply(c,unique_ScFlight,subdf$FlightScore[j],SIMPLIFY = FALSE)
      unique_ScBonus=mapply(c,unique_ScBonus,subdf$BonusScore[j],SIMPLIFY = FALSE)
      unique_ScMine=mapply(c,unique_ScMine,subdf$MineScore[j],SIMPLIFY = FALSE)
      unique_ScFortress=mapply(c,unique_ScFortress,subdf$FortressScore[j],SIMPLIFY = FALSE)
      unique_APM=mapply(c,unique_APM,subdf$APM[j],SIMPLIFY = FALSE)
      # unique_RollSD=mapply(c,unique_RollSD,subdf$RollSD[j],SIMPLIFY = FALSE)
    }
    concatenate_df[i,]=subdf[1,]
    concatenate_df$ScoresMin[i]=unique_ScM
    concatenate_df$FlightScore[i]=unique_ScFlight
    concatenate_df$BonusScore[i]=unique_ScBonus
    concatenate_df$MineScore[i]=unique_ScMine
    concatenate_df$FortressScore[i]=unique_ScFortress
    concatenate_df$APM[i]=unique_APM
    #  concatenate_df$RollSD[i]=unique_RollSD
    i=i+1
  }
  #concatenate_df=subset(concatenate_df,select=c("Pseudo","ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM","RollSD"))
  concatenate_df=subset(concatenate_df,select=c("Pseudo","ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM"))
  return(concatenate_df)
}

read_data_score<-function(files_data,add_press=FALSE){
  df_data=foreach(i=1:length(files_data),.combine=rbind)%do%{
    file_d=read.table(paste0(path_clean,files_data[i]),header=TRUE,sep="\t")
    if(!add_press){
      file_d=subset(file_d,Type!="Press")
    }
    c(file_d$Date[1],file_d$Session[1],file_d$Pseudo[1],list(file_d$system_time),list(file_d$Point),list(file_d$Point[file_d$Group=="Flight"]),list(file_d$Point[file_d$Group=="Bonus"]),list(file_d$Point[file_d$Group=="Mine"]),list(file_d$Point[file_d$Group=="Fortress"]),list(as.numeric(cumsum(file_d$Point))))
  }
  #,list(cumsum(file_d$Point)),"CumScore"
  colnames(df_data)=c("Date","Session","Pseudo","System_time","Point","FlightPoint","BonusPoint","MinePoint","FortressPoint","CumulScore")
  rownames(df_data)=1:nrow(df_data)
  df_data=as.data.frame(df_data)
  return(df_data)
}

read_final_Score<-function(files_data,path_clean=path_clean,detailed=FALSE){
  if(detailed){
    df_data=foreach(i=1:length(files_data),.combine = rbind)%do%{
      file=read.table(paste0(path_clean,"/",files_data[i]), header=TRUE, sep="\t",dec=".",fill=TRUE)
      prct_bonus=(sum(file$Type=="ShotsBonusCapture"|file$Type=="PointsBonusCapture")*100)/sum(file$Type=="NewBonus")
      prct_mine=(sum(file$Type=="FriendMineDestruction"|file$Type=="FoeMineDestruction")*100)/sum(file$Type=="NewMine")
      data.frame(file$Date[1],file$Session[1],file$Pseudo[1],sum(file$Point),sum(file$Point[file$Group=="Flight"]),sum(file$Point[file$Group=="Bonus"]),sum(file$Point[file$Group=="Mine"]),sum(file$Point[file$Group=="Fortress"]),sum(file$Type=="NewBonus"),sum(file$Type=="NewMine"),prct_bonus,prct_mine,sum(file$Type=="FortressShot"))
    }
    colnames(df_data)=c("Date","Session","Pseudo","TotalScore","Flight","Bonus","Mine","Fortress","NumberofBonus","NumberofMine","Bonus_Prct","Mine_Prct","FortressShot")
  }else{
    df_data=foreach(i=1:length(files_data),.combine = rbind)%do%{
      file=read.table(paste0(path_clean,"/",files_data[i]), header=TRUE, sep="\t",dec=".",fill=TRUE)
      data.frame(file$Date[1],file$Session[1],file$Pseudo[1],sum(file$Point),sum(file$Point[file$Group=="Flight"]),sum(file$Point[file$Group=="Bonus"]),sum(file$Point[file$Group=="Mine"]),sum(file$Point[file$Group=="Fortress"]))
    }
    colnames(df_data)=c("Date","Session","Pseudo","TotalScore","Flight","Bonus","Mine","Fortress")
  }
  return(df_data)
}


demographie<-function(final_df,df_demographique,ZMean,Delta){
  df_demographique$D01P1=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D01P1"]}))
  df_demographique$D02P1=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D02P1"]}))
  df_demographique$D02P2=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D02P2"]}))
  df_demographique$D03P1=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D03P1"]}))
  df_demographique$D03P2=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D03P2"]}))
  df_demographique$D04P1=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D04P1"]}))
  df_demographique$D04P2=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D04P2"]}))
  df_demographique$D05P1=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D05P1"]}))
  df_demographique$D05P2=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D05P2"]}))
  df_demographique$D14P1=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D14P1"]}))
  df_demographique$D14P2=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$TotalScore[final_df$Pseudo==x&final_df$Session=="D14P2"]}))
  if(Delta==TRUE){
    df_demographique$DeltaD1D14=df_demographique$D14P2-df_demographique$D01P1
    df_demographique$DeltaD1D5=df_demographique$D05P2-df_demographique$D01P1
    df_demographique$DeltaD14D5=df_demographique$D14P2-df_demographique$D05P2
    
    df_demographique$D01Flight=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Flight[final_df$Pseudo==x&final_df$Session=="D01P1"]}))
    df_demographique$D05Flight=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Flight[final_df$Pseudo==x&final_df$Session=="D05P2"]}))
    df_demographique$D14Flight=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Flight[final_df$Pseudo==x&final_df$Session=="D14P2"]}))
    
    df_demographique$DeltaD1D14Flight=df_demographique$D14Flight-df_demographique$D01Flight
    df_demographique$DeltaD14D5Flight=df_demographique$D14Flight-df_demographique$D05Flight
    df_demographique$DeltaD5D1Flight=df_demographique$D05Flight-df_demographique$D01Flight
    
    df_demographique$D01Bonus=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Bonus[final_df$Pseudo==x&final_df$Session=="D01P1"]}))
    df_demographique$D05Bonus=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Bonus[final_df$Pseudo==x&final_df$Session=="D05P2"]}))
    df_demographique$D14Bonus=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Bonus[final_df$Pseudo==x&final_df$Session=="D14P2"]}))
    
    df_demographique$DeltaD1D14Bonus=df_demographique$D14Bonus-df_demographique$D01Bonus
    df_demographique$DeltaD14D5Bonus=df_demographique$D14Bonus-df_demographique$D05Bonus
    df_demographique$DeltaD5D1Bonus=df_demographique$D05Bonus-df_demographique$D01Bonus
    
    
    df_demographique$D01Mine=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Mine[final_df$Pseudo==x&final_df$Session=="D01P1"]}))
    df_demographique$D05Mine=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Mine[final_df$Pseudo==x&final_df$Session=="D05P2"]}))
    df_demographique$D14Mine=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Mine[final_df$Pseudo==x&final_df$Session=="D14P2"]}))
    
    df_demographique$DeltaD1D14Mine=df_demographique$D14Mine-df_demographique$D01Mine
    df_demographique$DeltaD14D5Mine=df_demographique$D14Mine-df_demographique$D05Mine
    df_demographique$DeltaD5D1Mine=df_demographique$D05Mine-df_demographique$D01Mine
    
    
    df_demographique$D01Fortress=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Fortress[final_df$Pseudo==x&final_df$Session=="D01P1"]}))
    df_demographique$D05Fortress=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Fortress[final_df$Pseudo==x&final_df$Session=="D05P2"]}))
    df_demographique$D14Fortress=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$Fortress[final_df$Pseudo==x&final_df$Session=="D14P2"]}))
    
    df_demographique$DeltaD1D14Fortress=df_demographique$D14Fortress-df_demographique$D01Fortress
    df_demographique$DeltaD14D5Fortress=df_demographique$D14Fortress-df_demographique$D05Fortress
    df_demographique$DeltaD5D1Fortress=df_demographique$D05Fortress-df_demographique$D01Fortress
    
    
  }
  if(ZMean==TRUE){
    df_demographique$D01P1ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D01P1"]}))
    df_demographique$D02P2ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D02P1"]}))
    df_demographique$D02P2ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D02P2"]}))
    df_demographique$D03P1ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D03P1"]}))
    df_demographique$D03P2ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D03P2"]}))
    df_demographique$D04P1ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D04P1"]}))
    df_demographique$D04P2ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D04P2"]}))
    df_demographique$D05P1ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D05P1"]}))
    df_demographique$D05P2ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D05P2"]}))
    df_demographique$D14P1ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D14P1"]}))
    df_demographique$D14P2ZM=as.numeric(lapply(df_demographique$identifiant,function(x){final_df$ZMean[final_df$Pseudo==x&final_df$Session=="D14P2"]}))
  }
  names(df_demographique)[names(df_demographique)=="Group..blind."]="BlindGroup"
  names(df_demographique)[names(df_demographique)=="identifiant"]="Pseudo"
  #names(df_demographique)[names(df_demographique)=="Votre.?ge"]="Age"
  names(df_demographique)[names(df_demographique)=="Votre.genre"]="Genre"
  names(df_demographique)[names(df_demographique)=="sum_JV"]="GameLevel"
  if(ZMean==TRUE){
    df_demographique=subset(df_demographique,select=c("Pseudo","BlindGroup","Group","NET","GameLevel","Age",'Genre',"D01P1","D02P1","D02P2","D03P1","D03P2","D04P1","D04P2","D05P1","D05P2","D14P1","D14P2","D01P1ZM","D02P2ZM","D03P1ZM","D03P2ZM","D04P1ZM","D04P2ZM","D05P1ZM","D05P2ZM","D14P1ZM","D14P2ZM"))
  }else if(Delta==TRUE){
    df_demographique=subset(df_demographique,select=c("Pseudo","BlindGroup","Group","NET","GameLevel","Age",'Genre',"D01P1","D02P1","D02P2","D03P1","D03P2","D04P1","D04P2","D05P1","D05P2","D14P1","D14P2","DeltaD1D14","DeltaD14D5","DeltaD1D5","DeltaD1D14Flight","DeltaD14D5Flight","DeltaD5D1Flight","DeltaD1D14Bonus","DeltaD14D5Bonus","DeltaD5D1Bonus","DeltaD1D14Mine","DeltaD14D5Mine","DeltaD5D1Mine","DeltaD1D14Fortress","DeltaD14D5Fortress","DeltaD5D1Fortress"))
  }else{
    df_demographique=subset(df_demographique,select=c("Pseudo","BlindGroup","Group","NET","GameLevel","Age",'Genre',"D01P1","D02P1","D02P2","D03P1","D03P2","D04P1","D04P2","D05P1","D05P2","D14P1","D14P2"))
  }
  return(df_demographique)
}

demographie_long<-function(final_df,df_demographique){
  for(str_pseudo in unique(final_df$Pseudo)){
    final_df$NET[final_df$Pseudo==str_pseudo]=df_demographique$NET[df_demographique$identifiant==str_pseudo]
    final_df$GameLevel[final_df$Pseudo==str_pseudo]=df_demographique$sum_JV[df_demographique$identifiant==str_pseudo]
    final_df$Age[final_df$Pseudo==str_pseudo]=df_demographique$Age[df_demographique$identifiant==str_pseudo]
    final_df$Genre[final_df$Pseudo==str_pseudo]=df_demographique$Votre.genre[df_demographique$identifiant==str_pseudo]
    final_df$Group[final_df$Pseudo==str_pseudo]=df_demographique$Group[df_demographique$identifiant==str_pseudo]
    final_df$BlindGroup[final_df$Pseudo==str_pseudo]=df_demographique$Group..blind.[df_demographique$identifiant==str_pseudo]
    
  }
  return(final_df)
}

ZScores<-function(dl){
  dl$ZScore=scale(dl$TotalScore)
  #1 Yeo johnson transformation
  dl$Flight_YeoJ=yeojohnson(dl$Flight)$x.t
  dl$Bonus_YeoJ=yeojohnson(dl$Bonus)$x.t
  dl$Mine_YeoJ=yeojohnson(dl$Mine,standardize = TRUE)$x.t
  dl$Fortress_YeoJ=yeojohnson(dl$Fortress,standardize = TRUE)$x.t
  #2ZSousScores
  dl$ZFlight=scale(dl$Flight_YeoJ)
  dl$ZMine=scale(dl$Mine_YeoJ)
  dl$ZBonus=scale(dl$Bonus_YeoJ)
  dl$ZFortress=scale(dl$Fortress_YeoJ)
  #3 ZMean ( mean of the Sub ZScores)
  dl$ZMean=rowMeans(subset(dl,select=c("ZMine","ZFortress","ZBonus","ZFlight")))
  return(dl)
}

LearningRate<-function(dl,dw,shortTerm=FALSE,ZM=TRUE){
  dl$D=1:11
  if(shortTerm==TRUE){
    dl=subset(dl,Session!="D14P2"&Session!="D14P1")
  }
  if(ZM){
    fit_all=lm(ZMean~ln(D),data=dl) #fit_all_lin=lm(ZMean~D,data=dl)
    by_pseudo=dl%>%
      group_by(Pseudo)
    lnreg_pseudo=do(by_pseudo,tidy(lm(ZMean~ln(D),data=.)))
    slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]
    if(shortTerm){
      for(str_pseudo in unique(dw$Pseudo)){
        dw$LearningRateSTZM[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
      }
    }else{
      for(str_pseudo in unique(dw$Pseudo)){
        dw$LearningRateLTZM[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
      }
    }
  }else{
    fit_all=lm(TotalScore~ln(D),data=dl) #fit_all_lin=lm(ZMean~D,data=dl)
    by_pseudo=dl%>%
      group_by(Pseudo)
    lnreg_pseudo=do(by_pseudo,tidy(lm(TotalScore~ln(D),data=.)))
    slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]
    if(shortTerm){
      for(str_pseudo in unique(dw$Pseudo)){
        dw$LearningRateST[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
      }
    }else{
      for(str_pseudo in unique(dw$Pseudo)){
        dw$LearningRateLT[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
      }
    }
  }
  return(dw)
}
LearningRate_LT_SousScore<-function(dl,dw){
  dl$D=1:11
  by_pseudo=dl%>%
    group_by(Pseudo)
  #FLIGHT
  fit_all=lm(Flight~ln(D),data=dl) #fit_all_lin=lm(ZMean~D,data=dl)
  lnreg_pseudo=do(by_pseudo,tidy(lm(Flight~ln(D),data=.)))
  slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]
  for(str_pseudo in unique(dw$Pseudo)){
    dw$LRFlight[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
  }
  #BONUS
  lnreg_pseudo=do(by_pseudo,tidy(lm(Bonus~ln(D),data=.)))
  slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]
  for(str_pseudo in unique(dw$Pseudo)){
    dw$LRBonus[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
  }
  #Mine
  lnreg_pseudo=do(by_pseudo,tidy(lm(Mine~ln(D),data=.)))
  slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]
  for(str_pseudo in unique(dw$Pseudo)){
    dw$LRMine[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
  }
  #FORTRESS
  fit_all=lm(Fortress~D,data=dl) #LINEAR FIT
  lnreg_pseudo=do(by_pseudo,tidy(lm(Fortress~D,data=.)))
  slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="D"]
  for(str_pseudo in unique(dw$Pseudo)){
    dw$LRFortress[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="D"&lnreg_pseudo$Pseudo==str_pseudo]
  }
  return(dw)
}
LearningRate_LT_LINEAR<-function(dl,dw){
  by_pseudo=dl%>%
    group_by(Pseudo)
  lnreg_pseudo=do(by_pseudo,tidy(lm(TotalScore~D,data=.)))
  slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="D"]
  for(str_pseudo in unique(dw$Pseudo)){
    dw$LR_LINEAR[dw$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="D"&lnreg_pseudo$Pseudo==str_pseudo]
  }
  return(dw)
}