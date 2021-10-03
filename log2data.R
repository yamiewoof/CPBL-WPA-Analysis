source("C:\\Users\\carl_dis2003\\Desktop\\大學\\大三下\\R\\log_process\\util.R")

check_then_install("dplyr")
check_then_install("tm")
check_then_install("plyr")
check_then_install("taRifx")



# 1. source all functions
# remember add encoding = "UTF-8"
fns <- list.files("C:\\Users\\carl_dis2003\\Desktop\\大學\\大三下\\R\\log_process\\functions", full.names = T, pattern="*.R")
for (fn in fns) {
  source(fn, encoding = "UTF-8")
}

re.base = T
re.primary = T
re.exceptional = T
re.final = T

proc_year = 2015
gnl = list()
gnl[["2014"]][["g.nok"]] = c(43,64,215,120,94)
gnl[["2014"]][["g.ok"]] = setdiff(1:240, gnl[["2014"]][["g.nok"]])
gnl[["2015"]][["g.nok"]] = c(169, 170)
gnl[["2015"]][["g.ok"]] = setdiff(1:4, gnl[["2015"]][["g.nok"]])
gnl[["2016"]][["g.nok"]] = integer()
gnl[["2016"]][["g.ok"]] = setdiff(1:240, gnl[["2016"]][["g.nok"]])
gnl[["2017"]][["g.nok"]] = integer()
gnl[["2017"]][["g.ok"]] = setdiff(1:240, gnl[["2017"]][["g.nok"]])
gnl[["2018"]][["g.nok"]] = integer()
gnl[["2018"]][["g.ok"]] = setdiff(1:240, gnl[["2018"]][["g.nok"]])
gnl[["2019"]][["g.nok"]] = integer()
gnl[["2019"]][["g.ok"]] = setdiff(1:13, gnl[["2018"]][["g.nok"]])

# 2. run the single_game function
main_single_game = function(num) {
  print(paste("main single", num))
  # 2-1.load log_file
  log_path <- sprintf("C:\\Users\\carl_dis2003\\Desktop\\大學\\大三下\\R\\log_process\\logs\\%d\\例行賽%d(%d).txt", proc_year, num, proc_year)
  log_file <- readLines(log_path, encoding = "UTF-8")
  log_file <- dictionaryfunction(log_file)
  log_file <- strsplitfunction(log_file)
  log_file <- normalize_log_function(log_file)
  
  # 2-2. set the dummy list
  dummy_list <- list(
    num_logrow = num, inning = "NA",
    rem_type = "NA", base1 = "NA", base2 = "NA", base3 = "NA", 
    player = "NA", 
    to_check = 0
  )
  # 2-3. initialize the column waiting for vector
  c_numlogfile <- c_numlogrow <- c_numlogrow <- 
    c_inning <- c_remtype <- 
    c_base1 <- c_base2 <- c_base3 <- c_player <- 
    c_tocheck <- "NA"
  # 2-4. load funtions row by row
  for ( i in 1:length(log_file))  {
    log_row <- log_file[i]
    c_numlogfile[i] <- num
    c_numlogrow[i] <- i
    c_remtype[i] <- dummy_list$rem_type
    c_base1[i] <- dummy_list$base1
    c_base2[i] <- dummy_list$base2
    c_base3[i] <- dummy_list$base3
    
    # renew the current player, to_check
    dummy_list$player <- "NA"
    dummy_list$to_check <- 0
    
    ######## call the functions （推進、出局、例外） #############
    dummy_list <- inning_function(dummy_list, log_row)
    
    # 推進
    dummy_list <- hit1_function(dummy_list, log_row)
    dummy_list <- hit2_function(dummy_list, log_row)
    dummy_list <- hit3_function(dummy_list, log_row)
    dummy_list <- homerun_function(dummy_list, log_row)
    dummy_list <- walk_function(dummy_list, log_row)
    
    # 出局
    dummy_list <- outs_function(dummy_list, log_row)
    #dummy_list <- strikeout_function(dummy_list, log_row)
    
    # 例外
    dummy_list <- check_function(dummy_list, log_row)
    ##############################################################
    
    c_player[i] <- dummy_list$player
    c_tocheck[i] <- dummy_list$to_check
    c_inning[i] <- dummy_list$inning
  }
  
  # 3-4. set the  off_table (single_game)
  off_table <- data.frame(
    num_logfile = c_numlogfile, num_logrow = c_numlogrow, inning = c_inning,
    rem_type = c_remtype, 
    base1 = c_base1, base2 = c_base2, base3 = c_base3, 
    player = c_player,
    to_check = c_tocheck
  )
  # output
  return(off_table)
}

# 3. merge all off_table into offensive
if(re.base) {
  offensive_db <- lapply(1:4, main_single_game) %>% 
    Reduce(f = rbind)
}
  
# 4.merge offensive_db & anothercol

##程式碼從這邊開始--------------------------------------------------------------------------------------------
if(re.primary) {
  #沒問題的場次
  inningnoproblem<-gnl[[as.character(proc_year)]][["g.ok"]]
  #設定NULL合併用
  output1 <- NULL
  #迴圈開始-----
  for (num in inningnoproblem){
    print(paste("primary", num))
    #讀取資料
    fn <- sprintf("C:\\Users\\carl_dis2003\\Desktop\\大學\\大三下\\R\\log_process\\logs\\%d\\例行賽%d(%d).txt", proc_year, num, proc_year)
    x <- readLines(fn, encoding="UTF-8")
  
    #統一格式
    x          <- dictionaryfunction(x)
    #切割比分為自主一列
    x          <- strsplitfunction(x)
  
    #各欄位
    rowforgame <- c(1:length(x))
    specialcircumstances <- specialcircumstancesfunction()
    inning     <- inningfunction()
    id         <- idfunction()
    Base1      <- cbind(c(1:length(x)),c("NA"))[,2]             
    Base2      <- cbind(c(1:length(x)),c("NA"))[,2]              
    Base3      <- cbind(c(1:length(x)),c("NA"))[,2]              
    out        <- outfunction()
    away       <- pointfunction()
    home       <- point3function()
    Player     <- Playerfunction()
    #game.player<- game.playerfunction(x,num)
    direction  <- directionfunction()	    
    result     <- resultfunction()          
    follow.up  <- follow.upfunction(x)
    log        <- x
  
    outputmatrix <- cbind(num,rowforgame,specialcircumstances,inning,id,Base1,Base2,Base3,out,away,home,Player,direction,result,follow.up,log)
    #outputmatrix <- cbind(numforgame,rowforgame,specialcircumstances,inning,id,Base1,Base2,Base3,out,away,home,game.player,direction,result,follow.up,log)
    
    output1 <- rbind(output1,outputmatrix)
  }
  #迴圈結束------
}

if(re.exceptional) {
  ##例外加入
  #例行賽94,120,215 局數有問題 #例行賽64 九上誤植成九下#列行賽43沒有三出局#例行賽114 69列分數登記有誤
  #迴圈開始------
  output2<-NULL
  extragame<-gnl[[as.character(proc_year)]][["g.nok"]]
  for (num in extragame) {
    print(paste("exceptional", num))
    
    fn <- sprintf("C:\\Users\\carl_dis2003\\Desktop\\大學\\大三下\\R\\log_process\\logs\\%d\\例行賽%d(%d).txt", proc_year, num, proc_year)
    x <- readLines(fn, encoding="UTF-8")
  
    #統一格式
    x          <- dictionaryfunction(x)
    x          <- strsplitfunction(x)
    #各欄位
    rowforgame <- c(1:length(x))
    specialcircumstances <- specialcircumstancesfunction()
    inning     <- inningfunction()
    id         <- idfunction()
    Base1      <- cbind(c(1:length(x)),c("NA"))[,2]
    Base2      <- cbind(c(1:length(x)),c("NA"))[,2]
    Base3      <- cbind(c(1:length(x)),c("NA"))[,2]
    out        <- cbind(c(1:length(x)),c("NA"))[,2]
    away       <- pointfunction()
    home       <- point3function()
    Player     <- Playerfunction()
    #game.player<- game.playerfunction(x,num)
    direction  <- directionfunction()
    result     <- resultfunction()
    follow.up  <- follow.upfunction(x)
    log        <- x

    outputmatrix <- cbind(num,rowforgame,specialcircumstances,inning,id,Base1,Base2,Base3,out,away,home,Player,direction,result,follow.up,log)

    output2<-rbind(output2,outputmatrix)
  }
  #迴圈結束------
}

if(re.final) {
  print("finalize the data form")
  #merge output1 output2
  anothercol<-rbind(output1,output2)
  #sort row
  anothercol<-anothercol[order(as.numeric(anothercol[,1])) , ]
  anothercol <- data.frame(anothercol)
  
  # 發現anothercol和offensive_db的列數對不上，把anothercol多餘的列數砍掉
  if(nrow(anothercol) != nrow(offensive_db)){
    tt = as.numeric(table(anothercol$num)) == as.numeric(table(offensive_db$num_logfile))
    idx = names(table(anothercol$num)[idx])
    
    error_num = filter(anothercol, num %in% idx)
    anothercol = filter(anothercol, !num %in% idx)
    
    error_num = group_by(error_num, num) %>% 
      slice(-n()) %>% 
      ungroup()
    error_num = as.data.frame(error_num)
    
    anothercol = rbind(anothercol, error_num)
    anothercol<-anothercol[order(as.numeric(anothercol[,1])) , ]

  }
  
  #merge anothercol offensive_db
  merge2table=cbind(anothercol,offensive_db)
  
  #調整輸出欄位
  finalmatrix<-merge2table[,c(1:2,19,5,9:15,20:24,25,3,16)]



  # 5.找出例外狀況
  special<-character(length(finalmatrix[,1]))
  special[union(which(finalmatrix[,17]==1),which(finalmatrix[,18]==TRUE))]="warning"
  writeuse<-cbind(finalmatrix,special)

  #排列資料的col
  writeuse<-writeuse[,c(1:3,6,7,12:15,4,8:10,5,11,20,19)]
  
  #新增rowforextra
  rowforextra<-matrix(1,length(writeuse[,1]))
  outputuse<-cbind(writeuse,rowforextra)
  outputuse<-outputuse[,c(1:2,18,3:17)]

  #log空白列清除
  outputuse<-outputuse[outputuse$log!="",]
  outputuse <- dplyr::group_by(outputuse, num) %>%
    dplyr::mutate(rowforgame = 1:n())
  View(head(outputuse,10))
  # 6.Output
  write.csv(outputuse,file = sprintf("C:\\baseball\\CPBL\\logdata_%d.csv", proc_year), row.names=FALSE)
}
