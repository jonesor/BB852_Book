#Create personal calendar for Owen and Thomas-----
require(magrittr)
require(dplyr)
require(calendar)
require(lubridate)

savingsTimeSwitch <- with_tz(lubridate::as_datetime("2021-10-31 03:00:00"),"Europe/Copenhagen")

courseCal <- ic_read("bb852.ics") %>% 
  arrange(DTSTART) %>% 
  mutate(Session = row_number()) %>% 
  mutate(savingsT = DTSTART < savingsTimeSwitch) %>% 
  mutate(offsetT = if_else(savingsT == TRUE, 2, 1)) %>% #Check this every year!
  mutate(DTSTART2 = DTSTART + hours(offsetT)) %>% 
  mutate(DTEND2 = DTEND + hours(offsetT)) %>% 
  mutate(Room = gsub(pattern = "Odense ",replacement = "", x = LOCATION)) %>% 
  select(Session,DTSTART2,DTEND2, Room)

schedule <- readxl::read_excel("BB852_Schedule.xlsx")

icalSchedule <- left_join(courseCal,schedule) %>% 
  select(DTSTART2,DTEND2,Room,Type,Topic,Instructor) 

instructorList <- na.omit(unique(icalSchedule$Instructor))

for(inst in 1:length(instructorList)){
  if(dir.exists(paste0("personalCalendar_",instructorList[inst]))){
    system(paste0("rm -r personalCalendar_",instructorList[inst]))}
  dir.create(paste0("personalCalendar_",instructorList[inst]))
  
  icalScheduleTemp <- icalSchedule %>% filter(Instructor == instructorList[inst])
  
  for(i in 1:nrow(icalScheduleTemp)){
    icalName <- paste0("BB852_",icalScheduleTemp$DTSTART2[i])
    ic_write(file = paste0("personalCalendar_",instructorList[inst],"/",icalName,".ics"),
             ic = ic_event(start = icalScheduleTemp$DTSTART2[i],
                           end = icalScheduleTemp$DTEND2[i],
                           summary = paste0("BB852 ", icalScheduleTemp$Topic[i],", ",
                                            icalScheduleTemp$Type[i], " in ", icalScheduleTemp$Room[i])))
  }
  
  #Combine event files by appending the text
  files <- list.files(paste0("personalCalendar_",instructorList[inst]),full.names  = TRUE)
  combinedFiles <- NULL
  for(i in 1:length(files)){
    temp <- readLines(con = files[i])  
    combinedFiles <- append(combinedFiles,temp)
  }
  
  #Remove the Begin/End Calendar lines.
  combinedFiles[grep(pattern = "*:VCALENDAR",combinedFiles)]<-""
  
  #Add begin/end calendar lines to the text.
  combinedFiles[1] <- "BEGIN:VCALENDAR"
  combinedFiles[length(combinedFiles)] <- "END:VCALENDAR"
  
  writeLines(combinedFiles,con = paste0("BB852_",instructorList[inst],".ics"))
  
}


# StudentVersion ----


  if(dir.exists("studentCalendar")){system("rm -r studentCalendar")}
  dir.create("studentCalendar")
  
  icalScheduleTemp <- icalSchedule
  
  for(i in 1:nrow(icalScheduleTemp)){
    icalName <- paste0("BB852_",icalScheduleTemp$DTSTART2[i])
    ic_write(file = paste0("studentCalendar/",icalName,".ics"),
             ic = ic_event(start = icalScheduleTemp$DTSTART2[i],
                           end = icalScheduleTemp$DTEND2[i],
                           summary = paste0("BB852 ", icalScheduleTemp$Topic[i]," w.",icalScheduleTemp$Instructor[i],
                                            icalScheduleTemp$Type[i], " in ", icalScheduleTemp$Room[i])))
  }
  
  #Combine event files by appending the text
  files <- list.files("studentCalendar",full.names  = TRUE)
  combinedFiles <- NULL
  for(i in 1:length(files)){
    temp <- readLines(con = files[i])  
    combinedFiles <- append(combinedFiles,temp)
  }
  
  #Remove the Begin/End Calendar lines.
  combinedFiles[grep(pattern = "*:VCALENDAR",combinedFiles)]<-""
  
  #Add begin/end calendar lines to the text.
  combinedFiles[1] <- "BEGIN:VCALENDAR"
  combinedFiles[length(combinedFiles)] <- "END:VCALENDAR"
  
  writeLines(combinedFiles,con = "BB852_student.ics")
  
#Tidy up
  
  for(inst in 1:length(instructorList)){
    if(dir.exists(paste0("personalCalendar_",instructorList[inst]))){
      system(paste0("rm -r personalCalendar_",instructorList[inst]))}
    }
  system("rm -r studentCalendar")
  