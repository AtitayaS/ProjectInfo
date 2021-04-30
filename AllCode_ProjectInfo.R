################
#---Facebook---#
################

facebook1df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q1.csv")
facebook2df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q2.csv")
facebook3df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q3.csv")
facebook4df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q4.csv")


facebookdf = rbind(facebook1df,facebook2df,facebook3df,facebook4df)
names(facebookdf)


sum(is.na(facebookdf))

#facebookdf$fan = as.numeric(as.character(facebookdf$fan)) 

fbgroup = ddply(facebookdf, "account_username", summarise , 
                fbreac= mean(reaction),
                fbshare = mean(share), 
                fbengage = mean(engagement), 
                fbfan = max(fan))


#####################
#---Data Cleaning---#
#####################

#look
fbclean = fbgroup[grep("à",fbgroup$account_username),]
fbsep = separate(fbclean,account_username, c("username","useless"),sep = 'à')

#****real****#
fbrealsep = separate(fbgroup,account_username, c("account_user","useless"),sep = 'à')

glimpse(fbrealsep)

fbremove = fbrealsep[fbrealsep$account_user != ""&fbrealsep$account_user !='"',]


#***using fbremove for visualization***#

glimpse(fbremove)

#convert fbfan to numeric
fbremove$fbfan = as.numeric(as.character(fbremove$fbfan))

glimpse(fbremove)
#############
#---order---# 
#############

#by fan
fbgrouporderfan =  fbremove[order(fbremove$fbfan,decreasing = TRUE),]
glimpse(fbgrouporderfan)
fbgrouporderfan50 = fbgrouporderfan[1:50,]

#by engagement
fbgrouporderengage =  fbremove[order(fbremove$fbengage,decreasing = TRUE),]
fbgrouporderengage50 = fbgrouporderfan[1:50,]

############
#---bar1---# sort by fan
############

#fan
ggplot(fbgrouporderfan50, aes(x=fbfan, y=reorder(account_user,fbfan)))+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#engage
ggplot(fbgrouporderfan50, aes(x=fbengage, y=reorder(account_user,fbfan)))+
  geom_bar(stat="identity",fill="lightblue",colour="black")

#reaction
ggplot(fbgrouporderfan50, aes(x=fbreac, y=reorder(account_user,fbfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1",colour="black")

#share
ggplot(fbgrouporderfan50, aes(x=fbshare, y=reorder(account_user,fbfan)) )+
  geom_bar(stat="identity",fill="darkolivegreen2",colour="black")



############
#---bar2---# sort by engagement
############

#fan
ggplot(fbgrouporderengage50, aes(x=fbfan, y=reorder(account_user,fbengage)))+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#****engage****#
ggplot(fbgrouporderengage50, aes(x=fbengage, y=reorder(account_user,fbengage)))+
  geom_bar(stat="identity",fill="lightblue",colour="black")


######################
#---plot with date---#
######################
require(ggplot2)

#SORT BY FAN

#****influence 1****#

fbinflu1 = facebookdf[facebookdf$account_username == "Lowcostcosplay",]
fbinflu1$fan = as.numeric(as.character(fbinflu1$fan))
fbgroupinflu1 = ddply(fbinflu1, "created_at", summarise , 
                      fbreac= mean(reaction),
                      fbshare = mean(share), 
                      fbengage = mean(engagement), 
                      fbfan = max(fan))

fbgroupinflu1$date = as.Date(fbgroupinflu1$created_at, '%Y-%m-%d')
#separate date
fbgroupinflu1 %>% dplyr::mutate(year = lubridate::year(date), 
                                month = lubridate::month(date), 
                                day = lubridate::day(date)
)
#consider increasing in fan each post
ggplot(fbgroupinflu1, aes(x=date, y=fbfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(fbgroupinflu1, aes(x=date, y=fbengage))+
  geom_line()+geom_point()



#****influence 2****#

fbinflu2 = facebookdf[facebookdf$account_username == "SALE HERE",]
fbinflu2$fan = as.numeric(as.character(fbinflu2$fan))
fbgroupinflu2 = ddply(fbinflu2, "created_at", summarise , 
                      fbreac= mean(reaction),
                      fbshare = mean(share), 
                      fbengage = mean(engagement), 
                      fbfan = max(fan))

fbgroupinflu2$date = as.Date(fbgroupinflu2$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(fbgroupinflu2, aes(x=date, y=fbfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(fbgroupinflu2, aes(x=date, y=fbengage))+
  geom_line()+geom_point()

#separate date
fbif2sepdate = separate(fbgroupinflu2,date,c("year","month","day"),sep = "-")
#group again
fbif2sepdategrp = ddply(fbif2sepdate, "day", summarise , 
                        fbreac0= mean(fbreac),
                        fbshare0 = mean(fbshare), 
                        fbengage0 = mean(fbengage), 
                        fbfan0 = mean(fbfan))

#consider increasing of fan each day
ggplot(fbif2sepdategrp, aes(x=day, y=fbfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(fbif2sepdategrp, aes(x=day, y=fbengage0, group = 1))+
  geom_line()+geom_point()


#****influence3****#
fbinflu3 = facebookdf[grep("^Starvingtime",facebookdf$account_username),]
fbinflu3$fan = as.numeric(as.character(fbinflu3$fan))

fbrealsepinflu3 = separate(fbinflu3,account_username, c("account_username","useless"),sep = 'à')

glimpse(fbrealsepinflu3)
drop = c("useless")
fbdrop = fbrealsepinflu3[ ,(names(fbrealsepinflu3) != drop)]


fbgroupinflu3 = ddply(fbinflu3, "created_at", summarise , 
                      fbreac= mean(reaction),
                      fbshare = mean(share), 
                      fbengage = mean(engagement), 
                      fbfan = max(fan))

fbgroupinflu3$date = as.Date(fbgroupinflu3$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(fbgroupinflu3, aes(x=date, y=fbfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(fbgroupinflu3, aes(x=date, y=fbengage))+
  geom_line()+geom_point()

#separate date
fbif3sepdate = separate(fbgroupinflu3,date,c("year","month","day"),sep = "-")
#group again
fbif3sepdategrp = ddply(fbif3sepdate, "day", summarise , 
                        fbreac0= mean(fbreac),
                        fbshare0 = mean(fbshare), 
                        fbengage0 = mean(fbengage), 
                        fbfan0 = mean(fbfan))

#consider increasing of fan each day
ggplot(fbif3sepdategrp, aes(x=day, y=fbfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(fbif3sepdategrp, aes(x=day, y=fbengage0, group = 1))+
  geom_line()+geom_point()


#****influence4****#

fbinflu4 = facebookdf[facebookdf$account_username == "wongnai.com",]
fbinflu4$fan = as.numeric(as.character(fbinflu4$fan))

fbgroupinflu4 = ddply(fbinflu4, "created_at", summarise , 
                      fbreac= mean(reaction),
                      fbshare = mean(share), 
                      fbengage = mean(engagement), 
                      fbfan = max(fan))

fbgroupinflu4$date = as.Date(fbgroupinflu4$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(fbgroupinflu4, aes(x=date, y=fbfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(fbgroupinflu4, aes(x=date, y=fbengage))+
  geom_line()+geom_point()

#separate date
fbif4sepdate = separate(fbgroupinflu4,date,c("year","month","day"),sep = "-")
#group again
fbif4sepdategrp = ddply(fbif4sepdate, "day", summarise , 
                        fbreac0= mean(fbreac),
                        fbshare0 = mean(fbshare), 
                        fbengage0 = mean(fbengage), 
                        fbfan0 = mean(fbfan))

#consider increasing of fan each day
ggplot(fbif4sepdategrp, aes(x=day, y=fbfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(fbif4sepdategrp, aes(x=day, y=fbengage0, group = 1))+
  geom_line()+geom_point()


#****influence5****#

fbinflu5 = facebookdf[facebookdf$account_username == "Unseen Tour Thailand",]
fbinflu5$fan = as.numeric(as.character(fbinflu5$fan))

fbgroupinflu5 = ddply(fbinflu5, "created_at", summarise , 
                      fbreac= mean(reaction),
                      fbshare = mean(share), 
                      fbengage = mean(engagement), 
                      fbfan = max(fan))

fbgroupinflu5$date = as.Date(fbgroupinflu5$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(fbgroupinflu5, aes(x=date, y=fbfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(fbgroupinflu5, aes(x=date, y=fbengage))+
  geom_line()+geom_point()

#separate date
fbif5sepdate = separate(fbgroupinflu5,date,c("year","month","day"),sep = "-")
#group again
fbif5sepdategrp = ddply(fbif5sepdate, "day", summarise , 
                        fbreac0= mean(fbreac),
                        fbshare0 = mean(fbshare), 
                        fbengage0 = mean(fbengage), 
                        fbfan0 = mean(fbfan))

#consider increasing of fan each day
ggplot(fbif5sepdategrp, aes(x=day, y=fbfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(fbif5sepdategrp, aes(x=day, y=fbengage0, group = 1))+
  geom_line()+geom_point()

######################
#---Graph Facebook---#
######################

#-----------------#
#-------BAR-------#
#-----------------#

#bar by fan all observations
ggplot(fbgrouporderfan, aes(x=fbfan, y=reorder(account_user,fbfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "All account order by fan")+
  xlab("Facebook Fan")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#bar by fan 50 observations
#fan
ggplot(fbgrouporderfan50, aes(x=fbfan, y=reorder(account_user,fbfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Facebook Fan")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#engagement
ggplot(fbgrouporderfan50, aes(x=fbengage, y=reorder(account_user,fbfan)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Facebook Engagement")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#reaction
ggplot(fbgrouporderfan50, aes(x=fbreac, y=reorder(account_user,fbfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Facebook Reaction")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#share
ggplot(fbgrouporderfan50, aes(x=fbshare, y=reorder(account_user,fbfan)) )+
  geom_bar(stat="identity",fill="darkolivegreen2")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Facebook Share")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#bar by engagement all observations
ggplot(fbgrouporderengage, aes(x=fbengage, y=reorder(account_user,fbengage)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = T)+
  labs(title = "Influencers order by Engagement")+
  xlab("Facebook Engagement")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#bar by engagement 50 obsevertions
#fan
ggplot(fbgrouporderengage50, aes(x=fbfan, y=reorder(account_user,fbengage)))+
  geom_bar(stat="identity",fill="violetred1")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by Engagement")+
  xlab("Facebook Fan")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#****engage****#
ggplot(fbgrouporderengage50, aes(x=fbengage, y=reorder(account_user,fbengage)))+
  geom_bar(stat="identity",fill="skyblue2")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by Engagement")+
  xlab("Facebook Engagement")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#-----------------#
#interactive graph#
#-----------------#

#All Fan
ggplotly(
  
  ggplot(fbgrouporderfan, aes(x=fbfan, y=reorder(account_user,fbfan),
                              text = paste0("<b>Account Username: </b>",account_user,"<br>",
                                            "<b>Fan: </b>",fbfan,"<br>"
                              )
  ))+
    geom_bar(stat="identity",fill="thistle1")+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by Fan")+
    xlab("Facebook Fan")+ 
    ylab("Account Username")+
    theme(text=element_text(colour = "oldlace"),
          title = element_text(colour = "oldlace", size =10, face="bold"),
    )
  , tooltip = "text"
)

#All engagement
ggplotly(
  
  ggplot(fbgrouporderengage, aes(x=fbengage, y=reorder(account_user,fbengage),
                                 text = paste0("<b>Account Username: </b>",account_user,"<br>",
                                               "<b>Engagement: </b>",fbengage,"<br>"
                                 )
  ))+
    geom_bar(stat="identity",fill="skyblue2")+
    theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by Engagement")+
    xlab("Facebook Engagement")+ 
    ylab("Account Username")+
    theme(text=element_text(colour = "rosybrown4"),
          title = element_text(colour = "rosybrown4", size =10, face="bold"),
    )
  , tooltip = "text"
)


#********************#
#***Change on Date***#
#********************#

fbfilter0 = facebookdf[facebookdf$account_username %in% 
                         c("Lowcostcosplay","SALE HERE",
                           "wongnai.com"
                           ,"Unseen Tour Thailand"),]

fbinflu3 = facebookdf[grep("^Starvingtime",facebookdf$account_username),]
fbinflu3$fan = as.numeric(as.character(fbinflu3$fan))

fbrealsepinflu3 = separate(fbinflu3,account_username, 
                           c("account_username","useless"),sep = 'à')
drop = c("useless")
fbdrop = fbrealsepinflu3[ ,(names(fbrealsepinflu3) != drop)]

fbfilter = rbind(fbfilter0,fbdrop)

fbfilgrp = ddply(fbfilter, c("account_username","created_at"), summarise , 
                 fbreac= as.numeric(mean(reaction)),
                 fbshare = as.numeric(mean(share)), 
                 fbengage = as.numeric(mean(engagement)), 
                 fbfan = as.numeric(max(fan))
)

fbfilgrp$date = as.Date(fbfilgrp$created_at, '%Y-%m-%d')
fbfilsepdate = separate(fbfilgrp,date,c("year","month","day"),sep = "-")

fbfilsepdate$year = as.integer(fbfilsepdate$year)
fbfilsepdate$month = as.integer(fbfilsepdate$month)
fbfilsepdate$day = as.integer(fbfilsepdate$day)

fbfilsepdategrp = ddply(fbfilsepdate, c("account_username","day"), summarise , 
                        fbreac0 = mean(fbreac),
                        fbshare0 = mean(fbshare), 
                        fbeng0 = mean(fbengage), 
                        fbfan0 = max(fbfan))
glimpse(fbfilsepdategrp)

#---------------------------#
#Interactive Graph with date#
#---------------------------#
#FAN
ggplotly(
  ggplot(fbfilsepdategrp,aes(x=day,y=fbfan0,group=account_username, 
                             color=account_username,
                             text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                           "<b>Fan: </b>",fbfan0,"<br>",
                                           "<b>Day/Month/Year: </b>",day,"/12/2020 <br>")
  ))+
    geom_line()+
    geom_path(size = 0.8, lineend = "round")+
    geom_point(size = 1.5)+
    scale_x_continuous("Days of December 2020",breaks = c(1:30))+
    scale_y_continuous("Fan")+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Fan of Top 5 influencers of Facebook"),
  tooltip = 'text'
)

#Engagement
ggplotly(
  ggplot(fbfilsepdategrp,aes(x=day,y=fbeng0,group=account_username, 
                             color=account_username,
                             text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                           "<b>Engagement: </b>",fbeng0,"<br>",
                                           "<b>Day/Month/Year: </b>",day,"/12/2020 <br>")
  ))+
    geom_line()+
    geom_path(size = 0.8, lineend = "round")+
    geom_point(size = 1.5)+
    scale_x_continuous("Days of December 2020",breaks = c(1:30))+
    scale_y_continuous("Engagement")+
    theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
    labs(title = "Engagement of Top 5 influencers of Facebook")
  , tooltip = 'text'
)

#-------------------#
#Animation with date#
#-------------------#

#animation by fan
fbanimfan = ggplot(fbfilsepdategrp,aes(x=day,y=fbfan0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Days of December 2020",breaks = c(1:30))+
  scale_y_continuous("Fan")+
  theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
  labs(title = "Fan of Top 5 influencers of Facebook")+
  transition_reveal(day)

animate(fbanimfan, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("FBanimfan.gif")

#animation by engagement
fbanimeng = ggplot(fbfilsepdategrp,aes(x=day,y=fbeng0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Days of December 2020",breaks = c(1:30))+
  scale_y_continuous("Engagement")+
  theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
  labs(title = "Engagement of Top 5 influencers of Facebook")+
  transition_reveal(day)

animate(fbanimeng, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("FBanimeng.gif")

###############
#---Youtube---#
###############

youtubedf = read.csv("C:/Users/ACER/Downloads/YouTube_Data.csv")
names(youtubedf)

ytgroup = ddply(youtubedf, "account_username", summarise , 
                ytlike= mean(like),
                ytview = mean(view), 
                ytengage = mean(engagement), 
                ytfan = max(fan))


#####################
#---Data Cleaning---#
#####################

ytrealsep = separate(ytgroup,account_username, c("account_username","useless"),
                     sep = 'à')
ytrealsep = separate(ytrealsep,account_username, c("account_username","useless"),
                     sep = 'ã')

glimpse(ytrealsep)

ytremove = ytrealsep[ytrealsep$account_user != ""&ytrealsep$account_user !='"',]

ytremove = ytremove[names(ytremove) != 'useless']

#for (i in 1:nrow(ytgroup)) {
#  if( ytgroup2$ytfan[i] == 0 ){
#    ytgroup2 = ytgroup2[-c(i),]
#  }
#  else{
#    next
#  }
#}


#############
#---order---#
#############

#sort by fan
ytgroup2orderfan = ytremove[order(-ytremove$ytfan),]
ytgroup2orderfan50 =ytgroup2orderfan[1:50,]

#sort by view
ytgroup2orderview = ytremove[order(-ytremove$ytfan),]
ytgroup2orderview50 =ytgroup2orderview[1:50,]


############
#---bar1---# sort by fan
############

#****fan****#
ggplot(ytgroup2orderfan50, aes(x=ytfan, y=reorder(account_username,ytfan)))+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#engage
ggplot(ytgroup2orderfan50, aes(x=ytengage, y=reorder(account_username,ytfan)))+
  geom_bar(stat="identity",fill="lightblue",colour="black")

#like
ggplot(ytgroup2orderfan50, aes(x=ytlike, y=reorder(account_username,ytfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1",colour="black")

#view
ggplot(ytgroup2orderfan50, aes(x=ytview, y=reorder(account_username,ytfan)) )+
  geom_bar(stat="identity",fill="darkolivegreen2",colour="black")

############
#---bar2---# sort by view
############

#fan
ggplot(ytgroup2orderview50, aes(x=ytfan, y=reorder(account_username,ytview)))+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#engage
ggplot(ytgroup2orderview50, aes(x=ytengage, y=reorder(account_username,ytview)))+
  geom_bar(stat="identity",fill="lightblue",colour="black")

#like
ggplot(ytgroup2orderview50, aes(x=ytlike, y=reorder(account_username,ytview)) )+
  geom_bar(stat="identity",fill="lightsalmon1",colour="black")

#****view****#
ggplot(ytgroup2orderview50, aes(x=ytview, y=reorder(account_username,ytview)) )+
  geom_bar(stat="identity",fill="darkolivegreen2",colour="black")


######################
#---plot with date---#
######################

require(ggplot2)

#SORT BY FAN

#****influence 1****#

ytinflu1 = youtubedf[youtubedf$account_username == "Kaykai Salaider",]
glimpse(ytinflu1)
ytgroupinflu1 = ddply(ytinflu1, "created_at", summarise , 
                      ytlike= mean(like),
                      ytview = mean(view), 
                      ytengage = mean(engagement), 
                      ytfan = max(fan))

ytgroupinflu1$date = as.Date(ytgroupinflu1$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ytgroupinflu1, aes(x=date, y=ytfan))+
  geom_line()+geom_point()
#consider view in each post
ggplot(ytgroupinflu1, aes(x=date, y=ytview))+
  geom_line()+geom_point()

#separate date
ytif1sepdate = separate(ytgroupinflu1,date,c("year","month","day"),sep = "-")
#group again
ytif1sepdategrp = ddply(ytif1sepdate, "month", summarise , 
                        ytlike0= mean(ytlike),
                        ytview0 = mean(ytview), 
                        ytengage0 = mean(ytengage), 
                        ytfan0 = max(ytfan))


#consider increasing of fan each day
ggplot(ytif1sepdategrp, aes(x=month, y=ytfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ytif1sepdategrp, aes(x=month, y=ytengage0, group = 1))+
  geom_line()+geom_point()


#****influence 2****#

ytinflu2 = youtubedf[youtubedf$account_username == "zbing z.",]

ytgroupinflu2 = ddply(ytinflu2, "created_at", summarise , 
                      ytlike= mean(like),
                      ytview = mean(view), 
                      ytengage = mean(engagement), 
                      ytfan = max(fan))

ytgroupinflu2$date = as.Date(ytgroupinflu2$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ytgroupinflu2, aes(x=date, y=ytfan))+
  geom_line()+geom_point()
#consider view in each post
ggplot(ytgroupinflu2, aes(x=date, y=ytview))+
  geom_line()+geom_point()

#separate date
ytif2sepdate = separate(ytgroupinflu2,date,c("year","month","day"),sep = "-")
#group again
ytif2sepdategrp = ddply(ytif2sepdate, "month", summarise , 
                        ytlike0= mean(ytlike),
                        ytview0 = mean(ytview), 
                        ytengage0 = mean(ytengage), 
                        ytfan0 = max(ytfan))


#consider increasing of fan each day
ggplot(ytif2sepdategrp, aes(x=month, y=ytfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ytif2sepdategrp, aes(x=month, y=ytengage0, group = 1))+
  geom_line()+geom_point()

#****influence 3****#

ytinflu3 = youtubedf[youtubedf$account_username == "Bie The Ska",]

ytgroupinflu3 = ddply(ytinflu3, "created_at", summarise , 
                      ytlike= mean(like),
                      ytview = mean(view), 
                      ytengage = mean(engagement), 
                      ytfan = max(fan))

ytgroupinflu3$date = as.Date(ytgroupinflu3$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ytgroupinflu3, aes(x=date, y=ytfan))+
  geom_line()+geom_point()
#consider view in each post
ggplot(ytgroupinflu3, aes(x=date, y=ytview))+
  geom_line()+geom_point()

#separate date
ytif3sepdate = separate(ytgroupinflu3,date,c("year","month","day"),sep = "-")
#group again
ytif3sepdategrp = ddply(ytif3sepdate, "month", summarise , 
                        ytlike0= mean(ytlike),
                        ytview0 = mean(ytview), 
                        ytengage0 = mean(ytengage), 
                        ytfan0 = max(ytfan))


#consider increasing of fan each day
ggplot(ytif3sepdategrp, aes(x=month, y=ytfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ytif3sepdategrp, aes(x=month, y=ytengage0, group = 1))+
  geom_line()+geom_point()

#****influence 4****#

ytinflu4 = youtubedf[youtubedf$account_username == "My Mate Nate",]

ytgroupinflu4 = ddply(ytinflu4, "created_at", summarise , 
                      ytlike= mean(like),
                      ytview = mean(view), 
                      ytengage = mean(engagement), 
                      ytfan = max(fan))

ytgroupinflu4$date = as.Date(ytgroupinflu4$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ytgroupinflu4, aes(x=date, y=ytfan))+
  geom_line()+geom_point()
#consider view in each post
ggplot(ytgroupinflu4, aes(x=date, y=ytview))+
  geom_line()+geom_point()

#separate date
ytif4sepdate = separate(ytgroupinflu4,date,c("year","month","day"),sep = "-")
#group again
ytif4sepdategrp = ddply(ytif4sepdate, "month", summarise , 
                        ytlike0= mean(ytlike),
                        ytview0 = mean(ytview), 
                        ytengage0 = mean(ytengage), 
                        ytfan0 = max(ytfan))


#consider increasing of fan each day
ggplot(ytif4sepdategrp, aes(x=month, y=ytfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ytif4sepdategrp, aes(x=month, y=ytengage0, group = 1))+
  geom_line()+geom_point()


#****influence 5****#

ytinflu5 = youtubedf[youtubedf$account_username == "UDiEX2",]

ytgroupinflu5 = ddply(ytinflu5, "created_at", summarise , 
                      ytlike= mean(like),
                      ytview = mean(view), 
                      ytengage = mean(engagement), 
                      ytfan = max(fan))

ytgroupinflu5$date = as.Date(ytgroupinflu5$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ytgroupinflu5, aes(x=date, y=ytfan))+
  geom_line()+geom_point()
#consider view in each post
ggplot(ytgroupinflu5, aes(x=date, y=ytview))+
  geom_line()+geom_point()

#separate date
ytif5sepdate = separate(ytgroupinflu5,date,c("year","month","day"),sep = "-")
#group again
ytif5sepdategrp = ddply(ytif5sepdate, "month", summarise , 
                        ytlike0= mean(ytlike),
                        ytview0 = mean(ytview), 
                        ytengage0 = mean(ytengage), 
                        ytfan0 = max(ytfan))


#consider increasing of fan each day
ggplot(ytif5sepdategrp, aes(x=month, y=ytfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ytif5sepdategrp, aes(x=month, y=ytengage0, group = 1))+
  geom_line()+geom_point()


#####################
#---Graph Youtube---#
#####################


#-----------------#
#-------BAR-------#
#-----------------#

#bar by fan all observations
ggplot(ytgroup2orderfan, aes(x=ytfan, y=reorder(account_username,ytfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "All account order by fan")+
  xlab("Youtube Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,5000000,10000000),
                     labels = c("0","5M","10M"))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#bar by fan 50 observations
#fan
ggplot(ytgroup2orderfan50, aes(x=ytfan, y=reorder(account_username,ytfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Youtube Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,5000000,10000000),
                     labels = c("0","5M","10M"))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#engagement
ggplot(ytgroup2orderfan50, aes(x=ytengage, y=reorder(account_username,ytfan)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Youtube Engagement")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#like
ggplot(ytgroup2orderfan50, aes(x=ytlike, y=reorder(account_username,ytfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Youtube Like")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#view
ggplot(ytgroup2orderfan50, aes(x=ytview, y=reorder(account_username,ytfan)) )+
  geom_bar(stat="identity",fill="darkolivegreen2")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Youtube View")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,2000000,4000000),
                     labels = c("0","2M","4M"))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#bar by engagement all observations
ggplot(ytgroup2orderview, aes(x=ytview, y=reorder(account_username,ytview)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = T)+
  labs(title = "Influencers order by View")+
  xlab("View on Youtube")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,2000000,4000000),
                     labels = c("0","2M","4M"))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))


#bar by engagement 50 observations
#fan
ggplot(ytgroup2orderview50, aes(x=ytfan, y=reorder(account_username,ytview)))+
  geom_bar(stat="identity",fill="violetred1")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by View")+
  xlab("Youtube Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,5000000,10000000),
                     labels = c("0","5M","10M"))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#View
ggplot(ytgroup2orderview50, aes(x=ytview, y=reorder(account_username,ytview)))+
  geom_bar(stat="identity",fill="skyblue2")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by View")+
  xlab("View on Youtube")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,2000000,4000000),
                     labels = c("0","2M","4M"))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))


#-----------------#
#interactive graph#
#-----------------#

#All Fan
ggplotly(
  
  ggplot(ytgroup2orderfan, aes(x=ytfan, y=reorder(account_username,ytfan),
                               text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                             "<b>Fan: </b>",ytfan,"<br>"
                               )
  ))+
    geom_bar(stat="identity",fill="thistle1")+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by Fan")+
    xlab("Youtube Fan")+ 
    ylab("Account Username")+
    scale_x_continuous(breaks = c(0,5000000,10000000),
                       labels = c("0","5M","10M"))+
    theme(text=element_text(colour = "oldlace"),
          title = element_text(colour = "oldlace", size =10, face="bold"),
    )
  , tooltip = "text"
)

#All view
ggplotly(
  
  ggplot(ytgroup2orderview, aes(x=ytview, y=reorder(account_username,ytview),
                                text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                              "<b>Engagement: </b>",ytview,"<br>"
                                )
  ))+
    geom_bar(stat="identity",fill="skyblue2")+
    theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by View")+
    xlab("Youtube View")+ 
    ylab("Account Username")+
    scale_x_continuous(breaks = c(0,2000000,4000000),
                       labels = c("0","2M","4M"))+
    theme(text=element_text(colour = "rosybrown4"),
          title = element_text(colour = "rosybrown4", size =10, face="bold"),
    )
  , tooltip = "text"
)


#********************#
#***Change on Date***#
#********************#

ytfilter = youtubedf[youtubedf$account_username %in% 
                       c("Kaykai Salaider","zbing z.",
                         "Bie The Ska","My Mate Nate"
                         ,"UDiEX2"),]


ytfilgrp = ddply(ytfilter, c("account_username","created_at"), summarise , 
                 ytlike= as.numeric(mean(like)),
                 ytview = as.numeric(mean(view)), 
                 ytengage = as.numeric(mean(engagement)), 
                 ytfan = as.numeric(max(fan)))


ytfilgrp$date = as.Date(ytfilgrp$created_at, '%Y-%m-%d')
ytfilsepdate = separate(ytfilgrp,date,c("year","month","day"),sep = "-")

ytfilsepdate$year = as.integer(ytfilsepdate$year)
ytfilsepdate$month = as.integer(ytfilsepdate$month)
ytfilsepdate$day = as.integer(ytfilsepdate$day)


ytfilsepdategrp = ddply(ytfilsepdate, c("account_username","month"), summarise , 
                        ytlike0= mean(ytlike),
                        ytview0 = mean(ytview), 
                        ytengage0 = mean(ytengage), 
                        ytfan0 = max(ytfan))

glimpse(ytfilsepdategrp)

#---------------------------#
#Interactive Graph with date#
#---------------------------#
#FAN
ggplotly(
  ggplot(ytfilsepdategrp,aes(x=month,y=ytfan0,group=account_username, 
                             color=account_username,
                             text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                           "<b>Fan: </b>",ytfan0,"<br>",
                                           "<b>Month: </b>",month,"/2020 <br>")
  ))+
    geom_line()+
    geom_path(size = 0.8, lineend = "round")+
    geom_point(size = 1.5)+
    scale_x_continuous("Month of 2020",breaks = c(1:12))+
    scale_y_continuous("Fan",breaks = c(8000000,10000000,
                                        12000000,14000000),
                       labels = c("8M","10M","12M","14M"))+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Fan of Top 5 Youtubers"),
  tooltip = "text"
)

#View
ggplotly(ggplot(ytfilsepdategrp,aes(x=month,y=ytview0,group=account_username, 
                                    color=account_username,
                                    text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                                  "<b>View: </b>",ytview0,"<br>",
                                                  "<b>Month: </b>",month,"/2020 <br>")
)
)+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Month of 2020",breaks = c(1:30))+
  scale_y_continuous("View",breaks = c(2000000,4000000,6000000,8000000)
                     ,labels = c("2M","4M","6M","8M"))+
  theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
  labs(title = "View of Top 5 Youtubers"),
tooltip = "text"
)

#-------------------#
#Animation with date#
#-------------------#
#animation by fan 
ytanimfan = ggplot(ytfilsepdategrp,aes(x=month,y=ytfan0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Month of 2020",breaks = c(1:12))+
  scale_y_continuous("Fan",breaks = c(8000000,10000000,
                                      12000000,14000000),
                     labels = c("8M","10M","12M","14M"))+
  theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
  labs(title = "Fan of Top 5 Youtubers")+
  transition_reveal(month)

animate(ytanimfan, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("YTanimfan.gif")

#animation by view
ytanimview = ggplot(ytfilsepdategrp,aes(x=month,y=ytview0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Month of 2020",breaks = c(1:30))+
  scale_y_continuous("View",breaks = c(2000000,4000000,6000000,8000000)
                     ,labels = c("2M","4M","6M","8M"))+
  theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
  labs(title = "View of Top 5 Youtubers")+
  transition_reveal(month)


animate(ytanimview, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("YTanimview.gif")

###############
#---Twitter---#
###############

twitterdf = read.csv("C:/Users/ACER/Downloads/Twitter_Data.csv")
names(twitterdf)

twitdforder = twitterdf[order(twitterdf$account_username),c(4,9,10,12,17)]

ttgroup = ddply(twitterdf, "account_username", summarise , 
                ttfav = mean(favorite),
                ttre = mean(retweet) , 
                tteng = mean(engagement), 
                ttfan = max(fan))

#############
#---order---#
#############

#fan
ttgrouporderfan =  ttgroup[order(-ttgroup$ttfan),]
ttgrouporderfan50 = ttgrouporderfan[1:50,]

#engagement
ttgroupordereng =  ttgroup[order(-ttgroup$tteng),]
ttgroupordereng50 = ttgroupordereng[1:50,]


############
#---bar1---# sort by fan
############

#***********fan************
ggplot(ttgrouporderfan50, aes(x=ttfan, y=reorder(account_username,ttfan)) )+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#engage
ggplot(ttgrouporderfan50, aes(x=tteng, y=reorder(account_username,ttfan)) )+
  geom_bar(stat="identity",fill="lightblue",colour="black")

#favorite
ggplot(ttgrouporderfan50, aes(x=ttfav, y=reorder(account_username,ttfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1",colour="black")

#retweet
ggplot(ttgrouporderfan50, aes(x=ttre, y=reorder(account_username,ttfan)) )+
  geom_bar(stat="identity",fill="darkolivegreen2",colour="black")


############
#---bar2---# sort by engagement
############

#fan
ggplot(ttgroupordereng50, aes(x=ttfan, y=reorder(account_username,tteng)) )+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#***********engagement************
ggplot(ttgroupordereng50, aes(x=tteng, y=reorder(account_username,tteng)) )+
  geom_bar(stat="identity",fill="lightblue",colour="black")

#favorite
ggplot(ttgroupordereng50, aes(x=ttfav, y=reorder(account_username,tteng)) )+
  geom_bar(stat="identity",fill="lightsalmon1",colour="black")

#retweet
ggplot(ttgroupordereng50, aes(x=ttre, y=reorder(account_username,tteng)) )+
  geom_bar(stat="identity",fill="darkolivegreen2",colour="black")


######################
#---plot with date---#
######################
require(ggplot2)

#SORT BY FAN

#****influence 1****#

ttinflu1 = twitterdf[twitterdf$account_username == "UnderbedDara",]
glimpse(ttinflu1)
ttgroupinflu1 = ddply(ttinflu1, "created_at", summarise , 
                      ttfav = mean(favorite),
                      ttre = mean(retweet) , 
                      tteng = mean(engagement), 
                      ttfan = max(fan))

ttgroupinflu1$date = as.Date(ttgroupinflu1$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ttgroupinflu1, aes(x=date, y=ttfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(ttgroupinflu1, aes(x=date, y=tteng))+
  geom_line()+geom_point()

#separate date
ttif1sepdate = separate(ttgroupinflu1,date,c("year","month","day"),sep = "-")
#group again
ttif1sepdategrp = ddply(ttif1sepdate, "month", summarise , 
                        ttfav0 = mean(ttfav),
                        ttre0 = mean(ttre), 
                        tteng0 = mean(tteng), 
                        ttfan0 = max(ttfan))

#consider increasing of fan each day
ggplot(ttif1sepdategrp, aes(x=month, y=ttfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ttif1sepdategrp, aes(x=month, y=tteng0, group = 1))+
  geom_line()+geom_point()

#****influence 2****#

ttinflu2 = twitterdf[twitterdf$account_username == "THarmharm",]

ttgroupinflu2 = ddply(ttinflu2, "created_at", summarise , 
                      ttfav = mean(favorite),
                      ttre = mean(retweet) , 
                      tteng = mean(engagement), 
                      ttfan = max(fan))

ttgroupinflu2$date = as.Date(ttgroupinflu2$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ttgroupinflu2, aes(x=date, y=ttfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(ttgroupinflu2, aes(x=date, y=tteng))+
  geom_line()+geom_point()

#separate date
ttif2sepdate = separate(ttgroupinflu2,date,c("year","month","day"),sep = "-")
#group again
ttif2sepdategrp = ddply(ttif2sepdate, "month", summarise , 
                        ttfav0 = mean(ttfav),
                        ttre0 = mean(ttre), 
                        tteng0 = mean(tteng), 
                        ttfan0 = max(ttfan))

#consider increasing of fan each day
ggplot(ttif2sepdategrp, aes(x=month, y=ttfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ttif2sepdategrp, aes(x=month, y=tteng0, group = 1))+
  geom_line()+geom_point()

#****influence 3****#

ttinflu3 = twitterdf[twitterdf$account_username == "samakhom",]

ttgroupinflu3 = ddply(ttinflu3, "created_at", summarise , 
                      ttfav = mean(favorite),
                      ttre = mean(retweet) , 
                      tteng = mean(engagement), 
                      ttfan = max(fan))

ttgroupinflu3$date = as.Date(ttgroupinflu3$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ttgroupinflu3, aes(x=date, y=ttfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(ttgroupinflu3, aes(x=date, y=tteng))+
  geom_line()+geom_point()

#separate date
ttif3sepdate = separate(ttgroupinflu3,date,c("year","month","day"),sep = "-")
#group again
ttif3sepdategrp = ddply(ttif3sepdate, "month", summarise , 
                        ttfav0 = mean(ttfav),
                        ttre0 = mean(ttre), 
                        tteng0 = mean(tteng), 
                        ttfan0 = max(ttfan))

#consider increasing of fan each day
ggplot(ttif3sepdategrp, aes(x=month, y=ttfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ttif3sepdategrp, aes(x=month, y=tteng0, group = 1))+
  geom_line()+geom_point()

#****influence 4****#

ttinflu4 = twitterdf[twitterdf$account_username == "wongnai",]

ttgroupinflu4 = ddply(ttinflu4, "created_at", summarise , 
                      ttfav = mean(favorite),
                      ttre = mean(retweet) , 
                      tteng = mean(engagement), 
                      ttfan = max(fan))

ttgroupinflu4$date = as.Date(ttgroupinflu4$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ttgroupinflu4, aes(x=date, y=ttfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(ttgroupinflu4, aes(x=date, y=tteng))+
  geom_line()+geom_point()

#separate date
ttif4sepdate = separate(ttgroupinflu4,date,c("year","month","day"),sep = "-")
#group again
ttif4sepdategrp = ddply(ttif4sepdate, "month", summarise , 
                        ttfav0 = mean(ttfav),
                        ttre0 = mean(ttre), 
                        tteng0 = mean(tteng), 
                        ttfan0 = max(ttfan))

#consider increasing of fan each day
ggplot(ttif4sepdategrp, aes(x=month, y=ttfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ttif4sepdategrp, aes(x=month, y=tteng0, group = 1))+
  geom_line()+geom_point()

#****influence 5****#

ttinflu5 = twitterdf[twitterdf$account_username == "kidmakk",]

ttgroupinflu5 = ddply(ttinflu5, "created_at", summarise , 
                      ttfav = mean(favorite),
                      ttre = mean(retweet) , 
                      tteng = mean(engagement), 
                      ttfan = max(fan))

ttgroupinflu5$date = as.Date(ttgroupinflu5$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(ttgroupinflu5, aes(x=date, y=ttfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(ttgroupinflu5, aes(x=date, y=tteng))+
  geom_line()+geom_point()

#separate date
ttif5sepdate = separate(ttgroupinflu5,date,c("year","month","day"),sep = "-")
#group again
ttif5sepdategrp = ddply(ttif5sepdate, "month", summarise , 
                        ttfav0 = mean(ttfav),
                        ttre0 = mean(ttre), 
                        tteng0 = mean(tteng), 
                        ttfan0 = max(ttfan))

#consider increasing of fan each day
ggplot(ttif5sepdategrp, aes(x=month, y=ttfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(ttif5sepdategrp, aes(x=month, y=tteng0, group = 1))+
  geom_line()+geom_point()


#####################
#---Graph Twitter---#
#####################

#-----------------#
#-------BAR-------#
#-----------------#

#bar by fan all observations
ggplot(ttgrouporderfan, aes(x=ttfan, y=reorder(account_username,ttfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "All account order by fan")+
  xlab("Twitter Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,1000000,2000000,3000000),
                     labels = c("0","1M","2M",'3M'))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#bar by fan 50 observations
#fan
ggplot(ttgrouporderfan50, aes(x=ttfan, y=reorder(account_username,ttfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Twitter Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,1000000,2000000,3000000),
                     labels = c("0","1M","2M",'3M'))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#engagement
ggplot(ttgrouporderfan50, aes(x=tteng, y=reorder(account_username,ttfan)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Twitter Engagement")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,5000,10000),
                     labels = c("0","5,000","10,000"))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#favorite
ggplot(ttgrouporderfan50, aes(x=ttfav, y=reorder(account_username,ttfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Twitter Favorite")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,2500,5000,7500,10000),
                     labels = c("0",'2,500',"5,000",'7,500',"10,000"))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#retweet
ggplot(ttgrouporderfan50, aes(x=ttre, y=reorder(account_username,ttfan)) )+
  geom_bar(stat="identity",fill="darkolivegreen2")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Twitter Retweet")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,2000,4000,6000),
                     labels = c("0","2,000","4,000",'6,000'))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))


#bar by engagement all observations
ggplot(ttgrouporderfan, aes(x=tteng, y=reorder(account_username,tteng)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = T)+
  labs(title = "Influencers order by Engagement")+
  xlab("Twitter Engagement")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,5000,10000),
                     labels = c("0","5,000","10,000"))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))


#bar by engagement 50 observations
#fan
ggplot(ttgrouporderfan50, aes(x=ttfan, y=reorder(account_username,tteng)))+
  geom_bar(stat="identity",fill="violetred1")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by Engagement")+
  xlab("Twitter Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,1000000,2000000,3000000),
                     labels = c("0","1M","2M",'3M'))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#engagement
ggplot(ttgrouporderfan50, aes(x=tteng, y=reorder(account_username,tteng)))+
  geom_bar(stat="identity",fill="skyblue2")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by Engagement")+
  xlab("Twitter Engagement")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,5000,10000),
                     labels = c("0","5,000","10,000"))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#-----------------#
#interactive graph#
#-----------------#

#All Fan
ggplotly(
  
  ggplot(ttgrouporderfan, aes(x=ttfan, y=reorder(account_username,ttfan),
                              text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                            "<b>Fan: </b>",ttfan,"<br>"
                              )
  ))+
    geom_bar(stat="identity",fill="thistle1")+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by Fan")+
    xlab("Twitter Fan")+ 
    ylab("Account Username")+
    scale_x_continuous(breaks = c(0,1000000,2000000,
                                  3000000), labels = c("0","1M","2M","3M"))+
    theme(text=element_text(colour = "oldlace"),
          title = element_text(colour = "oldlace", size =10, face="bold"),
    )
  , tooltip = "text"
)

#All engagement
ggplotly(
  
  ggplot(ttgroupordereng, aes(x=tteng, y=reorder(account_username,tteng),
                              text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                            "<b>Engagement: </b>",tteng,"<br>"
                              )
  ))+
    geom_bar(stat="identity",fill="skyblue2")+
    theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by Engagement")+
    xlab("Twitter Engagement")+ 
    ylab("Account Username")+
    scale_x_continuous(breaks = c(0,5000,10000),
                       labels = c("0","5,000","10,000"))+
    theme(text=element_text(colour = "rosybrown4"),
          title = element_text(colour = "rosybrown4", size =10, face="bold"),
    )
  , tooltip = "text"
)

#********************#
#***Change on Date***#
#********************#


ttfilter = twitterdf[twitterdf$account_username %in% 
                       c("UnderbedDara","THarmharm",
                         "samakhom","wongnai"
                         ,"kidmakk"),]


ttfilgrp = ddply(ttfilter, c("account_username","created_at"), summarise , 
                 ttfav = as.numeric(mean(favorite)),
                 ttre = as.numeric(mean(retweet)) , 
                 tteng = as.numeric(mean(engagement)), 
                 ttfan = as.numeric(max(fan)))


ttfilgrp$date = as.Date(ttfilgrp$created_at, '%Y-%m-%d')
ttfilsepdate = separate(ttfilgrp,date,c("year","month","day"),sep = "-")

ttfilsepdate$year = as.integer(ttfilsepdate$year)
ttfilsepdate$month = as.integer(ttfilsepdate$month)
ttfilsepdate$day = as.integer(ttfilsepdate$day)


ttfilsepdategrp = ddply(ttfilsepdate, c("account_username","month"), summarise , 
                        ttfav0 = mean(ttfav),
                        ttre0 = mean(ttre), 
                        tteng0 = mean(tteng), 
                        ttfan0 = max(ttfan))

glimpse(ttfilsepdategrp)

#---------------------------#
#Interactive Graph with date#
#---------------------------#
#FAN
ggplotly(
  
  ggplot(ttfilsepdategrp,aes(x=month,y=ttfan0,group=account_username, 
                             color=account_username,
                             text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                           "<b>Fan: </b>",ttfan0,"<br>",
                                           "<b>Month: </b>",month,"/2020 <br>")
  ))+
    geom_line()+
    geom_path(size = 0.8, lineend = "round")+
    geom_point(size = 1.5)+
    scale_x_continuous("Month of 2020",breaks = c(1:12))+
    scale_y_continuous("Fan",breaks = c(2500000,3000000), labels = c('2,500,000','3,000,000'))+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Fan of Top 5 influencers on Twitter"),
  tooltip = "text"
  
)  

#ENGAGEMENT
ggplotly(
  
  ggplot(ttfilsepdategrp,aes(x=month,y=tteng0,group=account_username, 
                             color=account_username,
                             text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                           "<b>Engagement: </b>",tteng0,"<br>",
                                           "<b>Month: </b>",month,"/2020 <br>")
  ))+
    geom_line()+
    geom_path(size = 0.8, lineend = "round")+
    geom_point(size = 1.5)+
    scale_x_continuous("Month of 2020",breaks = c(1:30))+
    scale_y_continuous("Engagement",breaks = c(5000,10000,15000),labels = c('5,000','10,000','15,000'))+
    theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
    labs(title = "Engagement of Top 5 influencers on Twitter"),
  tooltip = "text"
)

#-------------------#
#Animation with date#
#-------------------#

#animation by fan 
ttanimfan = ggplot(ttfilsepdategrp,aes(x=month,y=ttfan0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Month of 2020",breaks = c(1:12))+
  scale_y_continuous("Fan",breaks = c(2500000,3000000), labels = c('2,500,000','3,000,000'))+
  theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
  labs(title = "Fan of Top 5 influencers on Twitter")+
  transition_reveal(month)


animate(ttanimfan, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("TTanimfan.gif")

#animation by engagement
ttanimeng = ggplot(ttfilsepdategrp,aes(x=month,y=tteng0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Month of 2020",breaks = c(1:30))+
  scale_y_continuous("Engagement",breaks = c(5000,10000,15000),labels = c('5,000','10,000','15,000'))+
  theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
  labs(title = "Engagement of Top 5 influencers on Twitter")+
  transition_reveal(month)


animate(ttanimeng, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("TTanimeng.gif")



#################
#---Instagram---#
#################

instagramdf = read.csv("C:/Users/ACER/Downloads/Instagram_Data.csv")

library(plyr)
iggroup = ddply(instagramdf, c("account_username"),summarise, 
                igeng = mean(engagement),
                iglike = mean(like),
                igfan=max(fan))

sum(is.na(instagramdf))
glimpse(iggroup)


###########
#--order--#
###########
#sort by fan
iggrouporderfan = iggroup[order(-iggroup$igfan),]
iggrouporderfan50 =iggrouporderfan[1:50,]

#sort by engagement
iggroupordereng = iggroup[order(-iggroup$igeng),]
iggroupordereng50 =iggroupordereng[1:50,]

############
#---bar1---# sort by fan
############

#****fan****#
ggplot(iggrouporderfan50, aes(x=igfan, y=reorder(account_username,igfan)))+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#engage
ggplot(iggrouporderfan50, aes(x=igeng, y=reorder(account_username,igfan)))+
  geom_bar(stat="identity",fill="lightblue",colour="black")

#like
ggplot(iggrouporderfan50, aes(x=iglike, y=reorder(account_username,igfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1",colour="black")


############
#---bar2---# sort by engagement
############

#fan
ggplot(iggroupordereng50, aes(x=igfan, y=reorder(account_username,igeng)))+
  geom_bar(stat="identity",fill="thistle1",colour="black")

#****engage****#
ggplot(iggroupordereng50, aes(x=igeng, y=reorder(account_username,igeng)))+
  geom_bar(stat="identity",fill="lightblue",colour="black")

#like
ggplot(iggroupordereng50, aes(x=iglike, y=reorder(account_username,igeng)) )+
  geom_bar(stat="identity",fill="lightsalmon1",colour="black")


######################
#---plot with date---#
######################
require(ggplot2)

#SORT BY FAN

#****influence 1****#

iginflu1 = instagramdf[instagramdf$account_username == "pimtha",]

iggroupinflu1 = ddply(iginflu1, "created_at", summarise , 
                      igeng = mean(engagement),
                      iglike = mean(like),
                      igfan=mean(fan))

iggroupinflu1$date = as.Date(iggroupinflu1$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(iggroupinflu1, aes(x=date, y=igfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(iggroupinflu1, aes(x=date, y=igeng))+
  geom_line()+geom_point()

#separate date
igif1sepdate = separate(iggroupinflu1,date,c("year","month","day"),sep = "-")
#group again
igif1sepdategrp = ddply(igif1sepdate, "month", summarise , 
                        igeng0 = mean(igeng),
                        iglike0 = mean(iglike),
                        igfan0 = mean(igfan))

#consider increasing of fan each day
ggplot(igif1sepdategrp, aes(x=month, y=igfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(igif1sepdategrp, aes(x=month, y=igeng0, group = 1))+
  geom_line()+geom_point()


#****influence 2****#

iginflu2 = instagramdf[instagramdf$account_username == "kaykai_ntch",]

iggroupinflu2 = ddply(iginflu2, "created_at", summarise , 
                      igeng = mean(engagement),
                      iglike = mean(like),
                      igfan=mean(fan))

iggroupinflu2$date = as.Date(iggroupinflu2$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(iggroupinflu2, aes(x=date, y=igfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(iggroupinflu2, aes(x=date, y=igeng))+
  geom_line()+geom_point()

#separate date
igif2sepdate = separate(iggroupinflu2,date,c("year","month","day"),sep = "-")
#group again
igif2sepdategrp = ddply(igif2sepdate, "month", summarise , 
                        igeng0 = mean(igeng),
                        iglike0 = mean(iglike),
                        igfan0 = mean(igfan))

#consider increasing of fan each day
ggplot(igif2sepdategrp, aes(x=month, y=igfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(igif2sepdategrp, aes(x=month, y=igeng0, group = 1))+
  geom_line()+geom_point()

#****influence 3****#

iginflu3 = instagramdf[instagramdf$account_username == "vanda29",]

iggroupinflu3 = ddply(iginflu3, "created_at", summarise , 
                      igeng = mean(engagement),
                      iglike = mean(like),
                      igfan=mean(fan))

iggroupinflu3$date = as.Date(iggroupinflu3$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(iggroupinflu3, aes(x=date, y=igfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(iggroupinflu3, aes(x=date, y=igeng))+
  geom_line()+geom_point()

#separate date
igif3sepdate = separate(iggroupinflu3,date,c("year","month","day"),sep = "-")
#group again
igif3sepdategrp = ddply(igif3sepdate, "month", summarise , 
                        igeng0 = mean(igeng),
                        iglike0 = mean(iglike),
                        igfan0 = mean(igfan))

#consider increasing of fan each day
ggplot(igif3sepdategrp, aes(x=month, y=igfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(igif3sepdategrp, aes(x=month, y=igeng0, group = 1))+
  geom_line()+geom_point()


#****influence 4****#

iginflu4 = instagramdf[instagramdf$account_username == "greenpeace",]

iggroupinflu4 = ddply(iginflu4, "created_at", summarise , 
                      igeng = mean(engagement),
                      iglike = mean(like),
                      igfan=mean(fan))

iggroupinflu4$date = as.Date(iggroupinflu4$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(iggroupinflu4, aes(x=date, y=igfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(iggroupinflu4, aes(x=date, y=igeng))+
  geom_line()+geom_point()

#separate date
igif4sepdate = separate(iggroupinflu4,date,c("year","month","day"),sep = "-")
#group again
igif4sepdategrp = ddply(igif4sepdate, "month", summarise , 
                        igeng0 = mean(igeng),
                        iglike0 = mean(iglike),
                        igfan0 = mean(igfan))

#consider increasing of fan each day
ggplot(igif4sepdategrp, aes(x=month, y=igfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(igif4sepdategrp, aes(x=month, y=igeng0, group = 1))+
  geom_line()+geom_point()


#****influence 5****#

iginflu5 = instagramdf[instagramdf$account_username == "thisisbebe",]

iggroupinflu5 = ddply(iginflu5, "created_at", summarise , 
                      igeng = mean(engagement),
                      iglike = mean(like),
                      igfan=mean(fan))

iggroupinflu5$date = as.Date(iggroupinflu5$created_at, '%Y-%m-%d')

#consider increasing in fan each post
ggplot(iggroupinflu5, aes(x=date, y=igfan))+
  geom_line()+geom_point()
#consider engagement in each post
ggplot(iggroupinflu5, aes(x=date, y=igeng))+
  geom_line()+geom_point()

#separate date
igif5sepdate = separate(iggroupinflu5,date,c("year","month","day"),sep = "-")
#group again
igif5sepdategrp = ddply(igif5sepdate, "month", summarise , 
                        igeng0 = mean(igeng),
                        iglike0 = mean(iglike),
                        igfan0 = mean(igfan))

#consider increasing of fan each day
ggplot(igif5sepdategrp, aes(x=month, y=igfan0, group = 1))+
  geom_line()+geom_point()
#consider increasing of engagement each day
ggplot(igif5sepdategrp, aes(x=month, y=igeng0, group = 1))+
  geom_line()+geom_point()



#######################
#---Graph Instagram---#
#######################


#-----------------#
#-------BAR-------#
#-----------------#

#bar by fan all observations
ggplot(iggrouporderfan, aes(x=igfan, y=reorder(account_username,igfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "All account order by fan")+
  xlab("Instagram Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,1000000,2000000,3000000,4000000),
                     labels = c("0","1M","2M",'3M','4M'))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#bar by fan 50 observations
#fan
ggplot(iggrouporderfan50, aes(x=igfan, y=reorder(account_username,igfan)))+
  geom_bar(stat="identity",fill="thistle1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Instagram Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,1000000,2000000,3000000,4000000),
                     labels = c("0","1M","2M",'3M','4M'))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#engagement
ggplot(iggrouporderfan50, aes(x=igeng, y=reorder(account_username,igfan)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Instagram Engagement")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,50000,100000,150000),
                     labels = c("0","50,000",'100,000','150,000'))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))

#Like
ggplot(iggrouporderfan50, aes(x=iglike, y=reorder(account_username,igfan)) )+
  geom_bar(stat="identity",fill="lightsalmon1")+
  theme_solarized_2(light = F)+
  labs(title = "Top 50 Influencers order by fan")+
  xlab("Instagram Like")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,50000,100000,150000),
                     labels = c("0","50,000",'100,000','150,000'))+
  theme(text=element_text(colour = "oldlace"),
        title = element_text(colour = "oldlace", size =10, face="bold"))


#bar by engagement all observations
ggplot(iggrouporderfan, aes(x=igeng, y=reorder(account_username,igeng)))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_solarized_2(light = T)+
  labs(title = "Influencers order by Engagement")+
  xlab("Instagram Engagement")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,50000,100000,150000),
                     labels = c("0","50,000",'100,000','150,000'))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))


#bar by engagement 50 observations
#fan
ggplot(iggrouporderfan50, aes(x=igfan, y=reorder(account_username,igeng)))+
  geom_bar(stat="identity",fill="violetred1")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by Engagement")+
  xlab("Instagram Fan")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,1000000,2000000,3000000,4000000),
                     labels = c("0","1M","2M",'3M','4M'))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#engagement
ggplot(iggrouporderfan50, aes(x=igeng, y=reorder(account_username,igeng)))+
  geom_bar(stat="identity",fill="skyblue2")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by Engagement")+
  xlab("Instagram Engagement")+ 
  ylab("Account Username")+
  scale_x_continuous(breaks = c(0,50000,100000,150000),
                     labels = c("0","50,000",'100,000','150,000'))+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))

#Like
ggplot(iggrouporderfan50, aes(x=iglike, y=reorder(account_username,igeng)) )+
  geom_bar(stat="identity",fill="sienna1")+
  theme_solarized_2(light = T)+
  labs(title = "Top 50 Influencers order by Engagement")+
  xlab("Instagram Like")+ 
  ylab("Account Username")+
  theme(text=element_text(colour = "rosybrown4"),
        title = element_text(colour = "rosybrown4", size =10, face="bold"))


#-----------------#
#interactive graph#
#-----------------#

#All Fan
ggplotly(
  
  ggplot(iggrouporderfan, aes(x=igfan, y=reorder(account_username,igfan),
                              text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                            "<b>Fan: </b>",igfan,"<br>"
                              )
  ))+
    geom_bar(stat="identity",fill="thistle1")+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by Fan")+
    xlab("Instagram Fan")+ 
    ylab("Account Username")+
    scale_x_continuous(breaks = c(0,1000000,2000000,3000000,4000000),
                       labels = c("0","1M","2M",'3M','4M'))+
    theme(text=element_text(colour = "oldlace"),
          title = element_text(colour = "oldlace", size =10, face="bold"),
    )
  , tooltip = "text"
)

#All engagement
ggplotly(
  
  ggplot(iggroupordereng, aes(x=igeng, y=reorder(account_username,igeng),
                              text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                            "<b>Engagement: </b>",igeng,"<br>"
                              )
  ))+
    geom_bar(stat="identity",fill="skyblue2")+
    theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
    labs(title = "Influencers order by Engagement")+
    xlab("Instagram Engagement")+ 
    ylab("Account Username")+
    scale_x_continuous(breaks = c(0,50000,100000,150000),
                       labels = c("0","50,000",'100,000','150,000'))+
    theme(text=element_text(colour = "rosybrown4"),
          title = element_text(colour = "rosybrown4", size =10, face="bold"),
    )
  , tooltip = "text"
)

#********************#
#***Change on Date***#
#********************#

igfilter = instagramdf[instagramdf$account_username %in% 
                         c("pimtha","kaykai_ntch",
                           "vanda29","greenpeace"
                           ,"thisisbebe"),]


igfilgrp = ddply(igfilter, c("account_username","created_at"), summarise , 
                 igeng = as.numeric(mean(engagement)),
                 iglike = as.numeric(mean(like)),
                 igfan= as.numeric(mean(fan))
)

igfilgrp$date = as.Date(igfilgrp$created_at, '%Y-%m-%d')
igfilsepdate = separate(igfilgrp,date,c("year","month","day"),sep = "-")

igfilsepdate$year = as.integer(igfilsepdate$year)
igfilsepdate$month = as.integer(igfilsepdate$month)
igfilsepdate$day = as.integer(igfilsepdate$day)


igfilsepdategrp = ddply(igfilsepdate, c("account_username","month"), summarise , 
                        igeng0 = mean(igeng),
                        iglike0 = mean(iglike),
                        igfan0 = mean(igfan))

glimpse(igfilsepdategrp)

#---------------------------#
#Interactive Graph with date#
#---------------------------#
#FAN
ggplotly(
  ggplot(igfilsepdategrp,aes(x=month,y=igfan0,group=account_username, 
                             color=account_username,
                             text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                           "<b>Fan: </b>",igfan0,"<br>",
                                           "<b>Month: </b>",month,"/2020 <br>")
  ))+
    geom_line()+
    geom_path(size = 0.8, lineend = "round")+
    geom_point(size = 1.5)+
    scale_x_continuous("Month of 2020",breaks = c(1:12))+
    scale_y_continuous("Fan")+
    theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
    labs(title = "Fan of Top 5 influencers on Instagram"),
  tooltip = "text"
)

#engagement

ggplotly(
  ggplot(igfilsepdategrp,aes(x=month,y=igeng0,group=account_username, 
                             color=account_username,
                             text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                           "<b>Engagement: </b>",igeng0,"<br>",
                                           "<b>Month: </b>",month,"/2020 <br>")
  ))+
    geom_line()+
    geom_path(size = 0.8, lineend = "round")+
    geom_point(size = 1.5)+
    scale_x_continuous("Month of 2020",breaks = c(1:30))+
    scale_y_continuous("Engagement")+
    theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
    labs(title = "Engagement of Top 5 influencers on Instagram")
  ,tooltip = 'text'
)

#-------------------#
#Animation with date#
#-------------------#

#animation by fan --> instagram has a weird data in fan
iganimfan = ggplot(igfilsepdategrp,aes(x=month,y=igfan0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Month of 2020",breaks = c(1:12))+
  scale_y_continuous("Fan")+
  theme_solarized_2(light = F,base_size = 14, base_family = "Georgia")+
  labs(title = "Fan of Top 5 influencers on Instagram")+
  transition_reveal(month)

animate(iganimfan, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("IGanimfan.gif")

#animation by engagement
iganimeng = ggplot(igfilsepdategrp,aes(x=month,y=igeng0,group=account_username, color=account_username))+
  geom_line()+
  geom_path(size = 0.8, lineend = "round")+
  geom_point(size = 1.5)+
  scale_x_continuous("Month of 2020",breaks = c(1:30))+
  scale_y_continuous("Engagement")+
  theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
  labs(title = "Engagement of Top 5 influencers on Instagram")+
  transition_reveal(month)

animate(iganimeng, height=600,width=1000,fps=30,duration=10,
        end_pause = 120,
        renderer = gifski_renderer())
anim_save("IGanimeng.gif")
















