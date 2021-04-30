#info test
ui = fluidPage(theme = shinytheme('slate'),
               navbarPage('Social Media',
                          tabPanel('Facebook',
                                   titlePanel('Facebook Influencers'),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Fan and Engagement", 
                                                plotlyOutput("fbFan"),
                                                plotlyOutput("fbEng")),
                                       tabPanel("Top5 Influencers and Date", 
                                                plotlyOutput("fbFanDate"),
                                                plotlyOutput("fbEngDate"))
                                     )
                                   )
                                   ),
                          tabPanel('Instagram',
                                   titlePanel('Instagram Influencers'),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Fan and Engagement", 
                                                plotlyOutput("igFan"),
                                                plotlyOutput("igEng")),
                                       tabPanel("Top5 Influencers and Date", 
                                                plotlyOutput("igFanDate"),
                                                plotlyOutput("igEngDate"))
                                     )
                                   )
                          ),
                          tabPanel('Twitter',
                                   titlePanel('Twitter Influencers'),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Fan and Engagement", 
                                                plotlyOutput("ttFan"),
                                                plotlyOutput("ttEng")),
                                       tabPanel("Top5 Influencers and Date", 
                                                plotlyOutput("ttFanDate"),
                                                plotlyOutput("ttEngDate"))
                                     )
                                   )
                          ),
                          tabPanel('Youtube',
                                   titlePanel('Youtube Influencers'),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Fan and Engagement", 
                                                plotlyOutput("ytFan"),
                                                plotlyOutput("ytEng")),
                                       tabPanel("Top5 Influencers and Date", 
                                                plotlyOutput("ytFanDate"),
                                                plotlyOutput("ytEngDate"))
                                     )
                                   )
                          )
                          
                          )
               )

server = function(input,output,session){
  
  ################
  #---Facebook---#
  ################
  
  facebook1df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q1.csv")
  facebook2df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q2.csv")
  facebook3df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q3.csv")
  facebook4df = read.csv("C:/Users/ACER/Downloads/Facebook_Data_Q4.csv")
  
  
  facebookdf = rbind(facebook1df,facebook2df,facebook3df,facebook4df)
  
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
  fbremove = fbrealsep[fbrealsep$account_user != ""&fbrealsep$account_user !='"',]
  
  #convert fbfan to numeric
  fbremove$fbfan = as.numeric(as.character(fbremove$fbfan))
  
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
  
  
  output$fbFan = renderPlotly(ggplotly(
    
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
  )
  output$fbEng = renderPlotly(ggplotly(
    
    ggplot(fbgrouporderengage, aes(x=fbengage, y=reorder(account_user,fbfan),
                                   text = paste0("<b>Account Username: </b>",account_user,"<br>",
                                                 "<b>Engagement: </b>",fbengage,"<br>"
                                   )
    ))+
      geom_bar(stat="identity",fill="skyblue2")+
      theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
      labs(title = "Influencers order by Fan")+
      xlab("Facebook Engagement")+ 
      ylab("Account Username")+
      theme(text=element_text(colour = "rosybrown4"),
            title = element_text(colour = "rosybrown4", size =10, face="bold"),
      )
    , tooltip = "text"
  ))
  output$fbFanDate = renderPlotly(ggplotly(
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
  )
  output$fbEngDate = renderPlotly(ggplotly(
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
  ))
  
  
  #################
  #---Instagram---#
  #################
  
  instagramdf = read.csv("C:/Users/ACER/Downloads/Instagram_Data.csv")
  
  iggroup = ddply(instagramdf, c("account_username"),summarise, 
                  igeng = mean(engagement),
                  iglike = mean(like),
                  igfan=max(fan))
  
  ###########
  #--order--#
  ###########
  #sort by fan
  iggrouporderfan = iggroup[order(-iggroup$igfan),]
  iggrouporderfan50 =iggrouporderfan[1:50,]
  
  #sort by engagement
  iggroupordereng = iggroup[order(-iggroup$igeng),]
  iggroupordereng50 =iggroupordereng[1:50,]
  
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
  
  
  output$igFan = renderPlotly(ggplotly(
    
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
  ))
  output$igEng = renderPlotly(ggplotly(
    
    ggplot(iggroupordereng, aes(x=igeng, y=reorder(account_username,igeng),
                                text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                              "<b>Engagement: </b>",igeng,"<br>"
                                )
    ))+
      geom_bar(stat="identity",fill="skyblue2")+
      theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
      labs(title = "Influencers order by Fan")+
      xlab("Instagram Engagement")+ 
      ylab("Account Username")+
      scale_x_continuous(breaks = c(0,50000,100000,150000),
                         labels = c("0","50,000",'100,000','150,000'))+
      theme(text=element_text(colour = "rosybrown4"),
            title = element_text(colour = "rosybrown4", size =10, face="bold"),
      )
    , tooltip = "text"
  ))
  output$igFanDate = renderPlotly(ggplotly(
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
  ))
  output$igEngDate = renderPlotly(
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
    ))
  
  
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
  
  output$ttFan = renderPlotly(
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
  )
  
  output$ttEng = renderPlotly(
    ggplotly(
      
      ggplot(ttgroupordereng, aes(x=tteng, y=reorder(account_username,ttfan),
                                  text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                                "<b>Engagement: </b>",tteng,"<br>"
                                  )
      ))+
        geom_bar(stat="identity",fill="skyblue2")+
        theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
        labs(title = "Influencers order by Fan")+
        xlab("Twitter Engagement")+ 
        ylab("Account Username")+
        scale_x_continuous(breaks = c(0,5000,10000),
                           labels = c("0","5,000","10,000"))+
        theme(text=element_text(colour = "rosybrown4"),
              title = element_text(colour = "rosybrown4", size =10, face="bold"),
        )
      , tooltip = "text"
    )
  )
  
  output$ttFanDate = renderPlotly(
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
  )
  output$ttEngDate = renderPlotly(
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
    
  )
  
  ###############
  #---Youtube---#
  ###############
  
  youtubedf = read.csv("C:/Users/ACER/Downloads/YouTube_Data.csv")
  
  
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
  
  ytremove = ytrealsep[ytrealsep$account_user != ""&ytrealsep$account_user !='"',]
  
  ytremove = ytremove[names(ytremove) != 'useless']
  
  
  
  #############
  #---order---#
  #############
  
  #sort by fan
  ytgroup2orderfan = ytremove[order(-ytremove$ytfan),]
  ytgroup2orderfan50 =ytgroup2orderfan[1:50,]
  
  #sort by view
  ytgroup2orderview = ytremove[order(-ytremove$ytfan),]
  ytgroup2orderview50 =ytgroup2orderview[1:50,]
  
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
  
  output$ytFan = renderPlotly(ggplotly(
    
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
  ))
  output$ytEng = renderPlotly(ggplotly(
    
    ggplot(ytgroup2orderview, aes(x=ytview, y=reorder(account_username,ytview),
                                  text = paste0("<b>Account Username: </b>",account_username,"<br>",
                                                "<b>Engagement: </b>",ytview,"<br>"
                                  )
    ))+
      geom_bar(stat="identity",fill="skyblue2")+
      theme_solarized_2(light = T,base_size = 14, base_family = "Georgia")+
      labs(title = "Influencers order by Fan")+
      xlab("Youtube View")+ 
      ylab("Account Username")+
      scale_x_continuous(breaks = c(0,2000000,4000000),
                         labels = c("0","2M","4M"))+
      theme(text=element_text(colour = "rosybrown4"),
            title = element_text(colour = "rosybrown4", size =10, face="bold"),
      )
    , tooltip = "text"
  ))
  output$ytFanDate = renderPlotly(ggplotly(
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
  )
  output$ytEngDate = renderPlotly(ggplotly(ggplot(ytfilsepdategrp,aes(x=month,y=ytview0,group=account_username, 
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
  ))
  
}

shinyApp(ui,server)







