### Conversions for oyster landings data ###
# Goal - to get the volume of shell removed each year based off of the landings data
# to do this we must somehow convert the meat weight provided in the landings data to the volume of shell
# we can do this by
# meat weight --> # of bushels --> # of oysters --> meat weight per oyster --> pounds to grams --> dry weight --> height --> volume per oyster --> total volume

## Harvest Areas of interest and the counties used to represent them ##
#Suwannee = Dixie and Levy
#Apalachicola = Gulf and Franklin
#Pensacola = Santa Rosa and Escambia
#Choctawhatchee/St. Andrew = Ocaloosa and Walton

#setwd("E/Oyster_Landings")

library(dplyr)
library(ggplot2)
library(ggpubr)

## reading in the landings data and cleaning it up to include only the counties we are interested in ##

post_1984 <- read.csv("oyster_landings_by_county_84to22.csv", header = TRUE)

#post_1984 <- select(post_1984, Year, County_Landed, Pounds)

#Suwannee
Dixie <- filter(post_1984, County_Landed == "DIXIE")
Levy <- filter(post_1984, County_Landed == "LEVY")
Dixie <- select(Dixie, Year, Pounds, Trips, Average_Price, Estimated_Value)
Levy <- select(Levy, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Dixie) <- c("Year", "D.pounds","D.trips","D.avg_price","D.est.val") 
colnames(Levy) <- c("Year", "L.pounds","L.trips","L.avg_price","L.est.val") 
suw_counties <- full_join(Dixie, Levy, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Dixie1<-data.frame(Year=suw_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                   pounds=suw_counties$D.pounds, 
                   trips=suw_counties$D.trips, 
                   avg_price=suw_counties$D.avg_price, 
                   est_val=suw_counties$D.est.val)
Levy1<-data.frame(Year=suw_counties$Year, 
                  pounds=suw_counties$L.pounds, 
                  trips=suw_counties$L.trips, 
                  avg_price=suw_counties$L.avg_price, 
                  est_val=suw_counties$L.est.val)
Levy1[is.na(Levy1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Dixie1[is.na(Dixie1)] <- 0 # replace NAs with 0 so we can add columns in the next step
suw<-Dixie1+Levy1 # add Dixie and Levy data to consolidate into suwannee
suw$Year<-suw$Year/2 # fix the year

#Apalachicola
Gulf <- filter(post_1984, County_Landed == "GULF")
Franklin <- filter(post_1984, County_Landed == "FRANKLIN")
Gulf <- select(Gulf, Year, Pounds, Trips, Average_Price, Estimated_Value)
Franklin <- select(Franklin, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Gulf) <- c("Year", "G.pounds","G.trips","G.avg_price","G.est.val") 
colnames(Franklin) <- c("Year", "F.pounds","F.trips","F.avg_price","F.est.val") 
apa_counties <- full_join(Gulf, Franklin, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Gulf1<-data.frame(Year=apa_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                   pounds=apa_counties$G.pounds, 
                   trips=apa_counties$G.trips, 
                   avg_price=apa_counties$G.avg_price, 
                   est_val=apa_counties$G.est.val)
Franklin1<-data.frame(Year=apa_counties$Year, 
                  pounds=apa_counties$F.pounds, 
                  trips=apa_counties$F.trips, 
                  avg_price=apa_counties$F.avg_price, 
                  est_val=apa_counties$F.est.val)
Franklin1[is.na(Franklin1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Gulf1[is.na(Gulf1)] <- 0 # replace NAs with 0 so we can add columns in the next step
apa<-Gulf1+Franklin1 # add Gulf and Franklin data to consolidate into apalachicola
apa$Year<-apa$Year/2 # fix the year

#Pensacola
Santa_Rosa <- filter(post_1984, County_Landed == "SANTA ROSA")
Escambia <- filter(post_1984, County_Landed == "ESCAMBIA")
Santa_Rosa <- select(Santa_Rosa, Year, Pounds, Trips, Average_Price, Estimated_Value)
Escambia <- select(Escambia, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Santa_Rosa) <- c("Year", "SR.pounds","SR.trips","SR.avg_price","SR.est.val") 
colnames(Escambia) <- c("Year", "E.pounds","E.trips","E.avg_price","E.est.val") 
pens_counties <- full_join(Santa_Rosa, Escambia, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Santa_Rosa1<-data.frame(Year=pens_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                  pounds=pens_counties$SR.pounds, 
                  trips=pens_counties$SR.trips, 
                  avg_price=pens_counties$SR.avg_price, 
                  est_val=pens_counties$SR.est.val)
Escambia1<-data.frame(Year=pens_counties$Year, 
                      pounds=pens_counties$E.pounds, 
                      trips=pens_counties$E.trips, 
                      avg_price=pens_counties$E.avg_price, 
                      est_val=pens_counties$E.est.val)
Escambia1[is.na(Escambia1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Santa_Rosa1[is.na(Santa_Rosa1)] <- 0 # replace NAs with 0 so we can add columns in the next step
pens<-Santa_Rosa1+Escambia1 # add Santa_Rosa and Escambia data to consolidate into pensacola
pens$Year<-pens$Year/2 # fix the year

#Choctawhatchee/St. Andrew
Okaloosa <- filter(post_1984, County_Landed == "OKALOOSA")
Walton <- filter(post_1984, County_Landed == "WALTON")
Okaloosa <- select(Okaloosa, Year, Pounds, Trips, Average_Price, Estimated_Value)
Walton <- select(Walton, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Okaloosa) <- c("Year", "O.pounds","O.trips","O.avg_price","O.est.val") 
colnames(Walton) <- c("Year", "W.pounds","W.trips","W.avg_price","W.est.val") 
choc_counties <- full_join(Okaloosa, Walton, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Okaloosa1<-data.frame(Year=choc_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                  pounds=choc_counties$O.pounds, 
                  trips=choc_counties$O.trips, 
                  avg_price=choc_counties$O.avg_price, 
                  est_val=choc_counties$O.est.val)
Walton1<-data.frame(Year=choc_counties$Year, 
                      pounds=choc_counties$W.pounds, 
                      trips=choc_counties$W.trips, 
                      avg_price=choc_counties$W.avg_price, 
                      est_val=choc_counties$W.est.val)
Walton1[is.na(Walton1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Okaloosa1[is.na(Okaloosa1)] <- 0 # replace NAs with 0 so we can add columns in the next step
choc<-Okaloosa1+Walton1 # add Okaloosa and Walton data to consolidate into choctawhatchee
choc$Year<-choc$Year/2 # fix the year



suw <- select(suw, Year, pounds, trips, avg_price, est_val)
apa <- select(apa, Year, pounds, trips, avg_price, est_val)
pens <- select(pens, Year, pounds, trips, avg_price, est_val)
choc <- select(choc, Year, pounds, trips, avg_price, est_val)

suw$location<-"Suwannee Sound"
apa$location<-"Apalachicola Bay"
pens$location<-"Pensacola"
choc$location<-"Choctawhatchee Bay"

#colnames(suw) <- c("Year", "suw.pounds","suw.trips","suw.avg_price","suw.est.val") 
#colnames(apa) <- c("Year", "apa.pounds","apa.trips","apa.avg_price","apa.est.val") 
#colnames(pens) <- c("Year", "pens.pounds","pens.trips","pens.avg_price","pens.est.val") 
#colnames(choc) <- c("Year", "choc.pounds","choc.trips","choc.avg_price","choc.est.val") 


landings_84to22 <- full_join(suw, apa)
landings_84to22 <- full_join(landings_84to22, pens)
landings_84to22 <- full_join(landings_84to22, choc) # this is the cleaned landings data we will use

## Now we start the conversions

# converting the meat weight from the landings data to the number of bushels
landings_84to22$bushels<-landings_84to22$pounds/6.5625  #from FWC conversion factors


#converting the # of bushels to the # of oysters
landings_84to22$count<-landings_84to22$bushels*225  #from Berrigan paper
landings_84to22$field_count<-landings_84to22$bushels*291 #from my field collections

#converting from # of oysters to meat weight per oyster
landings_84to22$ind_wt<-landings_84to22$pounds/landings_84to22$count
landings_84to22$ind_wt_F<-landings_84to22$pounds/landings_84to22$field_count

#convert from pounds to grams
landings_84to22$grams<-landings_84to22$ind_wt*453.592

#for later use
land<-landings_84to22


#### using the minimum harvest size height (75mm)

#using the data from Hajovsky et al to be able to estimate the volume of a 75mm oyster
a<-read.csv("oyster data 07_20_20 ph.csv", header=TRUE)
m<-lm(log(a$Volume..mL.)~a$Height..mm.)
plot(log(a$Volume..mL.),a$Height..mm.)

#the code above uses all of the Hajovsky et al data, which is what I used to estimate the volume

fl<-filter(a, State.TX.divided=="FL")
m1<-lm(log(fl$Volume..mL.)~fl$Height..mm.)
plot(log(fl$Volume..mL.),fl$Height..mm.)
# the code above uses just the FL data from Hajovsky et al, this is not what I used, but 
#it can be easily changed to use this instead if preferred


#converting from height to volume (y=mx+b) y=volume, m=slope of lm, x=harvest size(75), b=y-intercept(volume when height is 0)
#m=0.034867
#b=0.928140
#m and b are from linear model (named m above)
land$logvol<-76.2*0.034867+0.928140 #from Hajovsky et al
land$vol<-exp(land$logvol)

land$logvol_F<-76.2*0.02298+1.73096 #from Hajovsky et al
land$vol_F<-exp(land$logvol_F)

#convert from individual oyster volume to total volume per county per year
land$tot_vol<-land$vol*land$count
land$tot_vol_F<-land$vol*land$field_count  #using the number of oys per bushel from my field collections

land$tot_vol_F1<-land$vol_F*land$count 

#convert from mL to meters cubed
land$Vol_m3<-land$tot_vol*0.000001
land$Vol_m3_F<-land$tot_vol_F*0.000001

land$Vol_m3_F1<-land$tot_vol_F1*0.000001

land$vol_wd_field<-0.0000479*land$field_count #using the water displacement volume from my field collections (only market sized oysters)
land$vol_rough_field<-0.000138*land$field_count #using the rough volume (LxWxH) from my field collections (market sized only)
land$vol_crate_field<-0.00246*land$field_count #using the volume per oyster in the crate (market sized only)

#calculating the number of lone cabbages
LC<-16000/1.308 #yards to meters
land[is.na(land)] = 0
land$num_LC<-land$Vol_m3/LC
land$num_LC_F<-land$Vol_m3_F/LC
land$num_LC_wd_field<-land$vol_wd_field/LC
land$num_LC_rough_field<-land$vol_rough_field/LC
land$num_LC_crate_field<-land$vol_crate_field/LC

# Calculate CPUE
land$CPUE<-land$Vol_m3/land$trips
land$CPUE_F<-land$Vol_m3_F/land$trips

#plots

#### Plots for Suwannee Sound ####
#number of cubic meters removed from suwannee sound
(suw_m3_plot<-ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = Vol_m3)) +
  
  geom_point(aes(colour ="Berrigan"), size=6) +
  geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=Vol_m3_F,colour="Field"), size=6)+
  theme_classic()+
  scale_x_continuous(limits=c(1980,2022),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1800),breaks=c(0,200,400,600, 800, 1000, 1200, 1400,1600,1800))+
  scale_color_manual(name="",breaks=c('Berrigan','Field'),values=c('Berrigan'='lightslateblue', 'Field'='tomato'))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed"))

#the plot below compares the hajovsky and field calculations for volume
(suw_m3_plot1<-ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = Vol_m3)) +
    
    geom_point(aes(colour ="Hajovsky et al."), size=6) +
    geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=Vol_m3_F1,colour="Field"), size=6)+
    theme_classic()+
    scale_x_continuous(limits=c(1980,2022),breaks=c(1980, 1985, 1990,
                                                    1995,2000, 2005, 2010,2015,2020, 2025)) +
    scale_y_continuous(limits=c(0,1800),breaks=c(0,200,400,600, 800, 1000, 1200, 1400,1600,1800))+
    scale_color_manual(name="",breaks=c('Hajovsky et al.','Field'),values=c('Hajovsky et al.'='tomato', 'Field'='lightslateblue'))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                  face="bold"),
          plot.title =element_text(size=16, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Shell Material Removed",
         y = "Cubic Meters Removed"))


#the plot below compares the different volume methods from field calculations
(suw_m3_plot_vol_comp<-ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = Vol_m3_F)) +
    
    geom_point(aes(colour ="Hajovsky et al."), size=6) +
    geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=vol_crate_field,colour="Crate Field"), size=6)+
    geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=vol_rough_field,colour="Rough Field"), size=6)+
    geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=vol_wd_field,colour="Water Disp. Field"), size=6)+
    theme_classic()+
    scale_x_continuous(limits=c(1980,2022),breaks=c(1980, 1985, 1990,
                                                    1995,2000, 2005, 2010,2015,2020, 2025)) +
    scale_y_continuous(limits=c(0,120000),breaks=c(0,10000, 20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000))+
    scale_color_manual(name="",breaks=c('Hajovsky et al.','Crate Field','Rough Field','Water Disp. Field'),values=c('Hajovsky et al.'='tomato', 'Crate Field'='lightslateblue', 'Rough Field'='lightblue', 'Water Disp. Field'='pink'))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                  face="bold"),
          plot.title =element_text(size=16, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Comparison of Methods",
         y = "Cubic Meters Removed"))


#the plot below compares the different volume methods from field calculations excluding the crate
(suw_m3_plot_vol_comp<-ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = Vol_m3_F)) +
    
    geom_point(aes(colour ="Hajovsky et al."), size=6) +
    #geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=vol_crate_field,colour="Crate Field"), size=6)+
    geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=vol_rough_field,colour="Rough Field"), size=6)+
    geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=vol_wd_field,colour="Water Disp. Field"), size=6)+
    theme_classic()+
    scale_x_continuous(limits=c(1980,2022),breaks=c(1980, 1985, 1990,
                                                    1995,2000, 2005, 2010,2015,2020, 2025)) +
    scale_y_continuous(limits=c(0,7000),breaks=c(0,1000, 2000,3000,4000,5000,6000,7000,8000,9000,10000))+
    scale_color_manual(name="",breaks=c('Hajovsky et al.','Rough Field','Water Disp. Field'),values=c('Hajovsky et al.'='tomato', 'Rough Field'='lightblue', 'Water Disp. Field'='pink'))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                  face="bold"),
          plot.title =element_text(size=16, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Comparison of Methods",
         y = "Cubic Meters Removed"))




#number of lone cabbages from Suwannee Sound
ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = num_LC)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0.25, 0.5, 0.75, 1))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed from Suwannee Sound",
       y = "# Lone Cabbage Reefs Removed")



#number of lone cabbages from Suwannee Sound using different volume methods
ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = num_LC_F)) +
  
  geom_point(aes(colour ="Hajovsky et al."), size=6) +
  geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=num_LC_crate_field,colour="Crate Field"), size=6)+
  geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=num_LC_rough_field,colour="Rough Field"), size=6)+
  geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=num_LC_wd_field,colour="Water Disp. Field"), size=6)+
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,10),breaks=c(0, 0.5, 1,2,3,4,5,6,7,8,9,10))+
  scale_color_manual(name="",breaks=c('Hajovsky et al.',"Crate Field",'Rough Field','Water Disp. Field'),values=c('Hajovsky et al.'='tomato',"Crate Field"='lightslateblue', 'Rough Field'='lightblue', 'Water Disp. Field'='pink'))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed from Suwannee Sound",
       y = "# Lone Cabbage Reefs Removed")


#number of lone cabbages from Suwannee Sound using different volume methods excluding the crate volume
ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = num_LC_F)) +
  
  geom_point(aes(colour ="Hajovsky et al."), size=6) +
  #geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=num_LC_crate_field,colour="Crate Field"), size=6)+
  geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=num_LC_rough_field,colour="Rough Field"), size=6)+
  geom_point(data = subset(land, location %in% c("Suwannee Sound")), aes(y=num_LC_wd_field,colour="Water Disp. Field"), size=6)+
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.25, 0.5, 0.75, 1))+
  scale_color_manual(name="",breaks=c('Hajovsky et al.','Rough Field','Water Disp. Field'),values=c('Hajovsky et al.'='tomato','Rough Field'='lightblue', 'Water Disp. Field'='pink'))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed from Suwannee Sound",
       y = "# Lone Cabbage Reefs Removed")


#number of trips in suwannee sound
(suw_trips_plot<-ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = trips)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,12000),breaks=c(0,2000,4000,6000, 8000, 10000, 12000))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Trips Taken",
       y = "# of Trips"))


# CPUE for Suwannee Sound
(suw_CPUE_plot<-ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x=Year, y=CPUE))+
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3,0.4,0.5))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Catch Per Unit Effort",
       y = "CPUE"))

suw_fig<-ggarrange(suw_m3_plot, suw_trips_plot, suw_CPUE_plot, 
                    labels = c("A", "B", "C"),
                    ncol = 3, nrow = 1)
annotate_figure(suw_fig,
                top = text_grob("Suwannee Sound", color = "black", face = "bold", size = 20))

# landings from Suwannee Sound
(suw_land_plot<-ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x = Year, y = pounds)) +
    
    geom_point(colour ="tomato", size=6) +
    theme_classic()+
    scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                    1995,2000, 2005, 2010,2015,2020, 2025)) +
    scale_y_continuous(limits=c(0,1200000),breaks=c(0,200000,400000,600000, 800000, 1000000, 1200000))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                  face="bold"),
          plot.title =element_text(size=16, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Landed Meat",
         y = "Pounds of Meat"))





#### Apalachicola ####
#volume in meters cubed of shell material removed from apalachicola
(apa_m3_plot<-ggplot(data = subset(land, location %in% c("Apalachicola Bay")), aes(x = Year, y = Vol_m3_F)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,5600),breaks=c(0,800,1600,2400,3200,4000,4800,5600))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed"))
##############################
#bring in steve geiger cultch info

library(readxl)
cultch <- read_excel("Copy of Shellplants.xlsx", 
                   sheet = "Sheet2")
names(cultch)

names(cultch)[1] <- "Year"
names(cultch)[7] <- "CubicYards"

c2 <- cultch %>% 
  dplyr::select(Year, CubicYards, Location)%>%
  mutate(Location = replace(Location,Location == "Apalachicola", "Apalachicola Bay"))

sum_cultch<- c2 %>%
  dplyr::group_by(Year, Location) %>%
  dplyr::summarise(Sum_yds=sum(CubicYards, na.rm=TRUE))
  
c3<-sum_cultch

c3$Sum_meters<-c3$Sum_yds * 0.765

#now edit Jamie's landings dataframe

names(land)

l1 <- land %>% 
  dplyr::select(Year, location, vol_crate_field)

names(l1)[2] <- "Location"
names(l1)[3] <- "Shell_vol"

names(c3)
names(l1)

c4=merge(c3,l1,by=c("Year", "Location"))


c4$removed<-c4$Shell_vol*-1



head(c4)


r1<-ggplot(c4, aes(Year, Sum_meters)) +
  geom_point(size=3, color="green") +
  geom_point(aes(Year,removed), size=3, shape=1)+
  scale_y_continuous(limits=c(-350000,75000),breaks=seq(-350000,75000,25000))+
  ggtitle("Cultch and removals") +
  xlim(1986,2022)+
  xlab("Date") +
  ylab("Cubic meters")

##show this in the meeting

colors<-c('cultch added'= 'green', 'cultch removed'='black')

r2<-ggplot(c4, aes(Year, Sum_meters) ) +
  geom_point(size=3, color="green",  shape = 16)+
  geom_point(aes(Year,Shell_vol),color="black", size=3, shape=16)+
  scale_y_continuous(limits=c(0,350000),breaks=seq(0,350000,25000))+
  ggtitle("Cultch and removals") +
  xlim(1986,2022)+
  labs(x = "Date",
       y= "Cubic meters",
       color="Legend") +
  scale_color_manual(values = colors)

##############################


#the plot below compares the different volume methods from field calculations 
apa_m3_plot_vol_comp<-ggplot(data = subset(land, location %in% c("Apalachicola Bay")), aes(x = Year, y = vol_crate_field)) +
    geom_point(aes(colour="Crate Field"), size=6) +
    geom_point(data = subset(land, location %in% c("Apalachicola Bay")), aes(y=vol_rough_field,colour="Rough Field"), size=6)+
    #geom_point(data = subset(land, location %in% c("Apalachicola Bay")), aes(y=vol_wd_field,colour="Water Disp. Field"), size=6)+
    theme_classic()+
    scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                    1995,2000, 2005, 2010,2015,2020, 2025)) +
    scale_y_continuous(limits=c(0,350000),breaks=c(0,50000,100000,150000, 200000,250000,300000,350000))+
    scale_color_manual(name="",breaks=c('Crate Field','Rough Field'),values=c('Crate Field'='lightslateblue', 'Rough Field'='tomato'))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                  face="bold"),
          plot.title =element_text(size=16, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Comparison of Methods",
         y = "Cubic Meters Removed")

###############################


# estimates of the number of lone cabbage reefs using different volume methods
(apa_LC_comp<-ggplot(data = subset(land, location %in% c("Apalachicola Bay")), aes(x = Year, y = num_LC_crate_field)) +
    
    geom_point(aes(colour ="Crate Field"), size=6) +
    geom_point(data = subset(land, location %in% c("Apalachicola Bay")), aes(y=num_LC_rough_field,colour="Rough Field"), size=6)+
    theme_classic()+
    scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                    1995,2000, 2005, 2010,2015,2020, 2025)) +
    scale_y_continuous(limits=c(0,30),breaks=c(0,1,2,5,10,15,20,25,30))+
    scale_color_manual(name="",breaks=c('Crate Field','Rough Field'),values=c('Crate Field'='lightslateblue', 'Rough Field'='tomato'))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                  face="bold"),
          plot.title =element_text(size=16, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Comparison of Methods",
         y = "# Lone Cabbage Reefs Removed"))



#number of lone cabbages from apalachicola
ggplot(data = subset(land, location %in% c("Apalachicola Bay")), aes(x = Year, y = num_LC)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.25, 0.5,0.75,1))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "# Lone Cabbage Reefs Removed")




#number of trips in Apalachicola
(apa_trips_plot<-ggplot(data = subset(land, location %in% c("Apalachicola Bay")), aes(x = Year, y = trips)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,60000),breaks=c(0,10000,20000,30000,40000,50000,60000))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Trips Taken",
       y = "# of Trips"))


# CPUE for Apalachicola
(apa_CPUE_plot<-ggplot(data = subset(land, location %in% c("Apalachicola Bay")), aes(x=Year, y=CPUE))+
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,0.2),breaks=c(0,0.1,0.2,0.3,0.4,0.5))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Catch Per Unit Effort",
       y = "CPUE"))

apa_fig<-ggarrange(apa_m3_plot, apa_trips_plot, apa_CPUE_plot, 
                    labels = c("A", "B", "C"),
                    ncol = 3, nrow = 1)
annotate_figure(apa_fig,
                top = text_grob("Apalachicola", color = "black", face = "bold", size = 20))

# landings from Apalachicola
(apa_land_plot<-ggplot(data = subset(land, location %in% c("Apalachicola Bay")), aes(x = Year, y = pounds)) +
    
    geom_point(colour ="tomato", size=6) +
    theme_classic()+
    scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                    1995,2000, 2005, 2010,2015,2020, 2025)) +
    scale_y_continuous(limits=c(0,3500000),breaks=c(0,500000,1500000,2500000,3500000 ))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                  face="bold"),
          plot.title =element_text(size=16, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Landed Meat",
         y = "Pounds of Meat"))




#### Pensacola ####
#volume in meters cubed of shell material removed from Pensacola
(pens_m3_plot<-ggplot(data = subset(land, location %in% c("Pensacola")), aes(x = Year, y = Vol_m3)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,600),breaks=c(0,100,200,300,400,500,600))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed"))


#number of lone cabbages from Pensacola
ggplot(data = subset(land, location %in% c("Pensacola")), aes(x = Year, y = num_LC)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.25, 0.5,0.75,1))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "# Lone Cabbage Reefs Removed")




#number of trips in Pensacola
(pens_trips_plot<-ggplot(data = subset(land, location %in% c("Pensacola")), aes(x = Year, y = trips)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1600),breaks=c(0,200,400,600,800,1000,1200,1400,1600))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Trips Taken",
       y = "# of Trips"))


# CPUE for Pensacola
(pens_CPUE_plot<-ggplot(data = subset(land, location %in% c("Pensacola")), aes(x=Year, y=CPUE))+
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,0.7),breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Catch Per Unit Effort",
       y = "CPUE"))

pens_fig<-ggarrange(pens_m3_plot, pens_trips_plot, pens_CPUE_plot, 
                    labels = c("A", "B", "C"),
                    ncol = 3, nrow = 1)
annotate_figure(pens_fig,
                top = text_grob("Pensacola", color = "black", face = "bold", size = 20))




#### Choctawhatchee ####
#volume in meters cubed of shell material removed from Choctawhatchee
(choc_m3_plot<-ggplot(data = subset(land, location %in% c("Choctawhatchee Bay")), aes(x = Year, y = Vol_m3)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,14),breaks=c(0,2,4,6,8,10,12,14))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed"))


#number of lone cabbages from Choctawhatchee
ggplot(data = subset(land, location %in% c("Choctawhatchee Bay")), aes(x = Year, y = num_LC)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.25, 0.5,0.75,1))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "# Lone Cabbage Reefs Removed")




#number of trips in Choctawhatchee
(choc_trips_plot<-ggplot(data = subset(land, location %in% c("Choctawhatchee Bay")), aes(x = Year, y = trips)) +
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,150),breaks=c(0,30,60,90,120,150))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Trips Taken",
       y = "# of Trips"))


# CPUE for Choctawhatchee
(choc_CPUE_plot<-ggplot(data = subset(land, location %in% c("Choctawhatchee Bay")), aes(x=Year, y=CPUE))+
  
  geom_point(colour ="tomato", size=6) +
  theme_classic()+
  scale_x_continuous(limits=c(1980,2025),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,0.25),breaks=c(0,0.05,0.1,0.15,0.2,0.25))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Catch Per Unit Effort",
       y = "CPUE"))

choc_fig<-ggarrange(choc_m3_plot, choc_trips_plot, choc_CPUE_plot, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)
annotate_figure(choc_fig,
                top = text_grob("Choctawhatchee", color = "black", face = "bold", size = 20))






#######
# Number of Lone cabbage reefs removed in Suwannee, Apalachicola, Pensacola, Choctawhatchee


ggplot(data = land, aes(x = Year, y = num_LC, color=location)) +
  
  geom_point(size=6) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442"))+
  theme_classic()+
  scale_x_continuous(limits=c(1980,2022),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.25, 0.5,0.75,1))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "# Lone Cabbage Reefs Removed")








ggplot(data = subset(land, location %in% c("Suwannee Sound")), aes(x=Vol_m3, y=CPUE))+
  
  geom_point(colour ="tomato", size=6, alpha=0.3) +
  theme_classic()+
  scale_x_continuous(limits=c(0, 1300)) +
  scale_y_continuous(limits=c(0,0.4),breaks=c(0,0.1,0.2,0.3,0.4))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "CPUE vs Shell Volume Removed", x= "Shell Volume Removed",
       y = "CPUE")

suw_fig<-ggarrange(suw_m3_plot, suw_trips_plot, suw_CPUE_plot, 
                   labels = c("A", "B", "C"),
                   ncol = 3, nrow = 1)
annotate_figure(suw_fig,
                top = text_grob("Suwannee Sound", color = "black", face = "bold", size = 20))

suw_fig1<-ggarrange(suw_land_plot, suw_trips_plot, suw_CPUE_plot, 
                   labels = c("A", "B", "C"),
                   ncol = 3, nrow = 1)
annotate_figure(suw_fig1,
                top = text_grob("Suwannee Sound", color = "black", face = "bold", size = 20))


apa_fig1<-ggarrange(apa_land_plot, apa_trips_plot, apa_CPUE_plot, 
                    labels = c("A", "B", "C"),
                    ncol = 3, nrow = 1)
annotate_figure(apa_fig1,
                top = text_grob("Apalachicola Bay", color = "black", face = "bold", size = 20))









ggplot(data = (subset(land, location %in% c("Suwannee Sound", "Apalachicola Bay"))), aes(x = Year, y = num_LC_F, color=location)) +
  
  geom_point(size=6) +
  scale_color_manual(name="Location",values=c("lightslateblue","tomato"))+
  theme_classic()+
  scale_x_continuous(limits=c(1980,2022),breaks=c(1980, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.25, 0.5,0.75,1))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,
                                                                face="bold"),
        plot.title =element_text(size=16, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "# Lone Cabbage Reefs Removed")







## summary stats calculations
suw_dat<-subset(land, location %in% c("Suwannee Sound"))
head(suw_dat)
sum(suw_dat$Vol_m3_F)
sum(suw_dat$Vol_m3)
max(suw_dat$Vol_m3_F)
min(suw_dat$Vol_m3_F)
sum(suw_dat$num_LC_F)

apa_dat<-subset(land, location %in% c("Apalachicola Bay"))
head(apa_dat)
sum(apa_dat$Vol_m3_F)
sum(apa_dat$Vol_m3)
max(apa_dat$Vol_m3_F)
min(apa_dat$Vol_m3_F)
sum(apa_dat$num_LC_F)

mean(apa_dat$vol_crate_field)
mean(apa_dat$vol_rough_field)
sum(apa_dat$vol_crate_field)
sum(apa_dat$vol_rough_field)
max(apa_dat$vol_crate_field)
max(apa_dat$vol_rough_field)

mean(apa_dat$num_LC_crate_field)
mean(apa_dat$num_LC_rough_field)
sum(apa_dat$num_LC_crate_field)
sum(apa_dat$num_LC_rough_field)
max(apa_dat$num_LC_crate_field)
max(apa_dat$num_LC_rough_field)
