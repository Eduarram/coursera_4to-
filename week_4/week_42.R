####################################################################################################################
###############################  read the data   ###################################################################
####################################################################################################################

summarySCC_PM25 <- readRDS("C:/Users/bodega/Desktop/coursera_4to-/week_4/summarySCC_PM25.rds")
Source_Classification_Code <- readRDS("C:/Users/bodega/Desktop/coursera_4to-/week_4/Source_Classification_Code.rds")

####################################################################################################################
##############################  plot 1   ##########################################################################
####################################################################################################################

library(tidyverse)
tem <- summarySCC_PM25 %>% group_by(year) %>% 
  summarise(tc = sum(Emissions, na.rm = TRUE))

with(tem, plot(x= year, 
               y= tc,
               ylab = "total anual emmisions [Tons]",
               xlab = "Year",
               cex=3,
               pch=3,
               col="deepskyblue",
               lwd=4))

####################################################################################################################
################################ plot 2   ##########################################################################
####################################################################################################################

teb <- summarySCC_PM25 %>% subset(fips=="24510") %>% 
  group_by(year) %>% 
  summarise(tebalt = sum(Emissions, na.rm = TRUE))

with(teb, plot(x=year, 
               y=tebalt,
               ylab = "total anual emmisions",
               xlab = "Year",
               main = "total emissions by year in baltimore",
               cex=2,
               pch=2,
               col="gold2",
               lwd=3))

####################################################################################################################
###################################  plot 4   ######################################################################
####################################################################################################################

tet <- summarySCC_PM25 %>% subset(fips=="24510") %>% 
  group_by(year, type) %>% 
  summarise(tetype = sum(Emissions, na.rm = TRUE))

ggplot(tet, mapping = aes(x=year, y=tetype)) + 
  geom_point(color= "firebrick1", size=3.8, alpha=1/3) + 
  facet_grid(.~type) +
  xlab("year") +
  ylab("total Emissions [Tons]") +
  ggtitle("total emissions by year in baltimore") + 
  geom_line()
  theme_light()

####################################################################################################################
##################################################### plot 4 #######################################################
####################################################################################################################

SCC.coal <- Source_Classification_Code[grep("[Cc]oal", Source_Classification_Code$EI.Sector),]
NCS <- subset(summarySCC_PM25, summarySCC_PM25$SCC %in% SCC.coal$SCC)

mergcoal <- merge(x=NCS, y=Source_Classification_Code,
                  by.x = "SCC",
                  by.y = "SCC")

mergcoal <- mergcoal %>% 
  group_by(year) %>% 
  summarise(coal_emissions = sum(Emissions), na.rm = TRUE)
              ######## plot ##########

ggplot(mergcoal, mapping = aes(x=year, y=coal_emissions)) + geom_line(color="cadetblue1") +
  geom_point(color="blue4", size=4) + theme_minimal()+
  geom_smooth(method = "lm") + #this is a trend line 
  xlab("year")+
  ylab("total emissions [coal]") +
  ggtitle("total annual coal combustion emmisions in the us")

####################################################################################################################
################################# plot 5 ###########################################################################
####################################################################################################################
     ############# Baltimore ###############
vehiclecs <- Source_Classification_Code[grep("[Vv]eh", Source_Classification_Code$Short.Name),]
Emisionsv <- subset(summarySCC_PM25, summarySCC_PM25$SCC %in% vehiclecs$SCC)

Emisionsv <- Emisionsv %>% filter(fips=="24510")

Emisionsv2 <- Emisionsv %>%  
  merge(y=vehiclecs, by.x = "SCC", by.y = "SCC") %>% 
  group_by(year) %>% 
  summarise(vehicles_Et=sum(Emissions, na.rm = TRUE))


    ################## plot ###################
ggplot(Emisionsv2, mapping = aes(x = year, y = vehicles_Et)) + 
  geom_point(size=3) +
  geom_segment(aes(x=year, xend= year,
                   y=0, yend= vehicles_Et))+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("year") + ylab("total annual emission in baltimore city") +
  ggtitle("total anual emisions in baltimore city")

####################################################################################################################
###################################### plot 6 ######################################################################
####################################################################################################################

######################### california.La city ##########################################################################


vehiclecs <- Source_Classification_Code[grep("[Vv]eh", Source_Classification_Code$Short.Name),]
Emisionsv3 <- subset(summarySCC_PM25, summarySCC_PM25$SCC %in% vehiclecs$SCC)

Emisionsv3 <- Emisionsv3 %>% filter(fips=="06037")

Emisions_california <- Emisionsv3 %>%  
  merge(y=vehiclecs, by.x = "SCC", by.y = "SCC") %>% 
  group_by(year) %>% 
  summarise(vehicles_Et=sum(Emissions, na.rm = TRUE))

######################################### comparative ##########################################

emissions_baltimore <- cbind(Emisionsv2, "City" = rep("Baltimore", 4))
emissions_california.la <- cbind(Emisions_california, "City" = rep("LAngeles", 4))

comparative <- rbind(emissions_baltimore, emissions_california.la)

library(gcookbook)

ggplot(comparative, mapping = aes(x=year, y=vehicles_Et, color=City)) + 
  geom_line() + geom_point(aes(color=City),size=3) +
  facet_grid(.~ City)+
  ylab("Total emmisions") +
  ggtitle("Comparison of Total Annual Vehicle Emissions in Baltimore and Los Angeles")



  


  



