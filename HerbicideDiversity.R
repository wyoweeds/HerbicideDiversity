library(dplyr)
library(tidyr)
library(ggplot2)

read.csv("HerbicideUseData.csv")->alldat # Read in data file
alldat$Value<-as.numeric(as.character(alldat$Value)) # make sure data recognized as data
alldat<-spread(alldat, Data.Item, Value) # Separate lbs and rates into different columns
colnames(alldat)[8:9]<-c("Total","AppRate") # Name those columns
alldat$AcreTrts<-alldat$Total/alldat$AppRate # Calculate acre-treatments
head(alldat)

alldat2 <- alldat %>% # add up acre-treatments for each year*crop combination
    group_by(Year, Commodity) %>% 
    mutate(TotalCropAcreTrts = sum(AcreTrts, na.rm=TRUE)) 
alldat3 <- alldat2 %>% # add up acre-treatments by mode of action
    group_by(Year, Commodity, MOA) %>%
    summarize(MOA.AcreTrts = sum(AcreTrts, na.rm=TRUE),
              TotalCropAcreTrts = unique(TotalCropAcreTrts))
head(alldat3)
tail(alldat3)

herbdiv <- alldat3 %>% # calculate Shannon diversity index (H) and evenness (E)
    mutate(P = MOA.AcreTrts / TotalCropAcreTrts) %>%
    group_by(Year, Commodity) %>%
    summarize(H = -sum(P * log(P), na.rm=TRUE),
              E = H / log(length(unique(MOA))))
herbdiv              
tail(herbdiv)

bycrop<-split(herbdiv, herbdiv$Commodity)
bycrop$CORN

png("Fig3_Evenness.png", height=4.5, width=8, units="in", res=300, pointsize=15) 
par(mar=c(3.2,3.2,0.5,0.5), mgp=c(2,0.7,0))
plot(E ~ Year, data=bycrop$CORN, ylim=c(0,1), type="b", bty="n",
     xlim=c(1990, 2015), ylab="Evenness", lwd=2)
points(E ~ Year, data=bycrop$SOYBEANS, type="b", pch=2, col=2, lwd=2)
points(E ~ Year, data=bycrop[[3]], type="b", pch=3, col=3, lwd=2)
points(E ~ Year, data=bycrop[[4]], type="b", pch=4, col=4,  lwd=2)
legend("bottomright", c("Corn","Soybean","Sping wheat","Winter wheat"),
       pch=1:4, col=1:4, lty=1, bty="n")
dev.off()


png("Fig4_Diversity.png", height=4.5, width=8, units="in", res=300, pointsize=15) 
par(mar=c(3.2,3.2,0.5,0.5), mgp=c(2,0.7,0))
plot(H ~ Year, data=bycrop$CORN, ylim=c(0.5,2.5), type="b", bty="n",
     xlim=c(1990, 2015), ylab="Shannon diveristy index", lwd=2)
points(H ~ Year, data=bycrop$SOYBEANS, type="b", pch=2, col=2, lwd=2)
points(H ~ Year, data=bycrop[[3]], type="b", pch=3, col=3, lwd=2)
points(H ~ Year, data=bycrop[[4]], type="b", pch=4, col=4, lwd=2)
legend("bottomright", c("Corn","Soybean","Sping wheat","Winter wheat"),
       pch=1:4, col=1:4, lty=1, bty="n")
dev.off()

summary(lm(E ~ Year, data=bycrop$CORN))
summary(lm(E ~ Year, data=bycrop$SOYBEAN))
summary(lm(E ~ Year, data=bycrop[[3]]))
summary(lm(E ~ Year, data=bycrop[[4]]))

### Set color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#4d9f78", "#D8D8D8", "#746fb3", "#e9488a",
               "#67a61e", "#e6ab2f", "#a6761f", "#666666")
### Create acre-treatment percentage column
alldat3$AcreTrtPct <- alldat3$MOA.AcreTrts/alldat3$TotalCropAcreTrts

### CORN
data.frame(subset(alldat3, alldat3$Commodity=="CORN"))->corn.dat
corn.full <- corn.dat %>%
    complete(Year, MOA,
             fill=list(Commodity="CORN",MOA.AcreTrts=0,
                       TotalCropAcreTrts=NA, AcreTrtPct=0))
corn.acres <- read.csv("SurveyedCornAcres.csv") %>%
    group_by(Year) %>%
    summarize(Surveyed.Acres = sum(Value))
length(corn.full$Year) / length(unique(corn.full$Year))->corn.Nyrs
corn.full$Acres <- rep(corn.acres$Surveyed.Acres, each=corn.Nyrs)
corn.full$MOA.peracre <- corn.full$MOA.AcreTrts / corn.full$Acres
corn.full$MOAr <- factor(corn.full$MOA, levels=rev(unique(corn.full$MOA)))
plotcorn.1<-ggplot(corn.full, aes(x=Year, y=AcreTrtPct, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Proportion of total acre treatments") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "8 (Lipid)","9 (EPSPS)","10 (GlutSynth)","13 (DOXP)",
                               "14 (PPO)","15 (VLCFA)","19 (IAAtr)",
                               "22 (PSI)","27 (HPPD)")) +
        theme_bw() + theme(legend.position="right")
plotcorn.2<-ggplot(corn.full, aes(x=Year, y=MOA.peracre, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Herbicide treatments per acre") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "8 (Lipid)","9 (EPSPS)","10 (GlutSynth)","13 (DOXP)",
                               "14 (PPO)","15 (VLCFA)","19 (IAAtr)",
                               "22 (PSI)","27 (HPPD)")) +
        theme_bw() + theme(legend.position="right")
plotcorn.1
ggsave("Fig1_CornProportionMOA.png", width=8, height=4.5, units="in")
plotcorn.2
ggsave("Fig2_CornAcreTrtMOA.png", width=8, height=4.5, units="in")
    
### SOYBEAN
data.frame(subset(alldat3, alldat3$Commodity=="SOYBEANS"))->soybean.dat
soybean.full <- soybean.dat %>%
    complete(Year, MOA,
             fill=list(Commodity="SOYBEANS",MOA.AcreTrts=0,
                       TotalCropAcreTrts=NA, AcreTrtPct=0))
soybean.acres <- read.csv("SurveyedSoybeanAcres.csv") %>%
    group_by(Year) %>%
    summarize(Surveyed.Acres = sum(Value))
length(soybean.full$Year) / length(unique(soybean.full$Year))->soybean.Nyrs
soybean.full$Acres <- rep(soybean.acres$Surveyed.Acres, each=soybean.Nyrs)
soybean.full$MOA.peracre <- soybean.full$MOA.AcreTrts / soybean.full$Acres
soybean.full$MOAr <- factor(soybean.full$MOA, levels=rev(unique(soybean.full$MOA)))
plotsoybean.1<-ggplot(soybean.full, aes(x=Year, y=AcreTrtPct, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Proportion of total acre treatments") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "9 (EPSPS)","10 (GlutSynth)","13 (DOXP)",
                               "14 (PPO)","15 (VLCFA)",
                               "22 (PSI)")) +
        theme_bw() + theme(legend.position="right")
plotsoybean.2<-ggplot(soybean.full, aes(x=Year, y=MOA.peracre, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Herbicide treatments per acre") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "9 (EPSPS)","10 (GlutSynth)","13 (DOXP)",
                               "14 (PPO)","15 (VLCFA)",
                               "22 (PSI)")) +
        theme_bw() + theme(legend.position="right")
plotsoybean.1
ggsave("Fig1_SoybeanProportionMOA.png", width=8, height=4.5, units="in")
plotsoybean.2
ggsave("Fig2_SoybeanAcreTrtMOA.png", width=8, height=4.5, units="in")
    
### WINTER WHEAT
cbPalette <- c("#999999", "#E69F00", "#009E73", "#56B4E9", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#4d9f78", "#D8D8D8", "#746fb3", "#e9488a",
               "#67a61e", "#e6ab2f", "#a6761f", "#666666")
data.frame(subset(alldat3, alldat3$Commodity=="WHEAT, WINTER"))->wheatw.dat
wheatw.full <- wheatw.dat %>%
    complete(Year, MOA,
             fill=list(Commodity="WHEAT, WINTER",MOA.AcreTrts=0,
                       TotalCropAcreTrts=NA, AcreTrtPct=0))
wheatw.acres <- read.csv("SurveyedWinterWheatAcres.csv") %>%
    group_by(Year) %>%
    summarize(Surveyed.Acres = sum(Value))
length(wheatw.full$Year) / length(unique(wheatw.full$Year))->wheatw.Nyrs
wheatw.full$Acres <- rep(wheatw.acres$Surveyed.Acres, each=wheatw.Nyrs)
wheatw.full$MOA.peracre <- wheatw.full$MOA.AcreTrts / wheatw.full$Acres
wheatw.full$MOAr <- factor(wheatw.full$MOA, levels=rev(unique(wheatw.full$MOA)))
plotwheatw.1<-ggplot(wheatw.full, aes(x=Year, y=AcreTrtPct, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Proportion of total acre treatments") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "8 (Lipid)","9 (EPSPS)",
                               "14 (PPO)","15 (VLCFA)",
                               "22 (PSI)","26 (Lipid)","27 (HPPD)")) +
        theme_bw() + theme(legend.position="right")
plotwheatw.2<-ggplot(wheatw.full, aes(x=Year, y=MOA.peracre, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Herbicide treatments per acre") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "8 (Lipid)","9 (EPSPS)",
                               "14 (PPO)","15 (VLCFA)",
                               "22 (PSI)","26 (Lipid)","27 (HPPD)")) +
        theme_bw() + theme(legend.position="right")
plotwheatw.1
ggsave("Fig1_WheatwProportionMOA.png", width=8, height=4.5, units="in")
plotwheatw.2
ggsave("Fig2_WheatwAcreTrtMOA.png", width=8, height=4.5, units="in")
    
### SPRING WHEAT
cbPalette <- c("#999999", "#E69F00", "#009E73", "#56B4E9", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#4d9f78", "#D8D8D8", "#746fb3", "#e9488a",
               "#67a61e", "#e6ab2f", "#a6761f", "#666666")
data.frame(subset(alldat3, alldat3$Commodity=="WHEAT, SPRING"))->wheats.dat
wheats.full <- wheats.dat %>%
    complete(Year, MOA,
             fill=list(Commodity="WHEAT, WINTER",MOA.AcreTrts=0,
                       TotalCropAcreTrts=NA, AcreTrtPct=0))
wheats.acres <- read.csv("SurveyedSpringWheatAcres.csv") %>%
    group_by(Year) %>%
    summarize(Surveyed.Acres = sum(Value))
length(wheats.full$Year) / length(unique(wheats.full$Year))->wheats.Nyrs
wheats.full$Acres <- rep(wheats.acres$Surveyed.Acres, each=wheats.Nyrs)
wheats.full$MOA.peracre <- wheats.full$MOA.AcreTrts / wheats.full$Acres
wheats.full$MOAr <- factor(wheats.full$MOA, levels=rev(unique(wheats.full$MOA)))
plotwheats.1<-ggplot(wheats.full, aes(x=Year, y=AcreTrtPct, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Proportion of total acre treatments") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "8 (Lipid)","9 (EPSPS)",
                               "14 (PPO)","15 (VLCFA)",
                               "26 (Lipid)","27 (HPPD)")) +
        theme_bw() + theme(legend.position="right")
plotwheats.2<-ggplot(wheats.full, aes(x=Year, y=MOA.peracre, group=as.factor(MOA),
                     fill=as.factor(MOA), order=-as.numeric(MOA))) +
    geom_area(position='stack', na.rm=TRUE) +
    ylab("Herbicide treatments per acre") +
    scale_fill_manual(values=cbPalette,
                      name="WSSA Group",
                      guide=guide_legend(ncol=1, reverse=TRUE),
                      labels=c("1 (ACCase)","2 (ALS)","3 (Mitosis)",
                               "4 (Auxin)","5 (PSII)","6 (PSII)","7 (PSII)",
                               "8 (Lipid)","9 (EPSPS)",
                               "14 (PPO)","15 (VLCFA)",
                               "26 (Lipid)","27 (HPPD)")) +
        theme_bw() + theme(legend.position="right")
plotwheats.1
ggsave("Fig1_WheatsProportionMOA.png", width=8, height=4.5, units="in")
plotwheats.2
ggsave("Fig2_WheatsAcreTrtMOA.png", width=8, height=4.5, units="in")
    
