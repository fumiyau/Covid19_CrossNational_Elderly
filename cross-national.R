#===============================================================================
# 2020/06/22
# Understanding cross national difference in COVID-19 deaths
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readxl)
######################################################################
# Change Working Directorys
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/18.Covid19_CrossNational") 

######################################################################
# Data create
######################################################################

usa <- read_excel("STMFinput/stmf.xlsx",sheet = "USA",skip=2) %>% 
  dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
                r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`)

uk <- read_excel("STMFinput/stmf.xlsx",sheet = "GBRTENW",skip=2) %>% 
  dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
                r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`) 

esp <- read_excel("STMFinput/stmf.xlsx",sheet = "ESP",skip=2) %>% 
  dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
                r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`) 

ita <- read_excel("STMFinput/stmf.xlsx",sheet = "ITA",skip=2) %>% 
  dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
                r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`) 

bel <- read_excel("STMFinput/stmf.xlsx",sheet = "BEL",skip=2) %>% 
  dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
                r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`) 

nld <- read_excel("STMFinput/stmf.xlsx",sheet = "NLD",skip=2) %>% 
  dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
                r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`) 
#swe <- read_excel("STMFinput/stmf.xlsx",sheet = "SWE",skip=2) %>% 
#  dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
#                r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`) 



df <- bind_rows(usa,uk,esp,ita,bel,swe,nld) %>% 
  filter(Year>2015 & Sex == "b") %>% 
  mutate(Year=as.character(Year),
         rt=rt*1000,
         r85=r85*1000,
         r75=r75*1000,
         r15=r15*1000,
         excess85=r85/r15,
         excess75=r75/r15)

dff <- bind_rows(usa,uk,esp) %>% 
  filter(Year>2015 & Sex == "f") %>% 
  mutate(Year=as.character(Year),
         rt=rt*1000,
         r85=r85*1000,
         r75=r75*1000,
         r15=r15*1000,
         excess85=r85/r15,
         excess75=r75/r15)
######################################################################
# Data viz
######################################################################

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_total <- ggplot(df, mapping = aes(x=Week,y=rt,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,30)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (all ages, both sexes)")
ggsave(plot_total,height=8,width=12,dpi=200, filename="2.Figures/Total.pdf",  family = "Helvetica")

plot_85 <- ggplot(df, mapping = aes(x=Week,y=r85,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,400)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (85+, both sexes)")
ggsave(plot_85,height=8,width=12,dpi=200, filename="2.Figures/Age85+.pdf",  family = "Helvetica")

plot_75 <- ggplot(df, mapping = aes(x=Week,y=r75,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,100)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (75-84, both sexes)")
ggsave(plot_75,height=8,width=12,dpi=200, filename="2.Figures/Age75-84.pdf",  family = "Helvetica")

plot_15 <- ggplot(df, mapping = aes(x=Week,y=r15,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,5)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (15-64, both sexes)")
ggsave(plot_15,height=8,width=12,dpi=200, filename="2.Figures/Age15-64.pdf",  family = "Helvetica")

plot_excess85 <- ggplot(df, mapping = aes(x=Week,y=excess85,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,150)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly excess mortality ratios in US, UK, Italy and Spain (85+ relative to 15-64, both sexes)")
ggsave(plot_excess85,height=8,width=12,dpi=200, filename="2.Figures/excess85.pdf",  family = "Helvetica")

plot_excess75 <- ggplot(df, mapping = aes(x=Week,y=excess75,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,50)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly excess mortality ratios in US, UK, Italy, and Spain (75-84 relative to 15-64, both sexes)")
ggsave(plot_excess75,height=8,width=12,dpi=200, filename="2.Figures/excess75.pdf",  family = "Helvetica")


