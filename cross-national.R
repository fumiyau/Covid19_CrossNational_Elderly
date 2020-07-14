#===============================================================================
# 2020/06/22
# Understanding cross national difference in COVID-19 deaths
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readxl)
library(foreach)
######################################################################
# Change Working Directorys
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/18.Covid19_CrossNational/") 

######################################################################
# Data create
######################################################################
list <- excel_sheets("STMFinput/stmf.xlsx")[c(2:26)]
All <- lapply(list,function(i){
  read_excel("STMFinput/stmf.xlsx",sheet = i,skip=2) %>% 
    dplyr::select(Country,Year,Week,Sex,r0=`0-14...11`,r15=`15-64...12`,r65=`65-74...13`,
                  r75=`75-84...14`,r85=`85+...15`,rt=`Total...16`)
})
df_all <- do.call(bind_rows, All) %>% 
  mutate(Year=as.character(Year),
         rt=rt*1000,
         r85=r85*1000,
         r75=r75*1000,
         r15=r15*1000) %>% 
  filter(Country != "ISL" & Year>2014)  #ISL's mortality looks messy

df_20162019 <- df_all %>%
  filter(Year != "2020") %>% 
  group_by(Country, Week, Sex) %>% 
  summarise_at(c("r0", "r15","r65","r75","r85","rt"),mean) %>% 
  mutate(Year="2015-2019")

df <- df_all %>% 
  filter(Year == "2020") %>% 
  bind_rows(df_20162019) %>% 
  mutate(  excess85=r85/r15,
           excess75=r75/r15)

dfb <- df %>% 
  filter(Sex == "b") 

dff <- df %>% 
  filter(Sex == "f") 

dfm <- df %>% 
  filter(Sex == "m")

######################################################################
# Data viz
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/18.Covid19_CrossNational/Covid19_CrossNational_Elderly")

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_excess85b <- ggplot(dfb, mapping = aes(x=Week,y=excess85,group=Year,color=Year,shape=Year))+
  geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,250)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=cbp1)+
  ggtitle("Weekly excess mortality ratios (85+ relative to 15-64, both sexes)")
ggsave(plot_excess85b,height=8,width=12,dpi=200, filename="Figures/excess85b.pdf",  family = "Helvetica")
ggsave(plot_excess85b,height=6,width=9,dpi=200, filename="Figures/excess85b.png",  family = "Helvetica")

plot_excess85f <- ggplot(dff, mapping = aes(x=Week,y=excess85,group=Year,color=Year,shape=Year))+
  geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,250)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=cbp1)+
  ggtitle("Weekly excess mortality ratios (85+ relative to 15-64, female)")
ggsave(plot_excess85f,height=8,width=12,dpi=200, filename="Figures/excess85f.pdf",  family = "Helvetica")

plot_excess85m <- ggplot(dfm, mapping = aes(x=Week,y=excess85,group=Year,color=Year,shape=Year))+
  geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,250)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=cbp1)+
  ggtitle("Weekly excess mortality ratios (85+ relative to 15-64, male)")
ggsave(plot_excess85m,height=8,width=12,dpi=200, filename="Figures/excess85m.pdf",  family = "Helvetica")

plot_excess75b <- ggplot(dfb, mapping = aes(x=Week,y=excess75,group=Year,color=Year,shape=Year))+
  geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,60)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=cbp1)+
  ggtitle("Weekly excess mortality ratios (75-84 relative to 15-64, both sexes)")
ggsave(plot_excess75b,height=8,width=12,dpi=200, filename="Figures/excess75b.pdf",  family = "Helvetica")

plot_excess75f <- ggplot(dff, mapping = aes(x=Week,y=excess75,group=Year,color=Year,shape=Year))+
  geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,60)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=cbp1)+
  ggtitle("Weekly excess mortality ratios (75-84 relative to 15-64, female)")
ggsave(plot_excess75f,height=8,width=12,dpi=200, filename="Figures/excess75f.pdf",  family = "Helvetica")

plot_excess75m <- ggplot(dfm, mapping = aes(x=Week,y=excess75,group=Year,color=Year,shape=Year))+
  geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Ratio of mortality rate")+ylim(0,60)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=cbp1)+
  ggtitle("Weekly excess mortality ratios (75-84 relative to 15-64, male)")
ggsave(plot_excess75m,height=8,width=12,dpi=200, filename="Figures/excess75m.pdf",  family = "Helvetica")

plot_total <- ggplot(df, mapping = aes(x=Week,y=rt,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,30)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (all ages, both sexes)")
ggsave(plot_total,height=8,width=12,dpi=200, filename="Figures/Total.pdf",  family = "Helvetica")

plot_85 <- ggplot(df, mapping = aes(x=Week,y=r85,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,500)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (85+, both sexes)")
ggsave(plot_85,height=8,width=12,dpi=200, filename="Figures/Age85+.pdf",  family = "Helvetica")

plot_75 <- ggplot(df, mapping = aes(x=Week,y=r75,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,150)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (75-84, both sexes)")
ggsave(plot_75,height=8,width=12,dpi=200, filename="Figures/Age75-84.pdf",  family = "Helvetica")

plot_15 <- ggplot(df, mapping = aes(x=Week,y=r15,group=Year,color=Year,shape=Year))+
  geom_point()+geom_line()+facet_wrap(~Country)+xlab("Week")+ylab("Death per 1,000")+ylim(0,10)+
  theme_few() +theme(legend.title=element_blank(), legend.position = "bottom")+
  scale_colour_manual(values=rev(cbp1))+
  ggtitle("Weekly mortality rates in US, UK (England and Wales), Italy and Spain (15-64, both sexes)")
ggsave(plot_15,height=8,width=12,dpi=200, filename="Figures/Age15-64.pdf",  family = "Helvetica")

