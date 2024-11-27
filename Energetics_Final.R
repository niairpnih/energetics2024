# Skeletal muscle mRNA splicing variant association with four different fitness and energetic measures in the GESTALT study
# Metadata files, folders and library preparation are not provided. 
# We share below the R code used to generate analysis, tables and figures.

# Figure 1a: Cross-correlation Matrix 
{
  ##Here we use the ggpairs function() of ggally
  head(metadata_muscle_mass3)

  metadata_for_ggally<-metadata_muscle_mass3%>%dplyr::select(years,FiberRatio,Percent_Muscle_Mass,bmi,WaistCirc_height,iso_ct,sex)

  figure1<-ggpairs(data=metadata_for_ggally,upper = list(continuous = wrap("cor", size = 3)), ggplot2::aes(colour = sex,alpha = 0.7),columnLabels = c("Age", "Fiber I/II", "Muscle Mass", "BMI", "Waist Circumf./Height", "mRNAs detected","Sex"))+
  theme(text=element_text(size=10))
figure1 #save 7*6

}

# Supplementary Figure S5
{ 
  ##highxmin vs Age
  mod_Age_highxmin<-lm(metadata_muscle_mass3$highxmin~metadata_muscle_mass3$years,data=metadata)  
  summary(mod_Age_highxmin)  
  p_val_Age_highxmin<-round(summary(mod_Age_highxmin)$coef[2,4],3)
  rsq_Age_highxmin<-round(summary(mod_Age_highxmin)$r.squared,3)
  title_Age_highxmin<-"PA vs Age"
  subtitle_Age_highxmin<-paste("r^2:",rsq_Age_highxmin,"\np-value:",p_val_Age_highxmin)
  summary(mod_Age_highxmin)
  p1<-ggplot(metadata_muscle_mass3,aes(x=years,y=highxmin))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_Age_highxmin,
            subtitle=subtitle_Age_highxmin)+
    xlab("Age")+
    ylab("PA")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  ####VO2 vs Age
  mod_Age_VO2<-lm(metadata_muscle_mass3$VO2~metadata_muscle_mass3$years,data=metadata)  
  summary(mod_Age_VO2)  
  p_val_Age_VO2<-round(summary(mod_Age_VO2)$coef[2,4],10)
  rsq_Age_VO2<-round(summary(mod_Age_VO2)$r.squared,3)
  title_Age_VO2<-"VO2 vs Age"
  subtitle_Age_VO2<-paste("r^2:",rsq_Age_VO2,"\np-value:",p_val_Age_VO2)
  summary(mod_Age_VO2)
  p2<-ggplot(metadata_muscle_mass3,aes(x=years,y=VO2))+
    geom_point(size=1, col='blue')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_Age_VO2,
            subtitle=subtitle_Age_VO2)+
    xlab("Age")+
    ylab("VO2")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  ###kPCr vs Age
  mod_Age_kPCr<-lm(metadata_muscle_mass3$kPCr~metadata_muscle_mass3$years,data=metadata)  
  summary(mod_Age_kPCr)  
  p_val_Age_kPCr<-signif(summary(mod_Age_kPCr)$coef[2,4],3)
  rsq_Age_kPCr<-round(summary(mod_Age_kPCr)$r.squared,3)
  title_Age_kPCr<-"kPCr vs Age"
  subtitle_Age_kPCr<-paste("r^2:",rsq_Age_kPCr,"\np-value:",p_val_Age_kPCr)
  summary(mod_Age_kPCr)
  p3<-ggplot(metadata_muscle_mass3,aes(x=years,y=kPCr))+
    geom_point(size=1, col='blue')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_Age_kPCr,
            subtitle=subtitle_Age_kPCr)+
    xlab("Age")+
    ylab("kPCr")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  ###5ADP vs Age
  mod_Age_Respirometry<-lm(metadata_Respirometry_muscle_mass3$X5ADP~metadata_Respirometry_muscle_mass3$years,data=metadata_Respirometry_muscle_mass3)  
  summary(mod_Age_Respirometry)  
  p_val_AgeRespirometry<-round(summary(mod_Age_Respirometry)$coef[2,4],5)
  rsq_Age_Respirometry<-round(summary(mod_Age_Respirometry)$r.squared,5)
  title_Age_Respirometry<-"Mit-O2flux vs Age"
  subtitle_Age_Respirometry<-paste("r^2:",rsq_Age_Respirometry,"\np-value:",p_val_AgeRespirometry)
  summary(mod_Age_Respirometry)
  p4<-ggplot(metadata_Respirometry_muscle_mass3,aes(x=years,y=Respirometry))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_Age_Respirometry,
            subtitle=subtitle_Age_Respirometry)+
    xlab("Age")+
    ylab("Mit-O2flux")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
 
  ####Physical Activity vs %  Muscle Mass
  mod_MuscleMass_highxmin<-lm(metadata_muscle_mass3$highxmin~metadata_muscle_mass3$Percent_Muscle_Mass,data=metadata)  
  summary(mod_MuscleMass_highxmin)  
  p_val_MuscleMass_highxmin<-round(summary(mod_MuscleMass_highxmin)$coef[2,4],3)
  rsq_MuscleMass_highxmin<-round(summary(mod_MuscleMass_highxmin)$r.squared,3)
  title_MuscleMass_highxmin<-"PA vs MuscleMass"
  subtitle_MuscleMass_highxmin<-paste("r^2:",rsq_MuscleMass_highxmin,"\np-value:",p_val_MuscleMass_highxmin)
  summary(mod_MuscleMass_highxmin)
  p5<-ggplot(metadata_muscle_mass3,aes(x=Percent_Muscle_Mass,y=highxmin))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_MuscleMass_highxmin,
            subtitle=subtitle_MuscleMass_highxmin)+
    xlab("MuscleMass")+
    ylab("PA")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  #VO2 vs % muscle mass
  mod_MuscleMass_VO2<-lm(metadata_muscle_mass3$VO2~metadata_muscle_mass3$Percent_Muscle_Mass,data=metadata)  
  summary(mod_MuscleMass_VO2)  
  p_val_MuscleMass_VO2<-round(summary(mod_MuscleMass_VO2)$coef[2,4],10)
  rsq_MuscleMass_VO2<-round(summary(mod_MuscleMass_VO2)$r.squared,3)
  title_MuscleMass_VO2<-"VO2 vs MuscleMass"
  subtitle_MuscleMass_VO2<-paste("r^2:",rsq_MuscleMass_VO2,"\np-value:",p_val_MuscleMass_VO2)
  summary(mod_MuscleMass_VO2)
  p6<-ggplot(metadata_muscle_mass3,aes(x=Percent_Muscle_Mass,y=VO2))+
    geom_point(size=1, col='red')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_MuscleMass_VO2,
            subtitle=subtitle_MuscleMass_VO2)+
    xlab("MuscleMass")+
    ylab("VO2")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  ####kPCr vs % Muscle Mass
  mod_MuscleMass_kPCr<-lm(metadata_muscle_mass3$kPCr~metadata_muscle_mass3$Percent_Muscle_Mass,data=metadata)  
  summary(mod_MuscleMass_kPCr)  
  p_val_MuscleMass_kPCr<-signif(summary(mod_MuscleMass_kPCr)$coef[2,4],3)
  rsq_MuscleMass_kPCr<-round(summary(mod_MuscleMass_kPCr)$r.squared,3)
  title_MuscleMass_kPCr<-"kPCr vs MuscleMass"
  subtitle_MuscleMass_kPCr<-paste("r^2:",rsq_MuscleMass_kPCr,"\np-value:",p_val_MuscleMass_kPCr)
  summary(mod_MuscleMass_kPCr)
  p7<-ggplot(metadata_muscle_mass3,aes(x=Percent_Muscle_Mass,y=kPCr))+
    geom_point(size=1, col='red')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_MuscleMass_kPCr,
            subtitle=subtitle_MuscleMass_kPCr)+
    xlab("MuscleMass")+
    ylab("kPCr")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  ###Respirometry vs Percent Muscle Mass
  mod_MuscleMass_Respirometry<-lm(metadata_Respirometry_muscle_mass3$X5ADP~metadata_Respirometry_muscle_mass3$Percent_Muscle_Mass,data=metadata_Respirometry_muscle_mass3)  
  summary(mod_MuscleMass_Respirometry)  
  p_val_MuscleMassRespirometry<-round(summary(mod_MuscleMass_Respirometry)$coef[2,4],5)
  rsq_MuscleMass_Respirometry<-round(summary(mod_MuscleMass_Respirometry)$r.squared,5)
  title_MuscleMass_Respirometry<-"Mit-O2flux vs MuscleMass"
  subtitle_MuscleMass_Respirometry<-paste("r^2:",rsq_MuscleMass_Respirometry,"\np-value:",p_val_MuscleMassRespirometry)
  summary(mod_MuscleMass_Respirometry)
  p8<-ggplot(metadata_Respirometry_muscle_mass3,aes(x=Percent_Muscle_Mass,y=Respirometry))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_MuscleMass_Respirometry,
            subtitle=subtitle_MuscleMass_Respirometry)+
    xlab("MuscleMass")+
    ylab("Mit-O2flux")+
    scale_x_continuous(breaks=seq(20,90,by=10),labels=seq(20,90,by=10))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  
  
  ###Physical Activity vs Fiber Ratio
  mod_FiberRatio_I_II_highxmin<-lm(metadata_muscle_mass3$highxmin~metadata_muscle_mass3$FiberRatio,data=metadata)  
  summary(mod_FiberRatio_I_II_highxmin)  
  p_val_FiberRatio_I_II_highxmin<-round(summary(mod_FiberRatio_I_II_highxmin)$coef[2,4],3)
  rsq_FiberRatio_I_II_highxmin<-round(summary(mod_FiberRatio_I_II_highxmin)$r.squared,3)
  title_FiberRatio_I_II_highxmin<-"PA vs FiberRatio_I_II"
  subtitle_FiberRatio_I_II_highxmin<-paste("r^2:",rsq_FiberRatio_I_II_highxmin,"\np-value:",p_val_FiberRatio_I_II_highxmin)
  summary(mod_FiberRatio_I_II_highxmin)
  p9<-ggplot(metadata_muscle_mass3,aes(x=FiberRatio,y=highxmin))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_FiberRatio_I_II_highxmin,
            subtitle=subtitle_FiberRatio_I_II_highxmin)+
    xlab("FiberRatio_I_II ")+
    ylab("PA")+
    scale_x_continuous(breaks=seq(0,3,by=0.5),labels=seq(0,3,by=0.5))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  ###VO2 vs Fiber Ratio
  mod_FiberRatio_I_II_VO2<-lm(metadata_muscle_mass3$VO2~metadata_muscle_mass3$FiberRatio,data=metadata)  
  summary(mod_FiberRatio_I_II_VO2)  
  p_val_FiberRatio_I_II_VO2<-round(summary(mod_FiberRatio_I_II_VO2)$coef[2,4],10)
  rsq_FiberRatio_I_II_VO2<-round(summary(mod_FiberRatio_I_II_VO2)$r.squared,3)
  title_FiberRatio_I_II_VO2<-"VO2 vs FiberRatio_I_II"
  subtitle_FiberRatio_I_II_VO2<-paste("r^2:",rsq_FiberRatio_I_II_VO2,"\np-value:",p_val_FiberRatio_I_II_VO2)
  summary(mod_FiberRatio_I_II_VO2)
  p10<-ggplot(metadata_muscle_mass3,aes(x=FiberRatio,y=VO2))+
    geom_point(size=1, col='blue')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_FiberRatio_I_II_VO2,
            subtitle=subtitle_FiberRatio_I_II_VO2)+
    xlab("FiberRatio_I_II ")+
    ylab("VO2")+
    scale_x_continuous(breaks=seq(0,3,by=0.5),labels=seq(0,3,by=0.5))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  p10
  ##VO2 vs Fiber Ratio
  mod_FiberRatio_I_II_VO2<-lm(metadata_muscle_mass3$VO2~metadata_muscle_mass3$FiberRatio+metadata_muscle_mass3$years,data=metadata)  
  summary(mod_FiberRatio_I_II_VO2)  
  p_val_FiberRatio_I_II_VO2<-round(summary(mod_FiberRatio_I_II_VO2)$coef[2,4],10)
  rsq_FiberRatio_I_II_VO2<-round(summary(mod_FiberRatio_I_II_VO2)$r.squared,3)
  title_FiberRatio_I_II_VO2<-"VO2 vs FiberRatio_I_II"
  subtitle_FiberRatio_I_II_VO2<-paste("r^2:",rsq_FiberRatio_I_II_VO2,"\np-value:",p_val_FiberRatio_I_II_VO2)
  summary(mod_FiberRatio_I_II_VO2)
  p10_adjusted<-ggplot(metadata_muscle_mass3,aes(x=FiberRatio,y=VO2))+
    geom_point(size=1, col='blue')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_FiberRatio_I_II_VO2,
            subtitle=subtitle_FiberRatio_I_II_VO2)+
    xlab("FiberRatio_I_II ")+
    ylab("VO2")+
    scale_x_continuous(breaks=seq(0,3,by=0.5),labels=seq(0,3,by=0.5))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  p10_adjusted
  ##kPCr vs Fiber Ratio
  mod_FiberRatio_I_II_kPCr<-lm(metadata_muscle_mass3$kPCr~metadata_muscle_mass3$FiberRatio,data=metadata)  
  summary(mod_FiberRatio_I_II_kPCr)  
  p_val_FiberRatio_I_II_kPCr<-signif(summary(mod_FiberRatio_I_II_kPCr)$coef[2,4],3)
  rsq_FiberRatio_I_II_kPCr<-round(summary(mod_FiberRatio_I_II_kPCr)$r.squared,3)
  title_FiberRatio_I_II_kPCr<-"kPCr vs FiberRatio_I_II"
  subtitle_FiberRatio_I_II_kPCr<-paste("r^2:",rsq_FiberRatio_I_II_kPCr,"\np-value:",p_val_FiberRatio_I_II_kPCr)
  summary(mod_FiberRatio_I_II_kPCr)
  p11<-ggplot(metadata_muscle_mass3,aes(x=FiberRatio,y=kPCr))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_FiberRatio_I_II_kPCr,
            subtitle=subtitle_FiberRatio_I_II_kPCr)+
    xlab("FiberRatio_I_II ")+
    ylab("kPCr")+
    scale_x_continuous(breaks=seq(0,3,by=0.5),labels=seq(0,3,by=0.5))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  mod_FiberRatio_I_II_Respirometry<-lm(metadata_Respirometry_muscle_mass3$X5ADP~metadata_Respirometry_muscle_mass3$FiberRatio,data=metadata_Respirometry_muscle_mass3)  
  summary(mod_FiberRatio_I_II_Respirometry)  
  p_val_FiberRatio_I_IIRespirometry<-round(summary(mod_FiberRatio_I_II_Respirometry)$coef[2,4],5)
  rsq_FiberRatio_I_II_Respirometry<-round(summary(mod_FiberRatio_I_II_Respirometry)$r.squared,5)
  title_FiberRatio_I_II_Respirometry<-"Mit-O2flux vs FiberRatio_I_II"
  subtitle_FiberRatio_I_II_Respirometry<-paste("r^2:",rsq_FiberRatio_I_II_Respirometry,"\np-value:",p_val_FiberRatio_I_IIRespirometry)
  summary(mod_FiberRatio_I_II_Respirometry)
  p12<-ggplot(metadata_Respirometry_muscle_mass3,aes(x=FiberRatio,y=Respirometry))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_FiberRatio_I_II_Respirometry,
            subtitle=subtitle_FiberRatio_I_II_Respirometry)+
    xlab("FiberRatio_I_II ")+
    ylab("Mit-O2flux")+
    scale_x_continuous(breaks=seq(0,3,by=0.5),labels=seq(0,3,by=0.5))+
    theme(axis.text.x = element_text(size=9),axis.text.y = element_text(size=9),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  

  ### Highxmin vs VO2
  mod_highxmin_VO2<-lm(metadata_muscle_mass3$highxmin~metadata_muscle_mass3$VO2,data=metadata)  
  summary(mod_highxmin_VO2)  
  
  p_val_highxmin_VO2<-signif(summary(mod_highxmin_VO2)$coef[2,4],3)
  rsq_highxmin_VO2<-round(summary(mod_highxmin_VO2)$r.squared,3)
  title_highxmin_VO2<-"PA vs VO2peak"
  subtitle_highxmin_VO2<-paste("r^2:",rsq_highxmin_VO2,"\np-value:",p_val_highxmin_VO2)
  summary(mod_highxmin_VO2)
  p13<-ggplot(metadata_muscle_mass3,aes(x=VO2,y=highxmin))+
    geom_point(size=1, col='red')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_highxmin_VO2,
            subtitle=subtitle_highxmin_VO2)+
    xlab("VO2")+
    ylab("PA")+
    #scale_x_continuous(breaks=seq(20,90,10),labels=seq(20,90,10))+
    theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  ##Highxmin vs kPCr
  mod_highxmin_kPCr<-lm(metadata_muscle_mass3$highxmin~metadata_muscle_mass3$kPCr,data=metadata)  
  summary(mod_highxmin_kPCr)  
  
  p_val_highxmin_kPCr<-signif(summary(mod_highxmin_kPCr)$coef[2,4],3)
  rsq_highxmin_kPCr<-round(summary(mod_highxmin_kPCr)$r.squared,3)
  title_highxmin_kPCr<-"PA vs kPCr"
  subtitle_highxmin_kPCr<-paste("r^2:",rsq_highxmin_kPCr,"\np-value:",p_val_highxmin_kPCr)
  summary(mod_highxmin_kPCr)
  p14<-ggplot(metadata_muscle_mass3,aes(x=kPCr,y=highxmin))+
    geom_point(size=1, col='red')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_highxmin_kPCr,
            subtitle=subtitle_highxmin_kPCr)+
    xlab("kPCr")+
    ylab("PA")+
    #scale_x_continuous(breaks=seq(20,90,10),labels=seq(20,90,10))+
    theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  ### VO2 vs kPCr
  mod_VO2_kPCr<-lm(metadata_muscle_mass3$VO2~metadata_muscle_mass3$kPCr,data=metadata)  
  summary(mod_VO2_kPCr)  
  
  p_val_VO2_kPCr<-signif(summary(mod_VO2_kPCr)$coef[2,4],3)
  rsq_VO2_kPCr<-round(summary(mod_VO2_kPCr)$r.squared,3)
  title_VO2_kPCr<-"VO2peak vs kPCr"
  subtitle_VO2_kPCr<-paste("r^2:",rsq_VO2_kPCr,"\np-value:",p_val_VO2_kPCr)
  summary(mod_VO2_kPCr)
  p15<-ggplot(metadata_muscle_mass3,aes(x=kPCr,y=VO2))+
    geom_point(size=1, col='red')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_VO2_kPCr,
            subtitle=subtitle_VO2_kPCr)+
    xlab("kPCr")+
    ylab("VO2")+
    #scale_x_continuous(breaks=seq(20,90,10),labels=seq(20,90,10))+
    theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  ######Highxmin vs Respirometry
  mod_highxmin_Resp<-lm(metadata_Respirometry_muscle_mass3$highxmin~metadata_Respirometry_muscle_mass3$X5ADP,data=metadata)  
  summary(mod_highxmin_Resp)  
  
  p_val_highxmin_Resp<-signif(summary(mod_highxmin_Resp)$coef[2,4],3)
  rsq_highxmin_Resp<-round(summary(mod_highxmin_Resp)$r.squared,3)
  title_highxmin_Resp<-"PA vs Mit-O2flux"
  subtitle_highxmin_Resp<-paste("r^2:",rsq_highxmin_Resp,"\np-value:",p_val_highxmin_Resp)
  summary(mod_highxmin_Resp)
  p16<-ggplot(metadata_Respirometry_muscle_mass3,aes(x=X5ADP,y=highxmin))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_highxmin_Resp,
            subtitle=subtitle_highxmin_Resp)+
    xlab("Mit-O2flux")+
    ylab("PA ")+
    #scale_x_continuous(breaks=seq(20,90,10),labels=seq(20,90,10))+
    theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  ### Respirometry vs VO2
  mod_Resp_VO2<-lm(metadata_Respirometry_muscle_mass3$X5ADP~metadata_Respirometry_muscle_mass3$VO2,data=metadata)  
  summary(mod_Resp_VO2)  
  
  p_val_Resp_VO2<-signif(summary(mod_Resp_VO2)$coef[2,4],5)
  rsq_Resp_VO2<-round(summary(mod_Resp_VO2)$r.squared,5)
  title_Resp_VO2<-"Mit-O2flux vs VO2peak"
  subtitle_Resp_VO2<-paste("r^2:",rsq_Resp_VO2,"\np-value:",p_val_Resp_VO2)
  summary(mod_Resp_VO2)
  p17<-ggplot(metadata_Respirometry_muscle_mass3,aes(x=VO2,y=X5ADP))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_Resp_VO2,
            subtitle=subtitle_Resp_VO2)+
    ylab("Mit-O2flux")+
    xlab("VO2")+
    #scale_x_continuous(breaks=seq(20,90,10),labels=seq(20,90,10))+
    theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  
  ### Respirometry vs kPCr
  mod_Resp_kPCr<-lm(metadata_Respirometry_muscle_mass3$X5ADP~metadata_Respirometry_muscle_mass3$kPCr,data=metadata)  
  summary(mod_Resp_kPCr)  
  
  p_val_Resp_kPCr<-signif(summary(mod_Resp_kPCr)$coef[2,4],3)
  rsq_Resp_kPCr<-round(summary(mod_Resp_kPCr)$r.squared,3)
  title_Resp_kPCr<-"Mit-O2flux vs kPCr"
  subtitle_Resp_kPCr<-paste("r^2:",rsq_Resp_kPCr,"\np-value:",p_val_Resp_kPCr)
  summary(mod_Resp_kPCr)
  p18<-ggplot(metadata_Respirometry_muscle_mass3,aes(x=kPCr,y=X5ADP))+
    geom_point(size=1, col='grey')+
    geom_smooth(method="lm",col='black')+
    theme_classic()+
    ggtitle(title_Resp_kPCr,
            subtitle=subtitle_Resp_kPCr)+
    ylab("Mit-O2flux")+
    xlab("kPCr")+
    #scale_x_continuous(breaks=seq(20,90,10),labels=seq(20,90,10))+
    theme(axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),plot.title=element_text(size=10),
          plot.subtitle=element_text(size=9))  
  #I changed this a bit.  Wanted equal plot size.
  (p1 | p2 | p3 | p4) /
    (p5 | p6 | p7 | p8) /
  (p9 |p10|p11|p12)/
    (p13|p14|p15|p16|p17|p18)+plot_annotation(tag_levels = 'a')&theme(plot.tag = element_text(size = 15))
  #14*9
  
}  

# Figure 2: Differential Gene Expression (also used to generate Figure 5)
{
# note: "adjusted" refers to the metadata file including the muscle mass parameter (however, is NOT adjusted for muscle mass)
# highxmin
  ##Adjusted for sex,years,batch and Fiber Ratio
{
    dds_highxmin_adjusted_muscle_mass3<-DESeqDataSetFromMatrix(countData=feature_dat,colData = metadata_muscle_mass3,design=~sex+years+phase+FiberRatio+highxmin)
    dds_highxmin_adjusted_muscle_mass3<-DESeq(dds_highxmin_adjusted_muscle_mass3)
    summary(dds_highxmin_adjusted_muscle_mass3)
    res_highxmin_adjusted_muscle_mass3<-DESeq2::results(dds_highxmin_adjusted_muscle_mass3,alpha=0.10)
    
    summary(res_highxmin_adjusted_muscle_mass3)
    
    res_ordered_highxmin_adjusted_muscle_mass3<-res_highxmin_adjusted_muscle_mass3[order(res_highxmin_adjusted_muscle_mass3$pvalue),]
    res_subset_highxmin_adjusted_muscle_mass3<-data.frame(res_ordered_highxmin_adjusted_muscle_mass3)
    res_subset_highxmin_adjusted_muscle_mass3<-subset(res_subset_highxmin_adjusted_muscle_mass3,pvalue<0.01)
    #sum(rownames(res_subset_highxmin_adjusted_muscle_mass3)%in%rownames(res_subset_highxmin_adjusted))
    DEseq2_full_model_highxmin_adjusted_muscle_mass3<-merge(data.frame(res_highxmin_adjusted_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
    DEseq2_full_model_highxmin_adjusted_muscle_mass3<-DEseq2_full_model_highxmin_adjusted_muscle_mass3[order(DEseq2_full_model_highxmin_adjusted_muscle_mass3$pvalue),]
    write.csv(DEseq2_full_model_highxmin_adjusted_muscle_mass3,"DEseq2_highxmin_muscle_mass3.csv")
    DEseq2_full_model_highxmin_adjusted_muscle_mass3_protein_coding<-DEseq2_full_model_highxmin_adjusted_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    DEseq2_full_model_highxmin_adjusted_muscle_mass3_non_coding<-DEseq2_full_model_highxmin_adjusted_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    sum(DEseq2_full_model_highxmin_adjusted_muscle_mass3$padj<0.01,na.rm=TRUE)
    sum(DEseq2_full_model_highxmin_adjusted_muscle_mass3$padj<0.05,na.rm=TRUE)
    sum(DEseq2_full_model_highxmin_adjusted_muscle_mass3$padj<0.10,na.rm=TRUE)
    
    sum(DEseq2_full_model_highxmin_adjusted_muscle_mass3$pvalue<0.01&DEseq2_full_model_highxmin_adjusted_muscle_mass3$log2FoldChange>0,na.rm=TRUE)
    sum(DEseq2_full_model_highxmin_adjusted_muscle_mass3$pvalue<0.01&DEseq2_full_model_highxmin_adjusted_muscle_mass3$log2FoldChange<0,na.rm=TRUE)
    
    sum(DEseq2_full_model_highxmin_adjusted_muscle_mass3$pvalue<0.05,na.rm=TRUE)
    DEseq2_sig_highxmin_adjusted_muscle_mass3<-subset(DEseq2_full_model_highxmin_adjusted_muscle_mass3,pvalue<0.01)
    res_subset_highxmin_adjusted_muscle_mass3<-subset(res_ordered_highxmin_adjusted_muscle_mass3,pvalue<0.01)
    res_subset_highxmin_adjusted_muscle_mass3<-data.frame(res_subset_highxmin_adjusted_muscle_mass3)
    DEseq2_sig_highxmin_0.05<-subset(DEseq2_full_model_highxmin_adjusted_muscle_mass3,pvalue<0.05)
    merged_final_highxmin_adjusted_muscle_mass3<-merge(res_subset_highxmin_adjusted_muscle_mass3,annot,by.x="row.names",by.y="Gene_stable_ID")
    merged_final_highxmin_adjusted_muscle_mass3<-merged_final_highxmin_adjusted_muscle_mass3[order(merged_final_highxmin_adjusted_muscle_mass3$pvalue),]
    colnames(merged_final_highxmin_adjusted_muscle_mass3)[1]<-"Gene_ID"
    merged_final_highxmin_adjusted_muscle_mass3_protein_coding<-merged_final_highxmin_adjusted_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    merged_final_highxmin_adjusted_muscle_mass3_non_coding<-merged_final_highxmin_adjusted_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    write.csv(merged_final_highxmin_adjusted_muscle_mass3, file="DEseq2_highxmin_sig_muscle_mass3.csv")
    write.csv(merged_final_highxmin_adjusted_muscle_mass3_protein_coding, file="DEseq2_highxmin_sig_protein_coding_muscle_mass3.csv")
    write.csv(merged_final_highxmin_adjusted_muscle_mass3_non_coding, file="DEseq2_highxmin_sig_non_coding_muscle_mass3.csv")
    table(merged_final_highxmin_adjusted_muscle_mass3$Gene_type)
    
    sum(merged_final_highxmin_adjusted_muscle_mass3_protein_coding$log2FoldChange>0,na.rm=TRUE)
    sum(merged_final_highxmin_adjusted_muscle_mass3_protein_coding$log2FoldChange<0,na.rm=TRUE)
  }
  
# VO2
##Adjusted as in highxmin
{
    all(colnames(feature_dat)==metadata_muscle_mass3$name)
    dds_adjusted_muscle_mass3<-DESeqDataSetFromMatrix(countData=feature_dat,colData = metadata_muscle_mass3,design=~sex+years+phase+FiberRatio+VO2)
    dds_VO2_muscle_mass3<-DESeq(dds_adjusted_muscle_mass3)
    summary(dds_VO2_muscle_mass3)
    res_VO2_muscle_mass3<-DESeq2::results(dds_VO2_muscle_mass3,alpha=0.10)
    summary(res_VO2_muscle_mass3)
    
    res_ordered_VO2_muscle_mass3<-res_VO2_muscle_mass3[order(res_VO2_muscle_mass3$pvalue),]
    res_subset_VO2_muscle_mass3<-data.frame(res_ordered_VO2_muscle_mass3)
    res_subset_VO2_muscle_mass3<-subset(res_subset_VO2_muscle_mass3,pvalue<0.01)
    #sum(rownames(res_subset_VO2_muscle_mass3)%in%rownames(res_subset_VO2))
    DEseq2_full_model_VO2_muscle_mass3<-merge(data.frame(res_VO2_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
    DEseq2_full_model_VO2_muscle_mass3<-DEseq2_full_model_VO2_muscle_mass3[order(DEseq2_full_model_VO2_muscle_mass3$pvalue),]
    write.csv(DEseq2_full_model_VO2_muscle_mass3,"DEseq2_VO2only_muscle_mass3.csv")
    DEseq2_full_model_VO2_muscle_mass3_protein_coding<-DEseq2_full_model_VO2_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    DEseq2_full_model_VO2_muscle_mass3_non_coding<-DEseq2_full_model_VO2_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    sum(DEseq2_full_model_VO2_muscle_mass3$padj<0.01,na.rm=TRUE)
    sum(DEseq2_full_model_VO2_muscle_mass3$padj<0.05,na.rm=TRUE)
    sum(DEseq2_full_model_VO2_muscle_mass3$padj<0.10,na.rm=TRUE)
    sum(DEseq2_full_model_VO2_muscle_mass3$pvalue<0.01,na.rm=TRUE)
    
    sum(DEseq2_full_model_VO2_muscle_mass3$pvalue<0.01&DEseq2_full_model_VO2_muscle_mass3$log2FoldChange>0,na.rm=TRUE)
    sum(DEseq2_full_model_VO2_muscle_mass3$pvalue<0.01&DEseq2_full_model_VO2_muscle_mass3$log2FoldChange<0,na.rm=TRUE)
    
    sum(DEseq2_full_model_VO2_muscle_mass3$pvalue<0.05,na.rm=TRUE)
    DEseq2_sig_VO2_muscle_mass3<-subset(DEseq2_full_model_VO2_muscle_mass3,pvalue<0.01)
    res_subset_VO2_muscle_mass3<-subset(res_ordered_VO2_muscle_mass3,pvalue<0.01)
    res_subset_VO2_muscle_mass3<-data.frame(res_subset_VO2_muscle_mass3)
    DEseq2_sig_VO2only_0.05<-subset(DEseq2_full_model_VO2_muscle_mass3,pvalue<0.05)
    merged_final_VO2_muscle_mass3<-merge(res_subset_VO2_muscle_mass3,annot,by.x="row.names",by.y="Gene_stable_ID")
    merged_final_VO2_muscle_mass3<-merged_final_VO2_muscle_mass3[order(merged_final_VO2_muscle_mass3$pvalue),]
    colnames(merged_final_VO2_muscle_mass3)[1]<-"Gene_ID"
    merged_final_VO2_muscle_mass3_protein_coding<-merged_final_VO2_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    merged_final_VO2_muscle_mass3_non_coding<-merged_final_VO2_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    write.csv(merged_final_VO2_muscle_mass3, file="DEseq2_VO2_sig_muscle_mass3.csv")
    write.csv(merged_final_VO2_muscle_mass3_protein_coding, file="DEseq2_VO2_sig_protein_coding_muscle_mass3.csv")
    write.csv(merged_final_VO2_muscle_mass3_non_coding, file="DEseq2_VO2_sig_non_coding_muscle_mass3.csv")
    table(merged_final_VO2_muscle_mass3$Gene_type)
    
    sum(merged_final_VO2_muscle_mass3_protein_coding$log2FoldChange>0,na.rm=TRUE)
    sum(merged_final_VO2_muscle_mass3_protein_coding$log2FoldChange<0,na.rm=TRUE)
  }
  
# kPCr
#Adjusted as in the above 2 analyses
{
    dds_kPCr_adjusted_muscle_mass3<-DESeqDataSetFromMatrix(countData=feature_dat,colData = metadata_muscle_mass3,design=~sex+years+phase+FiberRatio+kPCr)
    dds_kPCr_adjusted_muscle_mass3<-DESeq(dds_kPCr_adjusted_muscle_mass3)
    summary(dds_kPCr_adjusted_muscle_mass3)
    res_kPCr_adjusted_muscle_mass3<-DESeq2::results(dds_kPCr_adjusted_muscle_mass3,alpha=0.10)
    summary(res_kPCr_adjusted_muscle_mass3)
    
    res_ordered_kPCr_adjusted_muscle_mass3<-res_kPCr_adjusted_muscle_mass3[order(res_kPCr_adjusted_muscle_mass3$pvalue),]
    res_subset_kPCr_adjusted_muscle_mass3<-data.frame(res_ordered_kPCr_adjusted_muscle_mass3)
    res_subset_kPCr_adjusted_muscle_mass3<-subset(res_subset_kPCr_adjusted_muscle_mass3,pvalue<0.01)
    #sum(rownames(res_subset_kPCr_adjusted_muscle_mass3)%in%rownames(res_subset_kPCr_adjusted))
    DEseq2_full_model_kPCr_adjusted_muscle_mass3<-merge(data.frame(res_kPCr_adjusted_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
    DEseq2_full_model_kPCr_adjusted_muscle_mass3<-DEseq2_full_model_kPCr_adjusted_muscle_mass3[order(DEseq2_full_model_kPCr_adjusted_muscle_mass3$pvalue),]
    write.csv(DEseq2_full_model_kPCr_adjusted_muscle_mass3,"DEseq2_kPCr_muscle_mass3.csv")
    DEseq2_full_model_kPCr_adjusted_muscle_mass3_protein_coding<-DEseq2_full_model_kPCr_adjusted_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    DEseq2_full_model_kPCr_adjusted_muscle_mass3_non_coding<-DEseq2_full_model_kPCr_adjusted_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    sum(DEseq2_full_model_kPCr_adjusted_muscle_mass3$padj<0.01,na.rm=TRUE)
    sum(DEseq2_full_model_kPCr_adjusted_muscle_mass3$padj<0.05,na.rm=TRUE)
    sum(DEseq2_full_model_kPCr_adjusted_muscle_mass3$padj<0.10,na.rm=TRUE)
    sum(DEseq2_full_model_kPCr_adjusted_muscle_mass3$pvalue<0.01,na.rm=TRUE)
    
    sum(DEseq2_full_model_kPCr_adjusted_muscle_mass3$pvalue<0.01&DEseq2_full_model_kPCr_adjusted_muscle_mass3$log2FoldChange>0,na.rm=TRUE)
    sum(DEseq2_full_model_kPCr_adjusted_muscle_mass3$pvalue<0.01&DEseq2_full_model_kPCr_adjusted_muscle_mass3$log2FoldChange<0,na.rm=TRUE)
    
    sum(DEseq2_full_model_kPCr_adjusted_muscle_mass3$pvalue<0.05,na.rm=TRUE)
    DEseq2_sig_kPCr_adjusted_muscle_mass3<-subset(DEseq2_full_model_kPCr_adjusted_muscle_mass3,pvalue<0.01)
    res_subset_kPCr_adjusted_muscle_mass3<-subset(res_ordered_kPCr_adjusted_muscle_mass3,pvalue<0.01)
    res_subset_kPCr_adjusted_muscle_mass3<-data.frame(res_subset_kPCr_adjusted_muscle_mass3)
    DEseq2_sig_kPCr_0.05<-subset(DEseq2_full_model_kPCr_adjusted_muscle_mass3,pvalue<0.05)
    merged_final_kPCr_adjusted_muscle_mass3<-merge(res_subset_kPCr_adjusted_muscle_mass3,annot,by.x="row.names",by.y="Gene_stable_ID")
    merged_final_kPCr_adjusted_muscle_mass3<-merged_final_kPCr_adjusted_muscle_mass3[order(merged_final_kPCr_adjusted_muscle_mass3$pvalue),]
    colnames(merged_final_kPCr_adjusted_muscle_mass3)[1]<-"Gene_ID"
    merged_final_kPCr_adjusted_muscle_mass3_protein_coding<-merged_final_kPCr_adjusted_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    merged_final_kPCr_adjusted_muscle_mass3_non_coding<-merged_final_kPCr_adjusted_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    write.csv(merged_final_kPCr_adjusted_muscle_mass3, file="DEseq2_kPCr_sig_muscle_mass3.csv")
    write.csv(merged_final_kPCr_adjusted_muscle_mass3_protein_coding, file="DEseq2_kPCr_sig_protein_coding_muscle_mass3.csv")
    write.csv(merged_final_kPCr_adjusted_muscle_mass3_non_coding, file="DEseq2_kPCr_sig_non_coding_muscle_mass3.csv")
    table(merged_final_kPCr_adjusted_muscle_mass3$Gene_type)#Muscle Mass
    
    sum(merged_final_kPCr_adjusted_muscle_mass3_protein_coding$log2FoldChange>0,na.rm=TRUE)
    sum(merged_final_kPCr_adjusted_muscle_mass3_protein_coding$log2FoldChange<0,na.rm=TRUE)
  }
  
# X5ADP (Respirometry)
  #Adjusted as above.
{
    all(colnames(feature_dat_Respirometry)==metadata_Respirometry_muscle_mass3$name)
    dds_X5ADP_adjusted_muscle_mass3<-DESeqDataSetFromMatrix(countData=feature_dat_Respirometry,colData = metadata_Respirometry_muscle_mass3,design=~sex+years+phase+FiberRatio+X5ADP)
    dds_X5ADP_adjusted_muscle_mass3<-DESeq(dds_X5ADP_adjusted_muscle_mass3)
    summary(dds_X5ADP_adjusted_muscle_mass3)
    res_X5ADP_adjusted_muscle_mass3<-DESeq2::results(dds_X5ADP_adjusted_muscle_mass3,alpha=0.10)
    summary(res_X5ADP_adjusted_muscle_mass3)
    
    res_ordered_X5ADP_adjusted_muscle_mass3<-res_X5ADP_adjusted_muscle_mass3[order(res_X5ADP_adjusted_muscle_mass3$pvalue),]
    res_subset_X5ADP_adjusted_muscle_mass3<-data.frame(res_ordered_X5ADP_adjusted_muscle_mass3)
    res_subset_X5ADP_adjusted_muscle_mass3<-subset(res_subset_X5ADP_adjusted_muscle_mass3,pvalue<0.01)
    #sum(rownames(res_subset_X5ADP_adjusted_muscle_mass3)%in%rownames(res_subset_X5ADP_adjusted))
    DEseq2_full_model_X5ADP_adjusted_muscle_mass3<-merge(data.frame(res_X5ADP_adjusted_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
    DEseq2_full_model_X5ADP_adjusted_muscle_mass3<-DEseq2_full_model_X5ADP_adjusted_muscle_mass3[order(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$pvalue),]
    write.csv(DEseq2_full_model_X5ADP_adjusted_muscle_mass3,"DEseq2_X5ADP_muscle_mass3.csv")
    DEseq2_full_model_X5ADP_adjusted_muscle_mass3_protein_coding<-DEseq2_full_model_X5ADP_adjusted_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    DEseq2_full_model_X5ADP_adjusted_muscle_mass3_non_coding<-DEseq2_full_model_X5ADP_adjusted_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    sum(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$padj<0.01,na.rm=TRUE)
    sum(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$padj<0.05,na.rm=TRUE)
    sum(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$padj<0.10,na.rm=TRUE)
    sum(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$pvalue<0.01,na.rm=TRUE)
    
    sum(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$pvalue<0.01&DEseq2_full_model_X5ADP_adjusted_muscle_mass3$log2FoldChange>0,na.rm=TRUE)
    sum(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$pvalue<0.01&DEseq2_full_model_X5ADP_adjusted_muscle_mass3$log2FoldChange<0,na.rm=TRUE)
    
    sum(DEseq2_full_model_X5ADP_adjusted_muscle_mass3$pvalue<0.05,na.rm=TRUE)
    DEseq2_sig_X5ADP_adjusted_muscle_mass3<-subset(DEseq2_full_model_X5ADP_adjusted_muscle_mass3,pvalue<0.01)
    res_subset_X5ADP_adjusted_muscle_mass3<-subset(res_ordered_X5ADP_adjusted_muscle_mass3,pvalue<0.01)
    res_subset_X5ADP_adjusted_muscle_mass3<-data.frame(res_subset_X5ADP_adjusted_muscle_mass3)
    DEseq2_sig_X5ADP_0.05<-subset(DEseq2_full_model_X5ADP_adjusted_muscle_mass3,pvalue<0.05)
    merged_final_X5ADP_adjusted_muscle_mass3<-merge(res_subset_X5ADP_adjusted_muscle_mass3,annot,by.x="row.names",by.y="Gene_stable_ID")
    merged_final_X5ADP_adjusted_muscle_mass3<-merged_final_X5ADP_adjusted_muscle_mass3[order(merged_final_X5ADP_adjusted_muscle_mass3$pvalue),]
    colnames(merged_final_X5ADP_adjusted_muscle_mass3)[1]<-"Gene_ID"
    merged_final_X5ADP_adjusted_muscle_mass3_protein_coding<-merged_final_X5ADP_adjusted_muscle_mass3%>%dplyr::filter(Gene_type=="protein_coding")
    merged_final_X5ADP_adjusted_muscle_mass3_non_coding<-merged_final_X5ADP_adjusted_muscle_mass3%>%dplyr::filter(Gene_type!="protein_coding")
    write.csv(merged_final_X5ADP_adjusted_muscle_mass3, file="DEseq2_X5ADP_sig_muscle_mass3.csv")
    write.csv(merged_final_X5ADP_adjusted_muscle_mass3_protein_coding, file="DEseq2_X5ADP_sig_protein_coding_muscle_mass3.csv")
    write.csv(merged_final_X5ADP_adjusted_muscle_mass3_non_coding, file="DEseq2_X5ADP_sig_non_coding_muscle_mass3.csv")
    table(merged_final_X5ADP_adjusted_muscle_mass3$Gene_type)
    
    sum(merged_final_X5ADP_adjusted_muscle_mass3_protein_coding$log2FoldChange>0,na.rm=TRUE)
    sum(merged_final_X5ADP_adjusted_muscle_mass3_protein_coding$log2FoldChange<0,na.rm=TRUE)
  }
    
# VO2 high and low ~ aging
  #Adjusted for sex, batch and Fiber Ratio
{
      metadata_VO2_high_low<-metadata_muscle_mass3
      breaks_VO2<-quantile(metadata_VO2_high_low$VO2,prob=c(0,0.33333,0.66666,1))
      breaks_VO2[4]<-breaks_VO2[4]+1
      breaks_VO2[1]<-breaks_VO2[1]-1
      metadata_VO2_high_low$VO2_cut<-cut(metadata_VO2_high_low$VO2,breaks=breaks_VO2)
      unique(metadata_VO2_high_low$VO2_cut)
      
      metadata_VO2_high<-metadata_VO2_high_low|>dplyr::filter(VO2_cut=="(30.5,48.6]")
      metadata_VO2_low<-metadata_VO2_high_low|>dplyr::filter(VO2_cut=="(7,25.2]")
      
      feature_dat_high<-feature_dat[,which(colnames(feature_dat)%in%metadata_VO2_high$name)]
      feature_dat_low<-feature_dat[,which(colnames(feature_dat)%in%metadata_VO2_low$name)]
      all(metadata_VO2_high$name==colnames(feature_dat_high))
      all(metadata_VO2_low$name==colnames(feature_dat_low))
      
      ###VO2 high
      dds_adjusted_high<-DESeqDataSetFromMatrix(countData=feature_dat_high,colData = metadata_VO2_high,design=~sex+phase+FiberRatio+years)
      dds_VO2_high<-DESeq(dds_adjusted_high)
      summary(dds_VO2_high)
      res_VO2_high<-DESeq2::results(dds_VO2_high,alpha=0.10)
      summary(res_VO2_high)
      
      res_ordered_VO2_high<-res_VO2_high[order(res_VO2_high$pvalue),]
      res_subset_VO2_high<-data.frame(res_ordered_VO2_high)
      res_subset_VO2_high<-subset(res_subset_VO2_high,pvalue<0.01)
      #sum(rownames(res_subset_VO2_high)%in%rownames(res_subset_VO2))
      DEseq2_full_model_VO2_high<-merge(data.frame(res_VO2_high),annot,by.x="row.names",by.y="Gene_stable_ID")
      DEseq2_full_model_VO2_high<-DEseq2_full_model_VO2_high[order(DEseq2_full_model_VO2_high$pvalue),]
      write.csv(DEseq2_full_model_VO2_high,"DEseq2_VO2_high.csv")
      DEseq2_full_model_VO2_high_protein_coding<-DEseq2_full_model_VO2_high%>%dplyr::filter(Gene_type=="protein_coding")
      DEseq2_full_model_VO2_high_non_coding<-DEseq2_full_model_VO2_high%>%dplyr::filter(Gene_type!="protein_coding")
      sum(DEseq2_full_model_VO2_high$padj<0.01,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_high$padj<0.05,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_high$padj<0.10,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_high$pvalue<0.01,na.rm=TRUE)
      
      sum(DEseq2_full_model_VO2_high$pvalue<0.01&DEseq2_full_model_VO2_high$log2FoldChange>0,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_high$pvalue<0.01&DEseq2_full_model_VO2_high$log2FoldChange<0,na.rm=TRUE)
      
      sum(DEseq2_full_model_VO2_high$pvalue<0.05,na.rm=TRUE)
      DEseq2_sig_VO2_high<-subset(DEseq2_full_model_VO2_high,pvalue<0.01)
      res_subset_VO2_high<-subset(res_ordered_VO2_high,pvalue<0.01)
      res_subset_VO2_high<-data.frame(res_subset_VO2_high)
      DEseq2_sig_VO2only_0.05<-subset(DEseq2_full_model_VO2_high,pvalue<0.05)
      merged_final_VO2_high<-merge(res_subset_VO2_high,annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_final_VO2_high<-merged_final_VO2_high[order(merged_final_VO2_high$pvalue),]
      colnames(merged_final_VO2_high)[1]<-"Gene_ID"
      merged_final_VO2_high_protein_coding<-merged_final_VO2_high%>%dplyr::filter(Gene_type=="protein_coding")
      merged_final_VO2_high_non_coding<-merged_final_VO2_high%>%dplyr::filter(Gene_type!="protein_coding")
      write.csv(merged_final_VO2_high, file="DEseq2_VO2_sig_high.csv")
      write.csv(merged_final_VO2_high_protein_coding, file="DEseq2_VO2_sig_protein_coding_high.csv")
      write.csv(merged_final_VO2_high_non_coding, file="DEseq2_VO2_sig_non_coding_high.csv")
      table(merged_final_VO2_high$Gene_type)
      ##VO2 low
      #Again adjusted for sex, phase and Fiber Ratio
      dds_adjusted_low<-DESeqDataSetFromMatrix(countData=feature_dat_low,colData = metadata_VO2_low,design=~sex+phase+FiberRatio+years)
      dds_VO2_low<-DESeq(dds_adjusted_low)
      summary(dds_VO2_low)
      res_VO2_low<-DESeq2::results(dds_VO2_low,alpha=0.10)
      summary(res_VO2_low)
      
      res_ordered_VO2_low<-res_VO2_low[order(res_VO2_low$pvalue),]
      res_subset_VO2_low<-data.frame(res_ordered_VO2_low)
      res_subset_VO2_low<-subset(res_subset_VO2_low,pvalue<0.01)
      #sum(rownames(res_subset_VO2_low)%in%rownames(res_subset_VO2))
      DEseq2_full_model_VO2_low<-merge(data.frame(res_VO2_low),annot,by.x="row.names",by.y="Gene_stable_ID")
      DEseq2_full_model_VO2_low<-DEseq2_full_model_VO2_low[order(DEseq2_full_model_VO2_low$pvalue),]
      write.csv(DEseq2_full_model_VO2_low,"DEseq2_VO2_low.csv")
      DEseq2_full_model_VO2_low_protein_coding<-DEseq2_full_model_VO2_low%>%dplyr::filter(Gene_type=="protein_coding")
      DEseq2_full_model_VO2_low_non_coding<-DEseq2_full_model_VO2_low%>%dplyr::filter(Gene_type!="protein_coding")
      sum(DEseq2_full_model_VO2_low$padj<0.01,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_low$padj<0.05,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_low$padj<0.10,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_low$pvalue<0.01,na.rm=TRUE)
      
      sum(DEseq2_full_model_VO2_low$pvalue<0.01&DEseq2_full_model_VO2_low$log2FoldChange>0,na.rm=TRUE)
      sum(DEseq2_full_model_VO2_low$pvalue<0.01&DEseq2_full_model_VO2_low$log2FoldChange<0,na.rm=TRUE)
      
      sum(DEseq2_full_model_VO2_low$pvalue<0.05,na.rm=TRUE)
      DEseq2_sig_VO2_low<-subset(DEseq2_full_model_VO2_low,pvalue<0.01)
      res_subset_VO2_low<-subset(res_ordered_VO2_low,pvalue<0.01)
      res_subset_VO2_low<-data.frame(res_subset_VO2_low)
      DEseq2_sig_VO2only_0.05<-subset(DEseq2_full_model_VO2_low,pvalue<0.05)
      merged_final_VO2_low<-merge(res_subset_VO2_low,annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_final_VO2_low<-merged_final_VO2_low[order(merged_final_VO2_low$pvalue),]
      colnames(merged_final_VO2_low)[1]<-"Gene_ID"
      merged_final_VO2_low_protein_coding<-merged_final_VO2_low%>%dplyr::filter(Gene_type=="protein_coding")
      merged_final_VO2_low_non_coding<-merged_final_VO2_low%>%dplyr::filter(Gene_type!="protein_coding")
      write.csv(merged_final_VO2_low, file="DEseq2_VO2_sig_low.csv")
      write.csv(merged_final_VO2_low_protein_coding, file="DEseq2_VO2_sig_protein_coding_low.csv")
      write.csv(merged_final_VO2_low_non_coding, file="DEseq2_VO2_sig_non_coding_low.csv")
      table(merged_final_VO2_low$Gene_type)
      
      
      
    }
}

# Supplementary Figure S9: Mito-splicing volcano (including fig. 5c and 5d)
{
   #highxmin  
  ##We use 2 databases: Mitocarta and spliceosome database
  {
    ##Full Path edited out.
    mitocarta<-read.csv("human_mitocarta3.csv")
    splicing<-read.csv("Spliceosome_database_human.csv")
    mitocarta<-mitocarta[!mitocarta$EnsemblGeneID_mapping_version_20200130%in%splicing$Ensemble.Gene.ID,]
    splicing<-splicing[!splicing$Ensemble.Gene.ID%in%mitocarta$EnsemblGeneID_mapping_version_20200130,]
    
    
    full_model_volcano<-DEseq2_full_model_highxmin_adjusted_muscle_mass3_protein_coding
    full_model_volcano$pvalue<-(-log10(full_model_volcano$pvalue))
    mitocarta_highxmin<-full_model_volcano[which(full_model_volcano$Row.names%in%mitocarta$EnsemblGeneID_mapping_version_20200130),]
    mitocarta_highxmin$category<-"Mito"
    splicing_highxmin<-full_model_volcano[which(full_model_volcano$Row.names%in%splicing$Ensemble.Gene.ID),]
    splicing_highxmin$category<-"Splicing"
    mito_splic_highxmin<-rbind(mitocarta_highxmin,splicing_highxmin)
    mito_splic_highxmin<-mito_splic_highxmin[which(mito_splic_highxmin$pvalue>2),]
    
    highxmin_volc <- full_model_volcano |>
      ggplot(aes(x = log2FoldChange, y = pvalue)) +
      geom_point(col = "darkgrey", alpha = 0.5, size = 0.8) +
      theme_classic() +
      geom_point(data = mito_splic_highxmin, aes(x = log2FoldChange, y = pvalue, col = category), size = 1) +
      geom_text_repel(data = mito_splic_highxmin, aes(label = Gene_name, color = category), size = 3.5, max.overlaps = 30, segment.color = "black", segment.alpha = 0.3) +
      labs(x = "Beta PA", y = expression("-Log"["10"] ~ "P"), color = "Categories") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      scale_color_manual(values = c("chocolate3", "limegreen"), labels = c("Mito", "Splicing"))+ ylim(0,5)+ xlim(-0.003,0.003)
    
    highxmin_volc
    
    
  }
  
  #VO2
  {
    #Full Path removed for security reasons.
    mitocarta<-read.csv("human_mitocarta3.csv")
    splicing<-read.csv("Spliceosome_database_human.csv")
    mitocarta<-mitocarta[!mitocarta$EnsemblGeneID_mapping_version_20200130%in%splicing$Ensemble.Gene.ID,]
    splicing<-splicing[!splicing$Ensemble.Gene.ID%in%mitocarta$EnsemblGeneID_mapping_version_20200130,]
    
    
    full_model_volcano<-DEseq2_full_model_VO2_muscle_mass3_protein_coding
    full_model_volcano$pvalue<-(-log10(full_model_volcano$pvalue))
    mitocarta_VO2<-full_model_volcano[which(full_model_volcano$Row.names%in%mitocarta$EnsemblGeneID_mapping_version_20200130),]
    mitocarta_VO2$category<-"Mito"
    splicing_VO2<-full_model_volcano[which(full_model_volcano$Row.names%in%splicing$Ensemble.Gene.ID),]
    splicing_VO2$category<-"Splicing"
    mito_splic_VO2<-rbind(mitocarta_VO2,splicing_VO2)
    mito_splic_VO2<-mito_splic_VO2[which(mito_splic_VO2$pvalue>2),]
    
    full_model_volcano|>ggplot(aes(x=log2FoldChange,y=pvalue))+
      geom_point(col="grey",alpha=0.4,size=0.8)+theme_classic()+
      geom_point(data=mito_splic_VO2,aes(x=log2FoldChange,y=pvalue,col=category),size=1)+
      geom_text_repel(data=mito_splic_VO2,aes(label=Gene_name))+
      labs("x"="ß VO2",y=expression("-Log"["10"]~"P"),"color"="Categories")+
      guides(color = guide_legend(override.aes = list(size = 3)))+
      scale_color_manual(values=c("red","blue"),labels=c("Mito","Splicing"))
    
    
    VO2_volc <- full_model_volcano |>
      ggplot(aes(x = log2FoldChange, y = pvalue)) +
      geom_point(col = "darkgrey", alpha = 0.5, size = 0.8) +
      theme_classic() +
      geom_point(data = mito_splic_VO2, aes(x = log2FoldChange, y = pvalue, col = category), size = 1) +
      geom_text_repel(data = mito_splic_VO2, aes(label = Gene_name, color = category), size = 3.5, max.overlaps = 30, segment.color = "black", segment.alpha = 0.3) +
      labs(x = "Beta VO2", y = expression("-Log"["10"] ~ "P"), color = "Categories") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      scale_color_manual(values = c("chocolate3", "limegreen"), labels = c("Mito", "Splicing"))+ xlim(-0.1,0.1)+ ylim(0,5)
    
    VO2_volc
  }
  

  ##kPCr splicing 
  
  {
    mitocarta<-read.csv("human_mitocarta3.csv")
    splicing<-read.csv("Spliceosome_database_human.csv")
    mitocarta<-mitocarta[!mitocarta$EnsemblGeneID_mapping_version_20200130%in%splicing$Ensemble.Gene.ID,]
    splicing<-splicing[!splicing$Ensemble.Gene.ID%in%mitocarta$EnsemblGeneID_mapping_version_20200130,]
    
    
    full_model_volcano<-DEseq2_full_model_kPCr_adjusted_muscle_mass3_protein_coding
    full_model_volcano$pvalue<-(-log10(full_model_volcano$pvalue))
    mitocarta_kPCr<-full_model_volcano[which(full_model_volcano$Row.names%in%mitocarta$EnsemblGeneID_mapping_version_20200130),]
    mitocarta_kPCr$category<-"Mito"
    splicing_kPCr<-full_model_volcano[which(full_model_volcano$Row.names%in%splicing$Ensemble.Gene.ID),]
    splicing_kPCr$category<-"Splicing"
    mito_splic_kPCr<-rbind(mitocarta_kPCr,splicing_kPCr)
    mito_splic_kPCr<-mito_splic_kPCr[which(mito_splic_kPCr$pvalue>2),]
    
    kPCr_volc <- full_model_volcano |>
      ggplot(aes(x = log2FoldChange, y = pvalue)) +
      geom_point(col = "darkgrey", alpha = 0.5, size = 0.8) +
      theme_classic() +
      geom_point(data = mito_splic_kPCr, aes(x = log2FoldChange, y = pvalue, col = category), size = 1) +
      geom_text_repel(data = mito_splic_kPCr, aes(label = Gene_name, color = category), size = 3.5, max.overlaps = 30, segment.color = "black", segment.alpha = 0.3) +
      labs(x = "Beta kPCr", y = expression("-Log"["10"] ~ "P"), color = "Categories") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      scale_color_manual(values = c("chocolate3", "limegreen"), labels = c("Mito", "Splicing"))+ ylim(0,5)+ xlim(-60,60)
    
    kPCr_volc
    
    }  
   
  
  ##X5ADP splicing 
  
  {
    mitocarta<-read.csv("human_mitocarta3.csv")
    splicing<-read.csv("Spliceosome_database_human.csv")
    mitocarta<-mitocarta[!mitocarta$EnsemblGeneID_mapping_version_20200130%in%splicing$Ensemble.Gene.ID,]
    splicing<-splicing[!splicing$Ensemble.Gene.ID%in%mitocarta$EnsemblGeneID_mapping_version_20200130,]
    
    
    full_model_volcano<-DEseq2_full_model_X5ADP_adjusted_muscle_mass3_protein_coding
    full_model_volcano$pvalue<-(-log10(full_model_volcano$pvalue))
    mitocarta_X5ADP<-full_model_volcano[which(full_model_volcano$Row.names%in%mitocarta$EnsemblGeneID_mapping_version_20200130),]
    mitocarta_X5ADP$category<-"Mito"
    splicing_X5ADP<-full_model_volcano[which(full_model_volcano$Row.names%in%splicing$Ensemble.Gene.ID),]
    splicing_X5ADP$category<-"Splicing"
    mito_splic_X5ADP<-rbind(mitocarta_X5ADP,splicing_X5ADP)
    mito_splic_X5ADP<-mito_splic_X5ADP[which(mito_splic_X5ADP$pvalue>2),]
    
    X5ADP_volc <- full_model_volcano |>
      ggplot(aes(x = log2FoldChange, y = pvalue)) +
      geom_point(col = "darkgrey", alpha = 0.5, size = 0.8) +
      theme_classic() +
      geom_point(data = mito_splic_X5ADP, aes(x = log2FoldChange, y = pvalue, col = category), size = 1) +
      geom_text_repel(data = mito_splic_X5ADP, aes(label = Gene_name, color = category), size = 3.5, max.overlaps = 30, segment.color = "black", segment.alpha = 0.3) +
      labs(x = "Beta Mit-O2flux", y = expression("-Log"["10"] ~ "P"), color = "Categories") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      scale_color_manual(values = c("chocolate3", "limegreen"), labels = c("Mito", "Splicing"))+ ylim(0,5)+ xlim(-0.08,0.08)
    
    X5ADP_volc
    
    
  }   
  
  ##High Vo2
  #Volcano Plot
  {
    mitocarta<-read.csv("human_mitocarta3.csv")
    splicing<-read.csv("Spliceosome_database_human.csv")
    mitocarta<-mitocarta[!mitocarta$EnsemblGeneID_mapping_version_20200130%in%splicing$Ensemble.Gene.ID,]
    splicing<-splicing[!splicing$Ensemble.Gene.ID%in%mitocarta$EnsemblGeneID_mapping_version_20200130,]
    
    
    full_model_volcano<-DEseq2_full_model_VO2_high
    full_model_volcano$pvalue<-(-log10(full_model_volcano$pvalue))
    mitocarta_VO2<-full_model_volcano[which(full_model_volcano$Row.names%in%mitocarta$EnsemblGeneID_mapping_version_20200130),]
    mitocarta_VO2$category<-"Mito"
    splicing_VO2<-full_model_volcano[which(full_model_volcano$Row.names%in%splicing$Ensemble.Gene.ID),]
    splicing_VO2$category<-"Splicing"
    mito_splic_VO2<-rbind(mitocarta_VO2,splicing_VO2)
    mito_splic_VO2<-mito_splic_VO2[which(mito_splic_VO2$pvalue>(-log10(0.05))),]
    full_model_volcano|>ggplot(aes(x=log2FoldChange,y=pvalue))+
      geom_point(col="grey",alpha=0.4,size=0.8)+theme_classic()+
      geom_point(data=mito_splic_VO2,aes(x=log2FoldChange,y=pvalue,col=category),size=1)+
      geom_text_repel(data=mito_splic_VO2,aes(label=Gene_name))+
      labs("x"="ß VO2",y=expression("-Log"["10"]~"P"),"color"="Categories")+
      guides(color = guide_legend(override.aes = list(size = 3)))+
      scale_color_manual(values=c("red","blue"),labels=c("Mito","Splicing"))+
      xlim(-0.25,0.25)
    
   high_volc<-full_model_volcano |>
      ggplot(aes(x = log2FoldChange, y = pvalue)) +
      geom_point(col = "darkgrey", alpha = 0.5, size = 0.8) +
      theme_classic() +
      geom_point(data = mito_splic_VO2, aes(x = log2FoldChange, y = pvalue, col = category), size = 1) +
      geom_text_repel(data = mito_splic_VO2, aes(label = Gene_name, color = category), size = 3.5, max.overlaps = 30, segment.color = "black", segment.alpha = 0.3) +
      labs(x = "Beta Age (High VO2 cohort)", y = expression("-Log"["10"] ~ "P"), color = "Categories") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      scale_color_manual(values = c("chocolate3", "limegreen"), labels = c("Mito", "Splicing"))+ xlim(-0.1,0.1)+ ylim(0,4.5)
    
    
    
    
  }
  
  ##Low Vo2
  {
    mitocarta<-read.csv("human_mitocarta3.csv")
    splicing<-read.csv("Spliceosome_database_human.csv")
    mitocarta<-mitocarta[!mitocarta$EnsemblGeneID_mapping_version_20200130%in%splicing$Ensemble.Gene.ID,]
    splicing<-splicing[!splicing$Ensemble.Gene.ID%in%mitocarta$EnsemblGeneID_mapping_version_20200130,]
    
    
    full_model_volcano<-DEseq2_full_model_VO2_low
    full_model_volcano$pvalue<-(-log10(full_model_volcano$pvalue))
    mitocarta_VO2<-full_model_volcano[which(full_model_volcano$Row.names%in%mitocarta$EnsemblGeneID_mapping_version_20200130),]
    mitocarta_VO2$category<-"Mito"
    splicing_VO2<-full_model_volcano[which(full_model_volcano$Row.names%in%splicing$Ensemble.Gene.ID),]
    splicing_VO2$category<-"Splicing"
    mito_splic_VO2<-rbind(mitocarta_VO2,splicing_VO2)
    #Change from 2 to 1 for more.
    mito_splic_VO2<-mito_splic_VO2[which(mito_splic_VO2$pvalue>(-log10(0.05))),]
    full_model_volcano|>ggplot(aes(x=log2FoldChange,y=pvalue))+
      geom_point(col="grey",alpha=0.4,size=0.8)+theme_classic()+
      geom_point(data=mito_splic_VO2,aes(x=log2FoldChange,y=pvalue,col=category),size=1)+
      geom_text_repel(data=mito_splic_VO2,aes(label=Gene_name))+
      labs("x"="ß VO2",y=expression("-Log"["10"]~"P"),"color"="Categories")+
      guides(color = guide_legend(override.aes = list(size = 3)))+
      scale_color_manual(values=c("red","blue"),labels=c("Mito","Splicing"))
    
    
  low_volc<- full_model_volcano |>
      ggplot(aes(x = log2FoldChange, y = pvalue)) +
      geom_point(col = "darkgrey", alpha = 0.5, size = 0.8) +
      theme_classic() +
      geom_point(data = mito_splic_VO2, aes(x = log2FoldChange, y = pvalue, col = category), size = 1) +
      geom_text_repel(data = mito_splic_VO2, aes(label = Gene_name, color = category), size = 3.5, max.overlaps = 30, segment.color = "black", segment.alpha = 0.3) +
      labs(x = "Beta Age (Low VO2 cohort)", y = expression("-Log"["10"] ~ "P"), color = "Categories") +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      scale_color_manual(values = c("chocolate3", "limegreen"), labels = c("Mito", "Splicing"))+ xlim(-0.1,0.1)+ ylim(0,4.5)
    
    
  }  
    (low_volc|high_volc) + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 15))

    (highxmin_volc|VO2_volc|kPCr_volc|X5ADP_volc) + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 15))
    
  }
  
# Figure 2: Heatmap
{
#Here we use the pheatmap package.
#Z-score normalization performed.
#Grouped into tertile.  This applies to all heatmaps for this figure.

# highxmin
{
filter_new_highxmin_adjusted_protein_coding_up<-merged_final_highxmin_adjusted_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange>0)|>head(15)
    filter_new_highxmin_adjusted_protein_coding_down<-merged_final_highxmin_adjusted_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange<0)|>head(15)
    
    filter_new_highxmin_adjusted_protein_coding<-rbind(filter_new_highxmin_adjusted_protein_coding_up,
                                                       filter_new_highxmin_adjusted_protein_coding_down)
    #filter_new_highxmin_adjusted_protein_coding<-merged_final_highxmin_adjusted_muscle_mass3_protein_coding[1:30,]
    z_score_normalized_cpm_highxmin_adjusted_protein_coding<-t(scale(t(cpm_data_highxmin_adjusted)))
    idx<-which(rownames(z_score_normalized_cpm_highxmin_adjusted_protein_coding)%in%filter_new_highxmin_adjusted_protein_coding$Gene_ID)
    cpm_new_heatmap_highxmin_adjusted_protein_coding<-z_score_normalized_cpm_highxmin_adjusted_protein_coding[idx,]
    number<-as.numeric(gsub("MUSCLE_AGE(\\d+).*",'\\1',colnames(cpm_new_heatmap_highxmin_adjusted_protein_coding)))
    cpm_new_heatmap_highxmin_adjusted_protein_coding<-cpm_new_heatmap_highxmin_adjusted_protein_coding[,order(number)]
    cpm_new_heatmap_highxmin_adjusted_protein_coding<-data.frame(cpm_new_heatmap_highxmin_adjusted_protein_coding,check.names=F)
    cpm_new_heatmap_highxmin_adjusted_protein_coding$gene<-rownames(cpm_new_heatmap_highxmin_adjusted_protein_coding)
    cpm_new_long_highxmin_adjusted_protein_coding<-pivot_longer(data.frame(cpm_new_heatmap_highxmin_adjusted_protein_coding),cols=contains("MUSCLE"))
    cpm_new_long_highxmin_adjusted_protein_coding<-merge(cpm_new_long_highxmin_adjusted_protein_coding,metadata_muscle_mass3,by="name")
    head(cpm_new_long_highxmin_adjusted_protein_coding)
    cpm_new_long_highxmin_adjusted_protein_coding$years<-as.numeric(gsub("MUSCLE_AGE(\\d+).*","\\1",cpm_new_long_highxmin_adjusted_protein_coding$name))
    breaks_highxmin<-quantile(metadata$highxmin,prob=c(0,0.33333,0.66666,1))
    breaks_highxmin[4]<-breaks_highxmin[4]+1
    breaks_highxmin[1]<-breaks_highxmin[1]-1
    cpm_new_long_highxmin_adjusted_protein_coding$age<-cut(cpm_new_long_highxmin_adjusted_protein_coding$years,breaks=c(20,30,40,50,60,70,80,90))
    cpm_new_long_highxmin_adjusted_protein_coding$highxmin_cut<-cut(cpm_new_long_highxmin_adjusted_protein_coding$highxmin,breaks=breaks_highxmin)
    cpm_new_long_highxmin_adjusted_protein_coding$highxmin_cut<-factor(cpm_new_long_highxmin_adjusted_protein_coding$highxmin_cut)
    #levels(cpm_new_long_highxmin_adjusted_protein_coding$highxmin_cut)<-c("Low","Mid","High")
    levels(cpm_new_long_highxmin_adjusted_protein_coding$highxmin_cut)<-c("0-100 (min/week)","100-300 (min/week)","300-930 (min/week)")
    cpm_new_long2_highxmin_adjusted_protein_coding<-cpm_new_long_highxmin_adjusted_protein_coding%>%group_by(highxmin_cut,gene)%>%summarize(mean(value))
    colnames(cpm_new_long2_highxmin_adjusted_protein_coding)[3]<-"value"
    cpm_wide_highxmin_adjusted_protein_coding<-cpm_new_long2_highxmin_adjusted_protein_coding%>%pivot_wider(names_from=highxmin_cut)
    cpm_wide_highxmin_adjusted_protein_coding<-data.frame(cpm_wide_highxmin_adjusted_protein_coding,check.names=F)
    rownames(cpm_wide_highxmin_adjusted_protein_coding)<-cpm_wide_highxmin_adjusted_protein_coding$gene
    cpm_wide_merged_highxmin_adjusted_protein_coding<-merge(annot,cpm_wide_highxmin_adjusted_protein_coding,by.x="Gene_stable_ID",by.y="row.names")
    #idx<-which(cpm_wide_merged_highxmin_adjusted_protein_coding$Gene_name=="")
    rownames(cpm_wide_merged_highxmin_adjusted_protein_coding)[-idx]<-cpm_wide_merged_highxmin_adjusted_protein_coding$Gene_name
    #rownames(cpm_wide_merged_highxmin_adjusted_protein_coding)[idx]<-cpm_wide_merged_highxmin_adjusted_protein_coding$Gene_stable_ID[idx]
    cpm_wide_merged_highxmin_adjusted_protein_coding<-cpm_wide_merged_highxmin_adjusted_protein_coding[,13:16]
    cpm_wide_merged_highxmin_adjusted_protein_coding$gene<-NULL
    library(pheatmap)
    #pheatmap(cpm_wide_merged_highxmin_adjusted_protein_coding,scale="none",cluster_cols=FALSE)
    #pheatmap(t(cpm_wide_merged_highxmin_adjusted_protein_coding),scale="none",cluster_cols=TRUE,cluster_rows = FALSE)
    highxmin_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap((cpm_wide_merged_highxmin_adjusted_protein_coding),scale="none",cluster_cols=FALSE))
    ##I add it right below
    
    cpm_wide_merged_highxmin_adjusted_protein_coding_subset<-cpm_wide_merged_highxmin_adjusted_protein_coding[,c(1,3)]
    colnames(cpm_wide_merged_highxmin_adjusted_protein_coding_subset)<-c("Low PA","High PA")
    highxmin_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap(cpm_wide_merged_highxmin_adjusted_protein_coding_subset,scale="none",cluster_cols=FALSE))
}

# VO2
  ##See comments above the highxmin script.
{
filter_new_VO2_adjusted_protein_coding_up<-merged_final_VO2_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange>0)|>head(15)
    filter_new_VO2_adjusted_protein_coding_down<-merged_final_VO2_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange<0)|>head(15)
    
    filter_new_VO2_adjusted_protein_coding<-rbind(filter_new_VO2_adjusted_protein_coding_up,
                                                       filter_new_VO2_adjusted_protein_coding_down)

    #filter_new_VO2_adjusted_protein_coding<-merged_final_VO2_muscle_mass3_protein_coding[1:30,]
    z_score_normalized_cpm_VO2_adjusted_protein_coding<-t(scale(t(cpm_data_VO2_adjusted)))
    idx<-which(rownames(z_score_normalized_cpm_VO2_adjusted_protein_coding)%in%filter_new_VO2_adjusted_protein_coding$Gene_ID)
    cpm_new_heatmap_VO2_adjusted_protein_coding<-z_score_normalized_cpm_VO2_adjusted_protein_coding[idx,]
    number<-as.numeric(gsub("MUSCLE_AGE(\\d+).*",'\\1',colnames(cpm_new_heatmap_VO2_adjusted_protein_coding)))
    cpm_new_heatmap_VO2_adjusted_protein_coding<-cpm_new_heatmap_VO2_adjusted_protein_coding[,order(number)]
    cpm_new_heatmap_VO2_adjusted_protein_coding<-data.frame(cpm_new_heatmap_VO2_adjusted_protein_coding,check.names=F)
    cpm_new_heatmap_VO2_adjusted_protein_coding$gene<-rownames(cpm_new_heatmap_VO2_adjusted_protein_coding)
    cpm_new_long_VO2_adjusted_protein_coding<-pivot_longer(data.frame(cpm_new_heatmap_VO2_adjusted_protein_coding),cols=contains("MUSCLE"))
    cpm_new_long_VO2_adjusted_protein_coding<-merge(cpm_new_long_VO2_adjusted_protein_coding,metadata_muscle_mass3,by="name")
    head(cpm_new_long_VO2_adjusted_protein_coding)
    cpm_new_long_VO2_adjusted_protein_coding$years<-as.numeric(gsub("MUSCLE_AGE(\\d+).*","\\1",cpm_new_long_VO2_adjusted_protein_coding$name))
    breaks_VO2<-quantile(metadata_muscle_mass3$VO2,prob=c(0,0.33333,0.66666,1))
    breaks_VO2[4]<-breaks_VO2[4]+1
    breaks_VO2[1]<-breaks_VO2[1]-1
    cpm_new_long_VO2_adjusted_protein_coding$age<-cut(cpm_new_long_VO2_adjusted_protein_coding$years,breaks=c(20,30,40,50,60,70,80,90))
    cpm_new_long_VO2_adjusted_protein_coding$VO2_cut<-cut(cpm_new_long_VO2_adjusted_protein_coding$VO2,breaks=breaks_VO2)
    cpm_new_long_VO2_adjusted_protein_coding$VO2_cut<-factor(cpm_new_long_VO2_adjusted_protein_coding$VO2_cut)
    #levels(cpm_new_long_VO2_adjusted_protein_coding$VO2_cut)<-c("Low","Mid","High")
    levels(cpm_new_long_VO2_adjusted_protein_coding$VO2_cut)<-c("8-25.2 (mL/kg/min)","25.2-30.5 (mL/kg/min)","30.5-47.6 (mL/kg/min)")
    cpm_new_long2_VO2_adjusted_protein_coding<-cpm_new_long_VO2_adjusted_protein_coding%>%group_by(VO2_cut,gene)%>%summarize(mean(value))
    colnames(cpm_new_long2_VO2_adjusted_protein_coding)[3]<-"value"
    cpm_wide_VO2_adjusted_protein_coding<-cpm_new_long2_VO2_adjusted_protein_coding%>%pivot_wider(names_from=VO2_cut)
    cpm_wide_VO2_adjusted_protein_coding<-data.frame(cpm_wide_VO2_adjusted_protein_coding,check.names=F)
    rownames(cpm_wide_VO2_adjusted_protein_coding)<-cpm_wide_VO2_adjusted_protein_coding$gene
    cpm_wide_merged_VO2_adjusted_protein_coding<-merge(annot,cpm_wide_VO2_adjusted_protein_coding,by.x="Gene_stable_ID",by.y="row.names")
    #idx<-which(cpm_wide_merged_VO2_adjusted_protein_coding$Gene_name=="")
    rownames(cpm_wide_merged_VO2_adjusted_protein_coding)[-idx]<-cpm_wide_merged_VO2_adjusted_protein_coding$Gene_name
    #rownames(cpm_wide_merged_VO2_adjusted_protein_coding)[idx]<-cpm_wide_merged_VO2_adjusted_protein_coding$Gene_stable_ID[idx]
    cpm_wide_merged_VO2_adjusted_protein_coding<-cpm_wide_merged_VO2_adjusted_protein_coding[,13:16]
    cpm_wide_merged_VO2_adjusted_protein_coding$gene<-NULL
    library(pheatmap)
    #pheatmap(cpm_wide_merged_VO2_adjusted_protein_coding,scale="none",cluster_cols=FALSE)
    #pheatmap(t(cpm_wide_merged_VO2_adjusted_protein_coding),scale="none",cluster_cols=TRUE,cluster_rows = FALSE)
    VO2_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap((cpm_wide_merged_VO2_adjusted_protein_coding),scale="none",cluster_cols=FALSE))
    ##Code below
    cpm_wide_merged_VO2_adjusted_protein_coding_subset<-cpm_wide_merged_VO2_adjusted_protein_coding[,c(1,3)]
    
    colnames(cpm_wide_merged_VO2_adjusted_protein_coding_subset)<-c("Low VO2","High VO2")
    VO2_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap(cpm_wide_merged_VO2_adjusted_protein_coding_subset,scale="none",cluster_cols=FALSE))
    
    #(highxmin_vol_protein_coding|highxmin_heatmap_protein_coding)/(VO2_vol_protein_coding|VO2_heatmap_protein_coding)
  }
  
# kPCr
{ 
  ##See comments above the highxmin script.
    filter_new_kPCr_adjusted_protein_coding_up<-merged_final_kPCr_adjusted_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange>0)|>head(15)
    filter_new_kPCr_adjusted_protein_coding_down<-merged_final_kPCr_adjusted_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange<0)|>head(15)
    
    filter_new_kPCr_adjusted_protein_coding<-rbind(filter_new_kPCr_adjusted_protein_coding_up,
                                                   filter_new_kPCr_adjusted_protein_coding_down)
    
    #filter_new_kPCr_adjusted_protein_coding<-merged_final_kPCr_adjusted_muscle_mass3_protein_coding[1:30,]
    z_score_normalized_cpm_kPCr_adjusted_protein_coding<-t(scale(t(cpm_data_kPCr_adjusted)))
    idx<-which(rownames(z_score_normalized_cpm_kPCr_adjusted_protein_coding)%in%filter_new_kPCr_adjusted_protein_coding$Gene_ID)
    cpm_new_heatmap_kPCr_adjusted_protein_coding<-z_score_normalized_cpm_kPCr_adjusted_protein_coding[idx,]
    number<-as.numeric(gsub("MUSCLE_AGE(\\d+).*",'\\1',colnames(cpm_new_heatmap_kPCr_adjusted_protein_coding)))
    cpm_new_heatmap_kPCr_adjusted_protein_coding<-cpm_new_heatmap_kPCr_adjusted_protein_coding[,order(number)]
    cpm_new_heatmap_kPCr_adjusted_protein_coding<-data.frame(cpm_new_heatmap_kPCr_adjusted_protein_coding,check.names=F)
    cpm_new_heatmap_kPCr_adjusted_protein_coding$gene<-rownames(cpm_new_heatmap_kPCr_adjusted_protein_coding)
    cpm_new_long_kPCr_adjusted_protein_coding<-pivot_longer(data.frame(cpm_new_heatmap_kPCr_adjusted_protein_coding),cols=contains("MUSCLE"))
    cpm_new_long_kPCr_adjusted_protein_coding<-merge(cpm_new_long_kPCr_adjusted_protein_coding,metadata_muscle_mass3,by="name")
    head(cpm_new_long_kPCr_adjusted_protein_coding)
    cpm_new_long_kPCr_adjusted_protein_coding$years<-as.numeric(gsub("MUSCLE_AGE(\\d+).*","\\1",cpm_new_long_kPCr_adjusted_protein_coding$name))
    breaks_kPCr<-quantile(metadata_muscle_mass3$kPCr,prob=c(0,0.33333,0.66666,1))
    breaks_kPCr[4]<-breaks_kPCr[4]+1
    breaks_kPCr[1]<-breaks_kPCr[1]-1
    cpm_new_long_kPCr_adjusted_protein_coding$age<-cut(cpm_new_long_kPCr_adjusted_protein_coding$years,breaks=c(20,30,40,50,60,70,80,90))
    cpm_new_long_kPCr_adjusted_protein_coding$kPCr_cut<-cut(cpm_new_long_kPCr_adjusted_protein_coding$kPCr,breaks=breaks_kPCr)
    cpm_new_long_kPCr_adjusted_protein_coding$kPCr_cut<-factor(cpm_new_long_kPCr_adjusted_protein_coding$kPCr_cut)
    #levels(cpm_new_long_kPCr_adjusted_protein_coding$kPCr_cut)<-c("Low kPCr","Mid kPCr","High kPCr")
    levels(cpm_new_long_kPCr_adjusted_protein_coding$kPCr_cut)<-c("0.014-0.0195 (/s)","0.0195-0.0249 (/s)","0.0249-0.043 (/s)")
    cpm_new_long2_kPCr_adjusted_protein_coding<-cpm_new_long_kPCr_adjusted_protein_coding%>%group_by(kPCr_cut,gene)%>%summarize(mean(value))
    colnames(cpm_new_long2_kPCr_adjusted_protein_coding)[3]<-"value"
    cpm_wide_kPCr_adjusted_protein_coding<-cpm_new_long2_kPCr_adjusted_protein_coding%>%pivot_wider(names_from=kPCr_cut)
    cpm_wide_kPCr_adjusted_protein_coding<-data.frame(cpm_wide_kPCr_adjusted_protein_coding,check.names=F)
    rownames(cpm_wide_kPCr_adjusted_protein_coding)<-cpm_wide_kPCr_adjusted_protein_coding$gene
    cpm_wide_merged_kPCr_adjusted_protein_coding<-merge(annot,cpm_wide_kPCr_adjusted_protein_coding,by.x="Gene_stable_ID",by.y="row.names")
    #idx<-which(cpm_wide_merged_kPCr_adjusted_protein_coding$Gene_name=="")
    rownames(cpm_wide_merged_kPCr_adjusted_protein_coding)[-idx]<-cpm_wide_merged_kPCr_adjusted_protein_coding$Gene_name
    #rownames(cpm_wide_merged_kPCr_adjusted_protein_coding)[idx]<-cpm_wide_merged_kPCr_adjusted_protein_coding$Gene_stable_ID[idx]
    cpm_wide_merged_kPCr_adjusted_protein_coding<-cpm_wide_merged_kPCr_adjusted_protein_coding[,13:16]
    cpm_wide_merged_kPCr_adjusted_protein_coding$gene<-NULL
    library(pheatmap)
    #pheatmap(cpm_wide_merged_kPCr_adjusted_protein_coding,scale="none",cluster_cols=FALSE)
    #pheatmap(t(cpm_wide_merged_kPCr_adjusted_protein_coding),scale="none",cluster_cols=TRUE,cluster_rows = FALSE)
    
    kPCr_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap((cpm_wide_merged_kPCr_adjusted_protein_coding),scale="none",cluster_cols=FALSE))
    
    cpm_wide_merged_kPCr_adjusted_protein_coding_subset<-cpm_wide_merged_kPCr_adjusted_protein_coding[,c(1,3)]
    
    colnames(cpm_wide_merged_kPCr_adjusted_protein_coding_subset)<-c("Low kPCr","High kPCr")
    
    kPCr_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap(cpm_wide_merged_kPCr_adjusted_protein_coding_subset,scale="none",cluster_cols=FALSE))
    
    
    
    
}  
  
# X5ADP (Respirometry)
{
  ##See comments above the highxmin script.
    filter_new_X5ADP_adjusted_protein_coding_up<-merged_final_X5ADP_adjusted_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange>0)|>head(15)
    filter_new_X5ADP_adjusted_protein_coding_down<-merged_final_X5ADP_adjusted_muscle_mass3_protein_coding|>
      dplyr::filter(log2FoldChange<0)|>head(15)
    
    filter_new_X5ADP_adjusted_protein_coding<-rbind(filter_new_X5ADP_adjusted_protein_coding_up,
                                                    filter_new_X5ADP_adjusted_protein_coding_down)
    
    #filter_new_X5ADP_adjusted_protein_coding<-merged_final_X5ADP_adjusted_muscle_mass3_protein_coding[1:30,]
    z_score_normalized_cpm_X5ADP_adjusted_protein_coding<-t(scale(t(cpm_data_X5ADP_adjusted)))
    idx<-which(rownames(z_score_normalized_cpm_X5ADP_adjusted_protein_coding)%in%filter_new_X5ADP_adjusted_protein_coding$Gene_ID)
    cpm_new_heatmap_X5ADP_adjusted_protein_coding<-z_score_normalized_cpm_X5ADP_adjusted_protein_coding[idx,]
    number<-as.numeric(gsub("MUSCLE_AGE(\\d+).*",'\\1',colnames(cpm_new_heatmap_X5ADP_adjusted_protein_coding)))
    cpm_new_heatmap_X5ADP_adjusted_protein_coding<-cpm_new_heatmap_X5ADP_adjusted_protein_coding[,order(number)]
    cpm_new_heatmap_X5ADP_adjusted_protein_coding<-data.frame(cpm_new_heatmap_X5ADP_adjusted_protein_coding,check.names=F)
    cpm_new_heatmap_X5ADP_adjusted_protein_coding$gene<-rownames(cpm_new_heatmap_X5ADP_adjusted_protein_coding)
    cpm_new_long_X5ADP_adjusted_protein_coding<-pivot_longer(data.frame(cpm_new_heatmap_X5ADP_adjusted_protein_coding),cols=contains("MUSCLE"))
    cpm_new_long_X5ADP_adjusted_protein_coding<-merge(cpm_new_long_X5ADP_adjusted_protein_coding,metadata_Respirometry_muscle_mass3,by="name")
    head(cpm_new_long_X5ADP_adjusted_protein_coding)
    cpm_new_long_X5ADP_adjusted_protein_coding$years<-as.numeric(gsub("MUSCLE_AGE(\\d+).*","\\1",cpm_new_long_X5ADP_adjusted_protein_coding$name))
    breaks_X5ADP<-quantile(metadata_Respirometry$X5ADP,prob=c(0,0.33333,0.66666,1))
    breaks_X5ADP[4]<-breaks_X5ADP[4]+1
    breaks_X5ADP[1]<-breaks_X5ADP[1]-1
    cpm_new_long_X5ADP_adjusted_protein_coding$age<-cut(cpm_new_long_X5ADP_adjusted_protein_coding$years,breaks=c(20,30,40,50,60,70,80,90))
    cpm_new_long_X5ADP_adjusted_protein_coding$X5ADP_cut<-cut(cpm_new_long_X5ADP_adjusted_protein_coding$X5ADP,breaks=breaks_X5ADP)
    cpm_new_long_X5ADP_adjusted_protein_coding$X5ADP_cut<-factor(cpm_new_long_X5ADP_adjusted_protein_coding$X5ADP_cut)
    #levels(cpm_new_long_X5ADP_adjusted_protein_coding$X5ADP_cut)<-c("Low X5ADP","Mid X5ADP","High X5ADP")
    levels(cpm_new_long_X5ADP_adjusted_protein_coding$X5ADP_cut)<-c("33-155 (pmol/(s*mg))","155-192 (pmol/(s*mg))","192-293 (pmol/(s*mg))")
    cpm_new_long2_X5ADP_adjusted_protein_coding<-cpm_new_long_X5ADP_adjusted_protein_coding%>%group_by(X5ADP_cut,gene)%>%summarize(mean(value))
    colnames(cpm_new_long2_X5ADP_adjusted_protein_coding)[3]<-"value"
    cpm_wide_X5ADP_adjusted_protein_coding<-cpm_new_long2_X5ADP_adjusted_protein_coding%>%pivot_wider(names_from=X5ADP_cut)
    cpm_wide_X5ADP_adjusted_protein_coding<-data.frame(cpm_wide_X5ADP_adjusted_protein_coding,check.names=F)
    rownames(cpm_wide_X5ADP_adjusted_protein_coding)<-cpm_wide_X5ADP_adjusted_protein_coding$gene
    cpm_wide_merged_X5ADP_adjusted_protein_coding<-merge(annot,cpm_wide_X5ADP_adjusted_protein_coding,by.x="Gene_stable_ID",by.y="row.names")
    #idx<-which(cpm_wide_merged_X5ADP_adjusted_protein_coding$Gene_name=="")
    rownames(cpm_wide_merged_X5ADP_adjusted_protein_coding)[-idx]<-cpm_wide_merged_X5ADP_adjusted_protein_coding$Gene_name
    #rownames(cpm_wide_merged_X5ADP_adjusted_protein_coding)[idx]<-cpm_wide_merged_X5ADP_adjusted_protein_coding$Gene_stable_ID[idx]
    cpm_wide_merged_X5ADP_adjusted_protein_coding<-cpm_wide_merged_X5ADP_adjusted_protein_coding[,13:16]
    cpm_wide_merged_X5ADP_adjusted_protein_coding$gene<-NULL
    library(pheatmap)
    #pheatmap(cpm_wide_merged_X5ADP_adjusted_protein_coding,scale="none",cluster_cols=FALSE)
    #pheatmap(t(cpm_wide_merged_X5ADP_adjusted_protein_coding),scale="none",cluster_cols=TRUE,cluster_rows = FALSE)
    X5ADP_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap((cpm_wide_merged_X5ADP_adjusted_protein_coding),scale="none",cluster_cols=FALSE))
    cpm_wide_merged_X5ADP_adjusted_protein_coding_subset<-cpm_wide_merged_X5ADP_adjusted_protein_coding[,c(1,3)]
    colnames(cpm_wide_merged_X5ADP_adjusted_protein_coding_subset)<-c("Low Mit-O2","High Mit-O2")
    X5ADP_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap(cpm_wide_merged_X5ADP_adjusted_protein_coding_subset,scale="none",cluster_cols=FALSE))
    
    
  }
  
# VO2 high cohort ~ aging
  ##See comments above the highxmin script. 
{
  
    filter_new_VO2_high_up<-merged_final_VO2_high_protein_coding|>
      dplyr::filter(log2FoldChange>0)|>head(10)
    filter_new_VO2_high_down<-merged_final_VO2_high|>
      dplyr::filter(log2FoldChange<0)|>head(10)
    
    filter_new_VO2_high<-rbind(filter_new_VO2_high_up,
                                                  filter_new_VO2_high_down)
    DGE_VO2_high<-DGEList(feature_dat_high)
    DGE_VO2_high<-calcNormFactors(DGE_VO2_high)
    cpm_VO2_high<-cpm(DGE_VO2_high,log=TRUE)
    #filter_new_VO2_adjusted_protein_coding<-merged_final_VO2_muscle_mass3_protein_coding[1:30,]
    z_score_normalized_cpm_VO2_high<-t(scale(t(cpm_VO2_high)))
    idx<-which(rownames(z_score_normalized_cpm_VO2_high)%in%filter_new_VO2_high$Gene_ID)
    cpm_new_heatmap_VO2_high<-z_score_normalized_cpm_VO2_high[idx,]
    number<-as.numeric(gsub("MUSCLE_AGE(\\d+).*",'\\1',colnames(cpm_new_heatmap_VO2_high)))
    cpm_new_heatmap_VO2_high<-cpm_new_heatmap_VO2_high[,order(number)]
    cpm_new_heatmap_VO2_high<-data.frame(cpm_new_heatmap_VO2_high,check.names=F)
    cpm_new_heatmap_VO2_high$gene<-rownames(cpm_new_heatmap_VO2_high)
    cpm_new_long_VO2_high<-pivot_longer(data.frame(cpm_new_heatmap_VO2_high),cols=contains("MUSCLE"))
    cpm_new_long_VO2_high<-merge(cpm_new_long_VO2_high,metadata_VO2_high,by="name")
    head(cpm_new_long_VO2_high)
    cpm_new_long_VO2_high$years<-as.numeric(gsub("MUSCLE_AGE(\\d+).*","\\1",cpm_new_long_VO2_high$name))
    breaks_years<-quantile(metadata_VO2_high$years,prob=c(0,0.33333,0.66666,1))
    breaks_years[4]<-breaks_years[4]+1
    breaks_years[1]<-breaks_years[1]-1
    #cpm_new_long_VO2_high$age<-cut(cpm_new_long_VO2_high$years,breaks=c(20,30,40,50,60,70,80,90))
    cpm_new_long_VO2_high$years_cut<-cut(cpm_new_long_VO2_high$years,breaks=breaks_years)
    cpm_new_long_VO2_high$years_cut<-factor(cpm_new_long_VO2_high$years_cut)
    #levels(cpm_new_long_VO2_high$VO2_cut)<-c("Low","Mid","High")
    #levels(cpm_new_long_VO2_high$VO2_cut)<-c("8-25.2 (mL/kg/min)","25.2-30.5 (mL/kg/min)","30.5-47.6 (mL/kg/min)")
    cpm_new_long2_VO2_high<-cpm_new_long_VO2_high%>%group_by(years_cut,gene)%>%summarize(mean(value))
    colnames(cpm_new_long2_VO2_high)[3]<-"value"
    cpm_wide_VO2_high<-cpm_new_long2_VO2_high%>%pivot_wider(names_from=years_cut)
    cpm_wide_VO2_high<-data.frame(cpm_wide_VO2_high,check.names=F)
    rownames(cpm_wide_VO2_high)<-cpm_wide_VO2_high$gene
    cpm_wide_merged_VO2_high<-merge(annot,cpm_wide_VO2_high,by.x="Gene_stable_ID",by.y="row.names")
    #idx<-which(cpm_wide_merged_VO2_high$Gene_name=="")
    rownames(cpm_wide_merged_VO2_high)[-idx]<-cpm_wide_merged_VO2_high$Gene_name
    #rownames(cpm_wide_merged_VO2_high)[idx]<-cpm_wide_merged_VO2_high$Gene_stable_ID[idx]
    cpm_wide_merged_VO2_high<-cpm_wide_merged_VO2_high[,13:16]
    cpm_wide_merged_VO2_high$gene<-NULL
    library(pheatmap)
    #pheatmap(cpm_wide_merged_VO2_high,scale="none",cluster_cols=FALSE)
    #pheatmap(t(cpm_wide_merged_VO2_high),scale="none",cluster_cols=TRUE,cluster_rows = FALSE)
    VO2_high_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap((cpm_wide_merged_VO2_high),scale="none",cluster_cols=FALSE))
    ##Code below
    cpm_wide_merged_VO2_high_subset<-cpm_wide_merged_VO2_high[,c(1,3)]
    
    colnames(cpm_wide_merged_VO2_high_subset)<-c("Low Years","High Years")
    VO2_heatmap_high_protein_coding<-ggplotify::as.ggplot(pheatmap(cpm_wide_merged_VO2_high_subset,scale="none",cluster_cols=FALSE))
    
    #(highxmin_vol_protein_coding|highxmin_heatmap_protein_coding)/(VO2_vol_protein_coding|VO2_heatmap_protein_coding)
  }

# VO2 low cohort~ aging
  ##See comments above the highxmin script.
{
    filter_new_VO2_low_up<-merged_final_VO2_low_protein_coding|>
      dplyr::filter(log2FoldChange>0)|>head(10)
    filter_new_VO2_low_down<-merged_final_VO2_low|>
      dplyr::filter(log2FoldChange<0)|>head(10)
    
    filter_new_VO2_low<-rbind(filter_new_VO2_low_up,
                               filter_new_VO2_low_down)
    DGE_VO2_low<-DGEList(feature_dat_low)
    DGE_VO2_low<-calcNormFactors(DGE_VO2_low)
    cpm_VO2_low<-cpm(DGE_VO2_low,log=TRUE)
    #filter_new_VO2_adjusted_protein_coding<-merged_final_VO2_muscle_mass3_protein_coding[1:30,]
    z_score_normalized_cpm_VO2_low<-t(scale(t(cpm_VO2_low)))
    idx<-which(rownames(z_score_normalized_cpm_VO2_low)%in%filter_new_VO2_low$Gene_ID)
    cpm_new_heatmap_VO2_low<-z_score_normalized_cpm_VO2_low[idx,]
    number<-as.numeric(gsub("MUSCLE_AGE(\\d+).*",'\\1',colnames(cpm_new_heatmap_VO2_low)))
    cpm_new_heatmap_VO2_low<-cpm_new_heatmap_VO2_low[,order(number)]
    cpm_new_heatmap_VO2_low<-data.frame(cpm_new_heatmap_VO2_low,check.names=F)
    cpm_new_heatmap_VO2_low$gene<-rownames(cpm_new_heatmap_VO2_low)
    cpm_new_long_VO2_low<-pivot_longer(data.frame(cpm_new_heatmap_VO2_low),cols=contains("MUSCLE"))
    cpm_new_long_VO2_low<-merge(cpm_new_long_VO2_low,metadata_VO2_low,by="name")
    head(cpm_new_long_VO2_low)
    cpm_new_long_VO2_low$years<-as.numeric(gsub("MUSCLE_AGE(\\d+).*","\\1",cpm_new_long_VO2_low$name))
    breaks_years<-quantile(metadata_VO2_low$years,prob=c(0,0.33333,0.66666,1))
    breaks_years[4]<-breaks_years[4]+1
    breaks_years[1]<-breaks_years[1]-1
    #cpm_new_long_VO2_low$age<-cut(cpm_new_long_VO2_low$years,breaks=c(20,30,40,50,60,70,80,90))
    cpm_new_long_VO2_low$years_cut<-cut(cpm_new_long_VO2_low$years,breaks=breaks_years)
    cpm_new_long_VO2_low$years_cut<-factor(cpm_new_long_VO2_low$years_cut)
    #levels(cpm_new_long_VO2_low$VO2_cut)<-c("Low","Mid","low")
    #levels(cpm_new_long_VO2_low$VO2_cut)<-c("8-25.2 (mL/kg/min)","25.2-30.5 (mL/kg/min)","30.5-47.6 (mL/kg/min)")
    cpm_new_long2_VO2_low<-cpm_new_long_VO2_low%>%group_by(years_cut,gene)%>%summarize(mean(value))
    colnames(cpm_new_long2_VO2_low)[3]<-"value"
    cpm_wide_VO2_low<-cpm_new_long2_VO2_low%>%pivot_wider(names_from=years_cut)
    cpm_wide_VO2_low<-data.frame(cpm_wide_VO2_low,check.names=F)
    rownames(cpm_wide_VO2_low)<-cpm_wide_VO2_low$gene
    cpm_wide_merged_VO2_low<-merge(annot,cpm_wide_VO2_low,by.x="Gene_stable_ID",by.y="row.names")
    #idx<-which(cpm_wide_merged_VO2_low$Gene_name=="")
    rownames(cpm_wide_merged_VO2_low)[-idx]<-cpm_wide_merged_VO2_low$Gene_name
    #rownames(cpm_wide_merged_VO2_low)[idx]<-cpm_wide_merged_VO2_low$Gene_stable_ID[idx]
    cpm_wide_merged_VO2_low<-cpm_wide_merged_VO2_low[,13:16]
    cpm_wide_merged_VO2_low$gene<-NULL
    library(pheatmap)
    #pheatmap(cpm_wide_merged_VO2_low,scale="none",cluster_cols=FALSE)
    #pheatmap(t(cpm_wide_merged_VO2_low),scale="none",cluster_cols=TRUE,cluster_rows = FALSE)
    VO2_low_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap((cpm_wide_merged_VO2_low),scale="none",cluster_cols=FALSE))
    ##Code below
    cpm_wide_merged_VO2_low_subset<-cpm_wide_merged_VO2_low[,c(1,3)]
    
    colnames(cpm_wide_merged_VO2_low_subset)<-c("Low Years","low Years")
    VO2_heatmap_protein_coding<-ggplotify::as.ggplot(pheatmap(cpm_wide_merged_VO2_low_subset,scale="none",cluster_cols=FALSE))
    
    #(lowxmin_vol_protein_coding|lowxmin_heatmap_protein_coding)/(VO2_vol_protein_coding|VO2_heatmap_protein_coding)
  }
  
(highxmin_vol_protein_coding|VO2_vol_protein_coding|kPCr_vol_protein_coding|X5ADP_vol_protein_coding) + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 15))
# size 12*4
(highxmin_heatmap_protein_coding|VO2_heatmap_protein_coding|kPCr_heatmap_protein_coding|X5ADP_heatmap_protein_coding)
#size 12*6
}
  
# Tables 
{
# Common Genes 
{
    up_dat<-function(dat,type){
      if(type=="up"){
        dat%>%dplyr::filter(log2FoldChange>0,pvalue<0.05)}
      else if(type=="down"){
        dat%>%dplyr::filter(log2FoldChange<0,pvalue<0.05)}
    }
    
    DGE_highxmin<-read.csv("DEseq2_highxmin_sig_protein_coding_muscle_mass3.csv")
    DGE_VO2<-read.csv("DEseq2_VO2_sig_protein_coding_muscle_mass3.csv")
    DGE_kPCr<-read.csv("DEseq2_kPCr_sig_protein_coding_muscle_mass3.csv")
    DGE_X5ADP<-read.csv("DEseq2_X5ADP_sig_protein_coding_muscle_mass3.csv")
    
    
    Resp_up<-up_dat(DGE_X5ADP,"up")
    Resp_down<-up_dat(DGE_X5ADP,"down")
    kPCr_up<-up_dat(DGE_kPCr,"up")
    kPCr_down<-up_dat(DGE_kPCr,"down")
    VO2_up<-up_dat(DGE_VO2,"up")
    VO2_down<-up_dat(DGE_VO2,"down")
    highxmin_up<-up_dat(DGE_highxmin,"up")
    highxmin_down<-up_dat(DGE_highxmin,"down")
    
    all_genes<-unique(c(Resp_up$Gene_ID,kPCr_up$Gene_ID,VO2_up$Gene_ID,highxmin_up$Gene_ID))
    new_dat<-data.frame(genes=all_genes)
    idx_Resp<-all_genes%in%Resp_up$Gene_ID
    new_dat$up_in_Resp<-0
    new_dat$up_in_Resp[idx_Resp]<-1
    idx_VO2<-all_genes%in%VO2_up$Gene_ID
    new_dat$up_in_VO2<-0
    new_dat$up_in_VO2[idx_VO2]<-1
    idx_kPCr<-all_genes%in%kPCr_up$Gene_ID
    new_dat$up_in_kPCr<-0
    new_dat$up_in_kPCr[idx_kPCr]<-1
    idx_highxmin<-all_genes%in%highxmin_up$Gene_ID
    new_dat$up_in_highxmin<-0
    new_dat$up_in_highxmin[idx_highxmin]<-1
    rownames(new_dat)<-new_dat$genes
    new_dat$genes<-NULL
    new_dat_subset<-new_dat[rowSums(new_dat)>1,]
    up_common_DGE<-merge(annot,new_dat_subset,by.x="Gene_stable_ID",by.y="row.names")
    write.csv(up_common_DGE,"common_DGE_list_up_muscle_mass3.csv")
    
    all_genes<-unique(c(Resp_down$Gene_ID,kPCr_down$Gene_ID,VO2_down$Gene_ID,highxmin_down$Gene_ID))
    new_dat<-data.frame(genes=all_genes)
    idx_Resp<-all_genes%in%Resp_down$Gene_ID
    new_dat$down_in_Resp<-0
    new_dat$down_in_Resp[idx_Resp]<-1
    idx_VO2<-all_genes%in%VO2_down$Gene_ID
    new_dat$down_in_VO2<-0
    new_dat$down_in_VO2[idx_VO2]<-1
    idx_kPCr<-all_genes%in%kPCr_down$Gene_ID
    new_dat$down_in_kPCr<-0
    new_dat$down_in_kPCr<-0
    new_dat$down_in_kPCr[idx_kPCr]<-1
    idx_highxmin<-all_genes%in%highxmin_down$Gene_ID
    new_dat$down_in_highxmin<-0
    new_dat$down_in_highxmin[idx_highxmin]<-1
    rownames(new_dat)<-new_dat$genes
    new_dat$genes<-NULL
    new_dat_subset<-new_dat[rowSums(new_dat)>1,]
    down_common_DGE<-merge(annot,new_dat_subset,by.x="Gene_stable_ID",by.y="row.names")
    write.csv(down_common_DGE,"common_DGE_list_down_muscle_mass3.csv")
}
  
# Unique Genes 
{
    up_dat<-function(dat,type){
      if(type=="up"){
        dat%>%dplyr::filter(log2FoldChange>0,pvalue<0.01)}
      else if(type=="down"){
        dat%>%dplyr::filter(log2FoldChange<0,pvalue<0.01)}
    }
    
    Resp_up<-up_dat(merged_final_X5ADP_adjusted_muscle_mass3,"up")
    Resp_down<-up_dat(merged_final_X5ADP_adjusted_muscle_mass3,"down")
    kPCr_up<-up_dat(merged_final_kPCr_adjusted_muscle_mass3,"up")
    kPCr_down<-up_dat(merged_final_kPCr_adjusted_muscle_mass3,"down")
    VO2_up<-up_dat(merged_final_VO2_muscle_mass3,"up")
    VO2_down<-up_dat(merged_final_VO2_muscle_mass3,"down")
    highxmin_up<-up_dat(merged_final_highxmin_adjusted_muscle_mass3,"up")
    highxmin_down<-up_dat(merged_final_highxmin_adjusted_muscle_mass3,"down")
    
    all_genes<-unique(c(Resp_up$Gene_ID,kPCr_up$Gene_ID,VO2_up$Gene_ID,highxmin_up$Gene_ID))
    new_dat<-data.frame(genes=all_genes)
    idx_Resp<-all_genes%in%Resp_up$Gene_ID
    new_dat$up_in_Resp<-0
    new_dat$up_in_Resp[idx_Resp]<-1
    idx_VO2<-all_genes%in%VO2_up$Gene_ID
    new_dat$up_in_VO2<-0
    new_dat$up_in_VO2[idx_VO2]<-1
    idx_kPCr<-all_genes%in%kPCr_up$Gene_ID
    new_dat$up_in_kPCr<-0
    new_dat$up_in_kPCr<-0
    new_dat$up_in_kPCr[idx_kPCr]<-1
    idx_highxmin<-all_genes%in%highxmin_up$Gene_ID
    new_dat$up_in_highxmin<-0
    new_dat$up_in_highxmin[idx_highxmin]<-1
    rownames(new_dat)<-new_dat$genes
    new_dat$genes<-NULL
    new_dat_subset<-new_dat[rowSums(new_dat)==1,]
    up_unique_DGE<-merge(annot,new_dat_subset,by.x="Gene_stable_ID",by.y="row.names")
    write.csv(up_unique_DGE,"unique_DGE_list_up_muscle_mass3.csv")
    
    all_genes<-unique(c(Resp_down$Gene_ID,kPCr_down$Gene_ID,VO2_down$Gene_ID,highxmin_down$Gene_ID))
    new_dat<-data.frame(genes=all_genes)
    idx_Resp<-all_genes%in%Resp_down$Gene_ID
    new_dat$down_in_Resp<-0
    new_dat$down_in_Resp[idx_Resp]<-1
    idx_VO2<-all_genes%in%VO2_down$Gene_ID
    new_dat$down_in_VO2<-0
    new_dat$down_in_VO2[idx_VO2]<-1
    idx_kPCr<-all_genes%in%kPCr_down$Gene_ID
    new_dat$down_in_kPCr<-0
    new_dat$down_in_kPCr<-0
    new_dat$down_in_kPCr[idx_kPCr]<-1
    idx_highxmin<-all_genes%in%highxmin_down$Gene_ID
    new_dat$down_in_highxmin<-0
    new_dat$down_in_highxmin[idx_highxmin]<-1
    rownames(new_dat)<-new_dat$genes
    new_dat$genes<-NULL
    new_dat_subset<-new_dat[rowSums(new_dat)==1,]
    down_unique_DGE<-merge(annot,new_dat_subset,by.x="Gene_stable_ID",by.y="row.names")
    write.csv(down_unique_DGE,"unique_DGE_list_down_muscle_mass3.csv")
  }  
}

# Figure 3 a,b,c,d: Gene Set Enrichment Analysis (GSEA)
{
# highxmin 
{
      msigdbr_df <- msigdbr(species = "human")
      
      pathwaysH = split(x = msigdbr_df$ensembl_gene, f = msigdbr_df$gs_name)
      ## Do you want to do logfold, or the following formula. Multiply sign of the logfold by negative log of pvalue
      merged_for_GSEA<-merge(data.frame(res_highxmin_adjusted_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_for_GSEA<-merged_for_GSEA[order(merged_for_GSEA$pvalue),]
      merged_for_gsea<-merged_for_GSEA%>%dplyr::filter(Gene_type=="protein_coding")
      merged_for_GSEA$rank<-sign(merged_for_GSEA$log2FoldChange)*-log(merged_for_GSEA$pvalue)
      ##rank by the above formula and then name the data
      fgsea_rank<-merged_for_GSEA$rank
      names(fgsea_rank)=merged_for_GSEA$Row.names
      fgsea_rank<-fgsea_rank[!is.na(fgsea_rank)]
      fgsea_rank<-fgsea_rank[order(-fgsea_rank)]
      set.seed(2)
      
      m_t2g <- purrr::map_dfr(c("CP:REACTOME", "CP:KEGG", "CP:BIOCARTA"),~ msigdbr(species = "Homo sapiens", category = "C2", subcategory = .x) %>%                              
                                dplyr::select(gs_name, ensembl_gene))
      
      test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05)
      test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding_df<-data.frame(test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding)
      
      write.csv(test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding_df,"test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding.csv")
      #test_REACTOME_highxmin_adjusted_muscle_mass3<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05,pAdjustMethod="none")
      dim(test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding_df)
      View(data.frame(test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding_df))
      colnames(merged_for_gsea)[1]<-"Gene_stable_ID"
      
      m_t2g_final<-m_t2g
      # m_t2g_final$gs_name<-gsub("(REACTOME|WP|KEGG|BIOCARTA)","",m_t2g_final$gs_name)
      
      m_t2g_final$gs_name<-gsub("^_","",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("_"," ",m_t2g_final$gs_name)
      
      m_t2g_final$gs_name<-gsub("REACTOME","(R)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("BIOCARTA","(B)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("KEGG","(K)",m_t2g_final$gs_name)
      
      set.seed(2)
      test_REACTOME<-clusterProfiler::GSEA(geneList=fgsea_rank,TERM2GENE=m_t2g_final,by="fgsea")
      test_REACTOME_df<-data.frame(test_REACTOME)
      test_REACTOME_df_positive<-test_REACTOME_df[test_REACTOME_df$NES>0,]
      test_REACTOME_df_positive<-test_REACTOME_df_positive[order(-test_REACTOME_df_positive$NES),]
      if(dim(test_REACTOME_df_positive)[1]>10){
        row_pos=10
      }else{
        row_pos=dim(test_REACTOME_df_positive)[1]
      }
      test_REACTOME_df_positive<-test_REACTOME_df_positive[1:row_pos,]
      test_REACTOME_df_negative<-test_REACTOME_df[test_REACTOME_df$NES<0,]
      test_REACTOME_df_negative<-test_REACTOME_df_negative[order(test_REACTOME_df_negative$NES),]
      if(dim(test_REACTOME_df_negative)[1]>10){
        row_neg=10
      }else{
        row_neg=dim(test_REACTOME_df_negative)[1]
      }
      
      test_REACTOME_df_negative<-test_REACTOME_df_negative[1:row_neg,]
      
      test_REACTOME_df_positive_negative<-rbind(test_REACTOME_df_positive,test_REACTOME_df_negative)
      test_REACTOME_subset<-test_REACTOME_df_positive_negative[order(test_REACTOME_df_positive_negative$NES),]
      
      
      test_REACTOME_subset$Description<-factor(test_REACTOME_subset$Description,levels=test_REACTOME_subset$Description)
      test_REACTOME_subset%>%ggplot(aes(x=NES,y=Description,fill=NES,alpha=p.adjust),col="black")+
        geom_bar(stat="identity",width = 0.7)+scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0)+
        scale_alpha(trans="reverse")+
        theme_minimal()+
        guides(fill="none")+
        theme(axis.text.y=element_text(size=13,colour = "black"))
      #8X5
    }
    
# VO2
{
      msigdbr_df <- msigdbr(species = "human")
      pathwaysH = split(x = msigdbr_df$ensembl_gene, f = msigdbr_df$gs_name)
      ## Do you want to do logfold, or the following formula. Multiply sign of the logfold by negative log of pvalue
      merged_for_GSEA<-merge(data.frame(res_VO2_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_for_GSEA<-merged_for_GSEA[order(merged_for_GSEA$pvalue),]
      merged_for_gsea<-merged_for_GSEA%>%dplyr::filter(Gene_type=="protein_coding")
      merged_for_GSEA$rank<-sign(merged_for_GSEA$log2FoldChange)*-log(merged_for_GSEA$pvalue)
      
      
      ##rank by the above formula and then name the data
      fgsea_rank<-merged_for_GSEA$rank
      names(fgsea_rank)=merged_for_GSEA$Row.names
      fgsea_rank<-fgsea_rank[!is.na(fgsea_rank)]
      fgsea_rank<-fgsea_rank[order(-fgsea_rank)]
      
      m_t2g <- purrr::map_dfr(c("CP:REACTOME", "CP:KEGG", "CP:BIOCARTA"),~ msigdbr(species = "Homo sapiens", category = "C2", subcategory = .x) %>%                              dplyr::select(gs_name, ensembl_gene))
      
      set.seed(2)
      test_REACTOME_VO2_muscle_mass3_protein_coding<-GSEA(fgsea_rank,TERM2GENE=m_t2g)
      test_REACTOME_VO2_muscle_mass3_protein_coding_df<-data.frame(test_REACTOME_VO2_muscle_mass3_protein_coding)
      write.csv(test_REACTOME_VO2_muscle_mass3_protein_coding_df,"test_REACTOME_VO2_muscle_mass3_protein_coding.csv")
      #test_REACTOME_VO2_muscle_mass3<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05,pAdjustMethod="none")
      dim(test_REACTOME_VO2_muscle_mass3_protein_coding_df)
      View(data.frame(test_REACTOME_VO2_muscle_mass3_protein_coding_df))
      colnames(merged_for_gsea)[1]<-"Gene_stable_ID"
      #run_GSEA(merged_for_gsea)
      
      # ridgeplot(test_REACTOME_VO2_muscle_mass3_protein_coding,showCategory = 15)+scale_fill_viridis()+
      #  theme(axis.text.y=element_text(size=10))
      # test_REACTOME_VO2_muscle_mass3_df<-data.frame(test_REACTOME_VO2_muscle_mass3_protein_coding)
      
      m_t2g_final<-m_t2g
      #m_t2g_final$gs_name<-gsub("(REACTOME|WP|KEGG|BIOCARTA)","",m_t2g_final$gs_name)
      
      

      m_t2g_final$gs_name<-gsub("^_","",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("_"," ",m_t2g_final$gs_name)
      
      m_t2g_final$gs_name<-gsub("REACTOME","(R)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("BIOCARTA","(B)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("KEGG","(K)",m_t2g_final$gs_name)
      
      
      
      
      set.seed(2)
      test_REACTOME<-clusterProfiler::GSEA(geneList=fgsea_rank,TERM2GENE=m_t2g_final,by="fgsea")
      test_REACTOME_df<-data.frame(test_REACTOME)
      test_REACTOME_df_positive<-test_REACTOME_df[test_REACTOME_df$NES>0,]
      test_REACTOME_df_positive<-test_REACTOME_df_positive[order(-test_REACTOME_df_positive$NES),]
      if(dim(test_REACTOME_df_positive)[1]>14){
        row_pos=14
      }else{
        row_pos=dim(test_REACTOME_df_positive)[1]
      }
      test_REACTOME_df_positive<-test_REACTOME_df_positive[1:row_pos,]
      test_REACTOME_df_negative<-test_REACTOME_df[test_REACTOME_df$NES<0,]
      test_REACTOME_df_negative<-test_REACTOME_df_negative[order(test_REACTOME_df_negative$NES),]
      if(dim(test_REACTOME_df_negative)[1]>10){
        row_neg=10
      }else{
        row_neg=dim(test_REACTOME_df_negative)[1]
      }
      
      test_REACTOME_df_negative<-test_REACTOME_df_negative[1:row_neg,]
      
      test_REACTOME_df_positive_negative<-rbind(test_REACTOME_df_positive,test_REACTOME_df_negative)
      test_REACTOME_subset<-test_REACTOME_df_positive_negative[order(test_REACTOME_df_positive_negative$NES),]
      

      
      test_REACTOME_subset$Description<-factor(test_REACTOME_subset$Description,levels=test_REACTOME_subset$Description)
      test_REACTOME_subset%>%ggplot(aes(x=NES,y=Description,fill=NES,alpha=p.adjust),col="black")+
        geom_bar(stat="identity",width = 0.7)+scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0)+
        scale_alpha(trans="reverse")+
        theme_minimal()+
        guides(fill="none")+
        theme(axis.text.y=element_text(size=13,colour = "black"))
      #8.5*10
    }
  
# kPCr 
{
      msigdbr_df <- msigdbr(species = "human")
      pathwaysH = split(x = msigdbr_df$ensembl_gene, f = msigdbr_df$gs_name)
      ## Do you want to do logfold, or the following formula. Multiply sign of the logfold by negative log of pvalue
      merged_for_GSEA<-merge(data.frame(res_kPCr_adjusted_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_for_GSEA<-merged_for_GSEA[order(merged_for_GSEA$pvalue),]
      merged_for_gsea<-merged_for_GSEA%>%dplyr::filter(Gene_type=="protein_coding")
      merged_for_GSEA$rank<-sign(merged_for_GSEA$log2FoldChange)*-log(merged_for_GSEA$pvalue)
      fgsea_rank<-merged_for_GSEA$rank
      names(fgsea_rank)=merged_for_GSEA$Row.names
      fgsea_rank<-fgsea_rank[!is.na(fgsea_rank)]
      fgsea_rank<-fgsea_rank[order(-fgsea_rank)]
      set.seed(2)
      
      m_t2g <- purrr::map_dfr(c("CP:REACTOME", "CP:KEGG", "CP:BIOCARTA"),~ msigdbr(species = "Homo sapiens", category = "C2", subcategory = .x) %>%                              dplyr::select(gs_name, ensembl_gene))
      

      test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05)
      test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding_df<-data.frame(test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding)
      write.csv(test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding_df,"test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding.csv")
      #test_REACTOME_kPCr_adjusted_muscle_mass3<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05,pAdjustMethod="none")
      dim(test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding_df)
      View(data.frame(test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding_df))
      colnames(merged_for_gsea)[1]<-"Gene_stable_ID"
      #run_GSEA(merged_for_gsea)
      m_t2g_final<-m_t2g
      m_t2g_final$gs_name<-gsub("^_","",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("_"," ",m_t2g_final$gs_name)
      
      
      m_t2g_final$gs_name<-gsub("REACTOME","(R)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("BIOCARTA","(B)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("KEGG","(K)",m_t2g_final$gs_name)
      
      
      
      
      set.seed(2)
      test_REACTOME<-clusterProfiler::GSEA(geneList=fgsea_rank,TERM2GENE=m_t2g_final,by="fgsea")
      test_REACTOME_df<-data.frame(test_REACTOME)
      test_REACTOME_df_positive<-test_REACTOME_df[test_REACTOME_df$NES>0,]
      test_REACTOME_df_positive<-test_REACTOME_df_positive[order(-test_REACTOME_df_positive$NES),]
      if(dim(test_REACTOME_df_positive)[1]>10){
        row_pos=10
      }else{
        row_pos=dim(test_REACTOME_df_positive)[1]
      }
      test_REACTOME_df_positive<-test_REACTOME_df_positive[1:row_pos,]
      test_REACTOME_df_negative<-test_REACTOME_df[test_REACTOME_df$NES<0,]
      test_REACTOME_df_negative<-test_REACTOME_df_negative[order(test_REACTOME_df_negative$NES),]
      if(dim(test_REACTOME_df_negative)[1]>10){
        row_neg=10
      }else{
        row_neg=dim(test_REACTOME_df_negative)[1]
      }
      
      test_REACTOME_df_negative<-test_REACTOME_df_negative[1:row_neg,]
      
      test_REACTOME_df_positive_negative<-rbind(test_REACTOME_df_positive,test_REACTOME_df_negative)
      test_REACTOME_subset<-test_REACTOME_df_positive_negative[order(test_REACTOME_df_positive_negative$NES),]
      
      #KPCR
      #test_REACTOME_subset[grepl("POSITIVE REGULATION OF TUMOR NECROSIS FACTOR SUPERFAMILY CYTOKINE PRODUCTION",test_REACTOME_subset$Description),]$Description<-"POSITIVE REGULATION OF TNF CYTOKINE PRODUCTION"
      
      
      test_REACTOME_subset$Description<-factor(test_REACTOME_subset$Description,levels=test_REACTOME_subset$Description)
      test_REACTOME_subset%>%ggplot(aes(x=NES,y=Description,fill=NES,alpha=p.adjust),col="black")+
        geom_bar(stat="identity",width = 0.7)+scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0)+
        scale_alpha(trans="reverse")+
        theme_minimal()+
        guides(fill="none")+
        theme(axis.text.y=element_text(size=13,colour = "black"))
      #
    }
    
# X5ADP (Respirometry)
{
      msigdbr_df <- msigdbr(species = "human")
      pathwaysH = split(x = msigdbr_df$ensembl_gene, f = msigdbr_df$gs_name)
      ## Do you want to do logfold, or the following formula. Multiply sign of the logfold by negative log of pvalue
      merged_for_GSEA<-merge(data.frame(res_X5ADP_adjusted_muscle_mass3),annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_for_GSEA<-merged_for_GSEA[order(merged_for_GSEA$pvalue),]
      merged_for_gsea<-merged_for_GSEA%>%dplyr::filter(Gene_type=="protein_coding")
      merged_for_GSEA$rank<-sign(merged_for_GSEA$log2FoldChange)*-log(merged_for_GSEA$pvalue)
      ##rank by the above formula and then name the data
      fgsea_rank<-merged_for_GSEA$rank
      names(fgsea_rank)=merged_for_GSEA$Row.names
      fgsea_rank<-fgsea_rank[!is.na(fgsea_rank)]
      fgsea_rank<-fgsea_rank[order(-fgsea_rank)]
      set.seed(2)
      

      # Combine the results
      m_t2g <- purrr::map_dfr(c("CP:REACTOME", "CP:KEGG", "CP:BIOCARTA"),~ msigdbr(species = "Homo sapiens", category = "C2", subcategory = .x) %>%                              dplyr::select(gs_name, ensembl_gene))
      

      test_REACTOME_Respirometry_adjusted_muscle_mass3_protein_coding<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05)
      test_REACTOME_Respirometry_adjusted_muscle_mass3_protein_coding_df<-data.frame(test_REACTOME_Respirometry_adjusted_muscle_mass3_protein_coding)
      write.csv(test_REACTOME_Respirometry_adjusted_muscle_mass3_protein_coding_df,"test_REACTOME_X5ADP_adjusted_muscle_mass3_protein_coding.csv")
      #test_REACTOME_Respirometry_adjusted_muscle_mass3<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05,pAdjustMethod="none")
      dim(test_REACTOME_Respirometry_adjusted_muscle_mass3_protein_coding_df)
      View(data.frame(test_REACTOME_Respirometry_adjusted_muscle_mass3_protein_coding_df))
      
      colnames(merged_for_gsea)[1]<-"Gene_stable_ID"
      #run_GSEA(merged_for_gsea)
      m_t2g_final<-m_t2g
      m_t2g_final$gs_name<-gsub("^_","",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("_"," ",m_t2g_final$gs_name)
      
      
      m_t2g_final$gs_name<-gsub("REACTOME","(R)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("BIOCARTA","(B)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("KEGG","(K)",m_t2g_final$gs_name)
      
      
      
      
      set.seed(2)
      test_REACTOME<-clusterProfiler::GSEA(geneList=fgsea_rank,TERM2GENE=m_t2g_final,by="fgsea")
      test_REACTOME_df<-data.frame(test_REACTOME)
      test_REACTOME_df_positive<-test_REACTOME_df[test_REACTOME_df$NES>0,]
      test_REACTOME_df_positive<-test_REACTOME_df_positive[order(-test_REACTOME_df_positive$NES),]
      if(dim(test_REACTOME_df_positive)[1]>10){
        row_pos=10
      }else{
        row_pos=dim(test_REACTOME_df_positive)[1]
      }
      test_REACTOME_df_positive<-test_REACTOME_df_positive[1:row_pos,]
      test_REACTOME_df_negative<-test_REACTOME_df[test_REACTOME_df$NES<0,]
      test_REACTOME_df_negative<-test_REACTOME_df_negative[order(test_REACTOME_df_negative$NES),]
      if(dim(test_REACTOME_df_negative)[1]>10){
        row_neg=10
      }else{
        row_neg=dim(test_REACTOME_df_negative)[1]
      }
      
      test_REACTOME_df_negative<-test_REACTOME_df_negative[1:row_neg,]
      
      test_REACTOME_df_positive_negative<-rbind(test_REACTOME_df_positive,test_REACTOME_df_negative)
      test_REACTOME_subset<-test_REACTOME_df_positive_negative[order(test_REACTOME_df_positive_negative$NES),]
      
      
      test_REACTOME_subset$Description<-factor(test_REACTOME_subset$Description,levels=test_REACTOME_subset$Description)
      test_REACTOME_subset%>%ggplot(aes(x=NES,y=Description,fill=NES,alpha=p.adjust),col="black")+
        geom_bar(stat="identity",width = 0.7)+scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0)+
        scale_alpha(trans="reverse")+
        theme_minimal()+
        guides(fill="none")+
        theme(axis.text.y=element_text(size=13,colour = "black"))
      
      
      
      
      
      
    }
    
# VO2 high ~ Aging
{
      msigdbr_df <- msigdbr(species = "human")
      
      pathwaysH = split(x = msigdbr_df$ensembl_gene, f = msigdbr_df$gs_name)
      ## Do you want to do logfold, or the following formula. Multiply sign of the logfold by negative log of pvalue
      merged_for_GSEA<-merge(data.frame(res_VO2_high),annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_for_GSEA<-merged_for_GSEA[order(merged_for_GSEA$pvalue),]
      merged_for_gsea<-merged_for_GSEA%>%dplyr::filter(Gene_type=="protein_coding")
      merged_for_GSEA$rank<-sign(merged_for_GSEA$log2FoldChange)*-log(merged_for_GSEA$pvalue)
      ##rank by the above formula and then name the data
      fgsea_rank<-merged_for_GSEA$rank
      names(fgsea_rank)=merged_for_GSEA$Row.names
      fgsea_rank<-fgsea_rank[!is.na(fgsea_rank)]
      fgsea_rank<-fgsea_rank[order(-fgsea_rank)]
      set.seed(2)
      
      m_t2g <- purrr::map_dfr(c("CP:REACTOME", "CP:KEGG", "CP:BIOCARTA"),~ msigdbr(species = "Homo sapiens", category = "C2", subcategory = .x) %>%                              dplyr::select(gs_name, ensembl_gene))
      
      test_REACTOME_VO2_adjusted_muscle_mass3_protein_coding<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05)
      test_REACTOME_VO2_adjusted_muscle_mass3_protein_coding_df<-data.frame(test_REACTOME_VO2_adjusted_muscle_mass3_protein_coding)
      
      write.csv(test_REACTOME_VO2_adjusted_muscle_mass3_protein_coding_df,"REACTOME_VO2_high.csv")
      dim(test_REACTOME_VO2_adjusted_muscle_mass3_protein_coding_df)
      View(data.frame(test_REACTOME_VO2_adjusted_muscle_mass3_protein_coding_df))
      colnames(merged_for_gsea)[1]<-"Gene_stable_ID"
      #run_GSEA(merged_for_gsea)
      

      m_t2g_final<-m_t2g
      m_t2g_final$gs_name<-gsub("_"," ",m_t2g_final$gs_name)
      
      
      m_t2g_final$gs_name<-gsub("REACTOME","(R)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("BIOCARTA","(B)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("KEGG","(K)",m_t2g_final$gs_name)
      
      set.seed(2)
      test_REACTOME<-clusterProfiler::GSEA(geneList=fgsea_rank,TERM2GENE=m_t2g_final,by="fgsea")
      test_REACTOME_df<-data.frame(test_REACTOME)
      test_REACTOME_df_positive<-test_REACTOME_df[test_REACTOME_df$NES>0,]
      test_REACTOME_df_positive<-test_REACTOME_df_positive[order(-test_REACTOME_df_positive$NES),]
      if(dim(test_REACTOME_df_positive)[1]>10){
        row_pos=10
      }else{
        row_pos=dim(test_REACTOME_df_positive)[1]
      }
      test_REACTOME_df_positive<-test_REACTOME_df_positive[1:row_pos,]
      test_REACTOME_df_negative<-test_REACTOME_df[test_REACTOME_df$NES<0,]
      test_REACTOME_df_negative<-test_REACTOME_df_negative[order(test_REACTOME_df_negative$NES),]
      if(dim(test_REACTOME_df_negative)[1]>17){
        row_neg=17
      }else{
        row_neg=dim(test_REACTOME_df_negative)[1]
      }
      
      test_REACTOME_df_negative<-test_REACTOME_df_negative[1:row_neg,]
      
      test_REACTOME_df_positive_negative<-rbind(test_REACTOME_df_positive,test_REACTOME_df_negative)
      test_REACTOME_subset<-test_REACTOME_df_positive_negative[order(test_REACTOME_df_positive_negative$NES),]
      

      test_REACTOME_subset$Description<-factor(test_REACTOME_subset$Description,levels=test_REACTOME_subset$Description)
      test_REACTOME_subset%>%ggplot(aes(x=NES,y=Description,fill=NES,alpha=p.adjust),col="black")+
        geom_bar(stat="identity",width = 0.7)+scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0)+
        scale_alpha(trans="reverse")+
        theme_minimal()+
        guides(fill="none")+
        theme(axis.text.y=element_text(size=13,colour = "black"))
      #11*10
    }
    
# Vo2 low ~ Aging
{
      msigdbr_df <- msigdbr(species = "human")
      
      pathwaysH = split(x = msigdbr_df$ensembl_gene, f = msigdbr_df$gs_name)
      ## Do you want to do logfold, or the following formula. Multiply sign of the logfold by negative log of pvalue
      merged_for_GSEA<-merge(data.frame(res_VO2_low),annot,by.x="row.names",by.y="Gene_stable_ID")
      merged_for_GSEA<-merged_for_GSEA[order(merged_for_GSEA$pvalue),]
      merged_for_gsea<-merged_for_GSEA%>%dplyr::filter(Gene_type=="protein_coding")
      merged_for_GSEA$rank<-sign(merged_for_GSEA$log2FoldChange)*-log(merged_for_GSEA$pvalue)
      ##rank by the above formula and then name the data
      fgsea_rank<-merged_for_GSEA$rank
      names(fgsea_rank)=merged_for_GSEA$Row.names
      fgsea_rank<-fgsea_rank[!is.na(fgsea_rank)]
      fgsea_rank<-fgsea_rank[order(-fgsea_rank)]
      set.seed(2)
      
      # Combine the results
      m_t2g <- purrr::map_dfr(c("CP:REACTOME", "CP:KEGG", "CP:BIOCARTA"),~ msigdbr(species = "Homo sapiens", category = "C2", subcategory = .x) %>%                              
                                dplyr::select(gs_name, ensembl_gene))
      
      test_C2_VO2_adjusted_muscle_mass3_protein_coding<-GSEA(fgsea_rank,TERM2GENE=m_t2g,pvalueCutoff=0.05)
      test_C2_VO2_adjusted_muscle_mass3_protein_coding_df<-data.frame(test_C2_VO2_adjusted_muscle_mass3_protein_coding)
      
      write.csv(test_C2_VO2_adjusted_muscle_mass3_protein_coding_df,"C2_VO2_low.csv")
      dim(test_C2_VO2_adjusted_muscle_mass3_protein_coding_df)
      View(data.frame(test_C2_VO2_adjusted_muscle_mass3_protein_coding_df))
      colnames(merged_for_gsea)[1]<-"Gene_stable_ID"
      
      m_t2g_final<-m_t2g
      
      m_t2g_final$gs_name<-gsub("_"," ",m_t2g_final$gs_name)
      
      
      m_t2g_final$gs_name<-gsub("REACTOME","(R)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("BIOCARTA","(B)",m_t2g_final$gs_name)
      m_t2g_final$gs_name<-gsub("KEGG","(K)",m_t2g_final$gs_name)
      
      
      
      
      set.seed(2)
      test_C2<-clusterProfiler::GSEA(geneList=fgsea_rank,TERM2GENE=m_t2g_final,by="fgsea")
      test_C2_df<-data.frame(test_C2)
      test_C2_df_positive<-test_C2_df[test_C2_df$NES>0,]
      test_C2_df_positive<-test_C2_df_positive[order(-test_C2_df_positive$NES),]
      if(dim(test_C2_df_positive)[1]>10){
        row_pos=10
      }else{
        row_pos=dim(test_C2_df_positive)[1]
      }
      test_C2_df_positive<-test_C2_df_positive[1:row_pos,]
      test_C2_df_negative<-test_C2_df[test_C2_df$NES<0,]
      test_C2_df_negative<-test_C2_df_negative[order(test_C2_df_negative$NES),]
      if(dim(test_C2_df_negative)[1]>10){
        row_neg=10
      }else{
        row_neg=dim(test_C2_df_negative)[1]
      }
      
      test_C2_df_negative<-test_C2_df_negative[1:row_neg,]
      
      test_C2_df_positive_negative<-rbind(test_C2_df_positive,test_C2_df_negative)
      test_C2_subset<-test_C2_df_positive_negative[order(test_C2_df_positive_negative$NES),]
      
      
      test_C2_subset$Description<-factor(test_C2_subset$Description,levels=test_C2_subset$Description)
      test_C2_subset%>%ggplot(aes(x=NES,y=Description,fill=NES,alpha=p.adjust),col="black")+
        geom_bar(stat="identity",width = 0.7)+scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0)+
        scale_alpha(trans="reverse")+
        theme_minimal()+
        guides(fill="none")+
        theme(axis.text.y=element_text(size=10,colour = "black"))
      #11*10
    }
}

# Figure 3e: GSEA shared among energetics (dotplot)
{
  ## Overlap dotplot Protein Coding
  #install.packages("data.table")  # Install the package
  library(data.table)
  
  gsea_highxmin_muscle_mass3<-read.csv("test_REACTOME_highxmin_adjusted_muscle_mass3_protein_coding.csv")
  gsea_highxmin_muscle_mass3$Comparison<-"PA"
  gsea_Respirometry_muscle_mass3<-read.csv("test_REACTOME_X5ADP_adjusted_muscle_mass3_protein_coding.csv") 
  gsea_Respirometry_muscle_mass3$Comparison<-"Mit-O2flux"
  gsea_VO2_muscle_mass3<-read.csv("test_REACTOME_VO2_muscle_mass3_protein_coding.csv")
  gsea_VO2_muscle_mass3$Comparison<-"VO2"
  gsea_kPCr_muscle_mass3<-read.csv("test_REACTOME_kPCr_adjusted_muscle_mass3_protein_coding.csv") 
  gsea_kPCr_muscle_mass3$Comparison<-"kPCr"  
  gsea_list<-rbindlist(list(gsea_highxmin_muscle_mass3,gsea_Respirometry_muscle_mass3,gsea_VO2_muscle_mass3,gsea_kPCr_muscle_mass3))
  gsea_list_NES_positive<-gsea_list%>%filter(NES>0)
  gsea_list_NES_negative<-gsea_list%>%filter(NES<0)
  
  gsea_ct_positive<-gsea_list_NES_positive%>%group_by(ID)%>%summarize(n=n())
  gsea_ct_negative<-gsea_list_NES_negative%>%group_by(ID)%>%summarize(n=n())
  gsea_ids_to_keep_positive<-gsea_ct_positive[gsea_ct_positive$n>2,]$ID
  gsea_ids_to_keep_negative<-gsea_ct_negative[gsea_ct_negative$n>2,]$ID
  idx_positive<-which(gsea_list_NES_positive$ID%in%gsea_ids_to_keep_positive)
  idx_negative<-which(gsea_list_NES_negative$ID%in%gsea_ids_to_keep_negative)
  gsea_final_list_positive<-gsea_list_NES_positive[idx_positive,]
  gsea_final_list_negative<-gsea_list_NES_negative[idx_negative,]
  final_gsea<-rbind(gsea_final_list_positive,gsea_final_list_negative)
  common_GSEA_muscle_mass3<-final_gsea$ID
  final_gsea$Comparison<-factor(final_gsea$Comparison,levels=c("PA","VO2","kPCr","Mit-O2flux"))

  final_gsea$ID<-gsub("REACTOME","(R)",final_gsea$ID)
  final_gsea$ID<-gsub("BIOCARTA","(B)",final_gsea$ID)
  final_gsea$ID<-gsub("KEGG","(K)",final_gsea$ID)
  
  
  avg_NES<-final_gsea%>%group_by(ID)%>%summarize(avg_NES=mean(NES))
  avg_NES<-avg_NES[order(avg_NES$avg_NES),]
  final_gsea$ID<-factor(final_gsea$ID,levels=avg_NES$ID)
  i<-1
  core_gsea_count<-str_count(final_gsea$core_enrichment,"/")+1
  all_enrichments<-paste(final_gsea$core_enrichment,collapse="/")
  split_all_enrichments<-strsplit(all_enrichments,"/")[[1]]
  n_generatio<-length(unique(split_all_enrichments))
  gene_ratio_gsea<-core_gsea_count/n_generatio
  final_gsea$`Gene Ratio`<-gene_ratio_gsea
  GSEA_plot<-final_gsea%>%ggplot(aes(x=Comparison,y=ID))+
    geom_point(aes(col=NES,size=`Gene Ratio`))+
    theme_minimal()+
    scale_color_gradient2(low = 'blue', mid = 'white', high = 'red')+
    theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=13))
  ggsave("gsea_overlap_muscle_mass3_protein_coding.pdf",height=10,width=15)
  GSEA_plot
  #size 13*11
  #12 10
}

# Table GSEA Unique (WE ARE NOT USING IN THE PAPER, RIGHT? IF NOT WE CAN DELETE IT)
{
  idx_highxmin_unique<-which(!(gsea_highxmin_muscle_mass3$ID%in%gsea_VO2_muscle_mass3$ID |
                                 gsea_highxmin_muscle_mass3$ID %in% gsea_kPCr_muscle_mass3$ID | 
                                 gsea_highxmin_muscle_mass3$ID %in%gsea_Respirometry_muscle_mass3$ID))
  unique_GSEA_to_highxmin<-gsea_highxmin_muscle_mass3[idx_highxmin_unique,]
  
  idx_VO2_unique<-which(!(gsea_VO2_muscle_mass3$ID%in%gsea_highxmin_muscle_mass3$ID |
                            gsea_VO2_muscle_mass3$ID %in% gsea_kPCr_muscle_mass3$ID | 
                            gsea_VO2_muscle_mass3$ID %in%gsea_Respirometry_muscle_mass3$ID))
  
  
  unique_GSEA_to_VO2<-gsea_VO2_muscle_mass3[idx_VO2_unique,]
  
  idx_KPCr_unique<-which(!(gsea_kPCr_muscle_mass3$ID%in%gsea_highxmin_muscle_mass3$ID |
                             gsea_kPCr_muscle_mass3$ID %in% gsea_VO2_muscle_mass3$ID | 
                             gsea_kPCr_muscle_mass3$ID %in%gsea_Respirometry_muscle_mass3$ID))
  
  
  
  
  unique_GSEA_to_kPCR<-gsea_kPCr_muscle_mass3[idx_KPCr_unique,]
  
  idx_Respirometry_unique<-which(!(gsea_Respirometry_muscle_mass3$ID%in%gsea_highxmin_muscle_mass3$ID |
                                     gsea_Respirometry_muscle_mass3$ID %in% gsea_VO2_muscle_mass3$ID | 
                                     gsea_Respirometry_muscle_mass3$ID %in%gsea_kPCr_muscle_mass3$ID))
  
  
  unique_GSEA_to_Resp<-gsea_Respirometry_muscle_mass3[idx_Respirometry_unique,]
  write.csv(unique_GSEA_to_highxmin,"unique_GSEA_highxmin.csv")
  write.csv(unique_GSEA_to_VO2,"unique_GSEA_VO2.csv")
  write.csv(unique_GSEA_to_kPCR,"unique_GSEA_kPCr.csv")
  write.csv(unique_GSEA_to_Resp,"unique_GSEA_Respirometry.csv")
}

# Figure 4: Alternative Splicing (AS) models
{
# preparation
{ 
  #Full Path Edited Out
  events<-read.delim("events.psi")
  metadata<-read.delim("metadata.txt",check.names=F)
  events_Respirometry<-read.delim("events_Respirometry.psi")
  ##Require at least 10% PSI in all columns.
  idx<-which(rowSums(events>0.10)==82)
  events<-events[idx,]
  idx<-which(rowSums(events_Respirometry>0.10)==54)
  events_Respirometry<-events_Respirometry[idx,]
  events_Respirometry<-events_Respirometry[,match(metadata_Respirometry_muscle_mass3$name,colnames(events_Respirometry))]
  all(colnames(events_Respirometry)==metadata_Respirometry_muscle_mass3$name)
  all(colnames(events_Respirometry)==metadata_Respirometry_muscle_mass3$name)
  annot<-read.delim("Homo_sapiens_HG38_GRCH38_104_Annotation.txt")
  events_universe<-unique(gsub(";.*","",rownames(events)))
  
  events_universe_ENTREZID_id<-mapIds(org.Hs.eg.db,
                                      keys=events_universe, 
                                      column="ENTREZID",
                                      keytype="ENSEMBL",
                                      multiVals="first")
  names(events_universe_ENTREZID_id)<-NULL
  events_Respirometry_universe<-unique(gsub(";.*","",rownames(events_Respirometry)))
  events_Respirometry_universe_ENTREZID_id<-mapIds(org.Hs.eg.db,
                                                   keys=events_Respirometry_universe, 
                                                   column="ENTREZID",
                                                   keytype="ENSEMBL",
                                                   multiVals="first")
  names(events_Respirometry_universe_ENTREZID_id)<-NULL
  
} 

# analysis
{
#Highxmin
{

  all(metadata_muscle_mass3$name==colnames(events))
  beta_vec<-vector()
  p_vec<-vector()
  for(i in 1:nrow(events)){
    data_highxmin<-events[i,]
    new_meta<-metadata_muscle_mass3
    new_meta$data<-data.frame(t(data_highxmin))[,1]
    mod<-lm(data~highxmin+years+phase+sex+FiberRatio,data=new_meta)
    Beta<-summary(mod)$coef[2,1]  
    pvalue<-summary(mod)$coef[2,4]
    beta_vec<-c(beta_vec,Beta)
    p_vec<-c(p_vec,pvalue)
  }
  df_highxmin_muscle_mass3<-data.frame("row.names"=row.names(events),"pvalue"=p_vec,Beta=beta_vec)
  df_highxmin_sig_muscle_mass3<-subset(df_highxmin_muscle_mass3,pvalue<0.01)
  df_highxmin_sig_muscle_mass3$events<-gsub("ENSG.*?;(.*?)\\:.*","\\1",rownames(df_highxmin_sig_muscle_mass3))
  df_highxmin_count_muscle_mass3<-data.frame(table(df_highxmin_sig_muscle_mass3$events))
  colnames(df_highxmin_count_muscle_mass3)<-c("Events","Count")
  df_highxmin_count_muscle_mass3$Proportion<-df_highxmin_count_muscle_mass3$Count/sum(df_highxmin_count_muscle_mass3$Count)
  df_highxmin_count_muscle_mass3$Proportion<-round(100*df_highxmin_count_muscle_mass3$Proportion,2)
  df_highxmin_count_muscle_mass3%>%ggplot(aes(x=Events,y=Count))+geom_bar(stat="identity")+geom_text(aes(label=paste(Proportion,"%",sep=" ")),vjust=-0.2)+
    theme_classic()
  df_highxmin_sig_muscle_mass3$GeneID<-gsub(";.*","",rownames(df_highxmin_sig_muscle_mass3))
  df_highxmin_sig_muscle_mass3$Event_name<-rownames(df_highxmin_sig_muscle_mass3)
  df_highxmin_sig_muscle_mass3<-merge(df_highxmin_sig_muscle_mass3,annot,by.x="GeneID",by.y="Gene_stable_ID")
  df_highxmin_sig_muscle_mass3<-df_highxmin_sig_muscle_mass3[order(df_highxmin_sig_muscle_mass3$pvalue),]
  write.csv(df_highxmin_sig_muscle_mass3,"highxmin_AS_muscle_mass3.csv")
  
  table(df_highxmin_sig_muscle_mass3$events)

  
  df_highxmin_sig_ego_muscle_mass3<-subset(df_highxmin_muscle_mass3,pvalue<0.1)
  
  df_highxmin_sig_ego_genes_muscle_mass3<-gsub(";.*","",rownames(df_highxmin_sig_ego_muscle_mass3))
  df_highxmin_sig_go_genes_muscle_mass3<-enrichGO(df_highxmin_sig_ego_genes_muscle_mass3,org.Hs.eg.db,ont="all",pvalueCutoff = 0.05,keyType = "ENSEMBL",universe=rownames(feature_dat))
  dim(df_highxmin_sig_go_genes_muscle_mass3)
  write.csv(data.frame(df_highxmin_sig_go_genes_muscle_mass3),"highxmin_go_AS_muscle_mass3.csv")
  
  
  highxmin_AS_df<-data.frame(df_highxmin_sig_go_genes_muscle_mass3)
  highxmin_AS_granule<-highxmin_AS_df[grepl("granule",highxmin_AS_df$Description),]
  granule_annot<-data.frame()
  for(i in 1:nrow(highxmin_AS_granule)){
    gene_ids<-strsplit(highxmin_AS_granule[i,]$geneID,"/")[[1]]
    annot_granules<-annot[annot$Gene_stable_ID%in%gene_ids,]
    annot_granules<-annot_granules[match(annot_granules$Gene_stable_ID,gene_ids),]
    annot_granules$Count<-highxmin_AS_granule$Count[i]
    annot_granules$Description<-highxmin_AS_granule$Description[i]
    annot_granules$GO<-highxmin_AS_granule$ID[i]
    annot_granules$Ontology<-highxmin_AS_granule$ONTOLOGY[i]
    annot_granules$pvalue<-highxmin_AS_granule$pvalue[i]
    annot_granules$p.adjust<-highxmin_AS_granule$p.adjust[i]
    annot_granules$Category<-"highxmin"
    
    granule_annot<-rbind(granule_annot,annot_granules)
  }
  
} 
  

#VO2 
{
  all(metadata_muscle_mass3$name==colnames(events))
  beta_vec<-vector()
  p_vec<-vector()
  for(i in 1:nrow(events)){
    data_VO2<-events[i,]
    new_meta<-metadata_muscle_mass3
    new_meta$data<-data.frame(t(data_VO2))[,1]
    mod<-lm(data~VO2+years+phase+sex+FiberRatio,data=new_meta)
    Beta<-summary(mod)$coef[2,1]  
    pvalue<-summary(mod)$coef[2,4]
    beta_vec<-c(beta_vec,Beta)
    p_vec<-c(p_vec,pvalue)
  }
  df_VO2_muscle_mass3<-data.frame("row.names"=row.names(events),"pvalue"=p_vec,Beta=beta_vec)
  df_VO2_sig_muscle_mass3<-subset(df_VO2_muscle_mass3,pvalue<0.01)
  df_VO2_sig_muscle_mass3$events<-gsub("ENSG.*?;(.*?)\\:.*","\\1",rownames(df_VO2_sig_muscle_mass3))
  df_VO2_count_muscle_mass3<-data.frame(table(df_VO2_sig_muscle_mass3$events))
  colnames(df_VO2_count_muscle_mass3)<-c("Events","Count")
  df_VO2_count_muscle_mass3$Proportion<-df_VO2_count_muscle_mass3$Count/sum(df_VO2_count_muscle_mass3$Count)
  df_VO2_count_muscle_mass3$Proportion<-round(100*df_VO2_count_muscle_mass3$Proportion,2)
  df_VO2_count_muscle_mass3%>%ggplot(aes(x=Events,y=Count))+geom_bar(stat="identity")+geom_text(aes(label=paste(Proportion,"%",sep=" ")),vjust=-0.2)+
    theme_classic()
  df_VO2_sig_muscle_mass3$GeneID<-gsub(";.*","",rownames(df_VO2_sig_muscle_mass3))
  df_VO2_sig_muscle_mass3$Event_name<-rownames(df_VO2_sig_muscle_mass3)
  df_VO2_sig_muscle_mass3<-merge(df_VO2_sig_muscle_mass3,annot,by.x="GeneID",by.y="Gene_stable_ID")
  df_VO2_sig_muscle_mass3<-df_VO2_sig_muscle_mass3[order(df_VO2_sig_muscle_mass3$pvalue),]
  write.csv(df_VO2_sig_muscle_mass3,"VO2_AS_muscle_mass3.csv")
  
  table(df_VO2_sig_muscle_mass3$events)

  
  df_VO2_sig_ego_muscle_mass3<-subset(df_VO2_muscle_mass3,pvalue<0.1)
  
  
  df_VO2_sig_ego_genes_muscle_mass3<-gsub(";.*","",rownames(df_VO2_sig_ego_muscle_mass3))
  df_VO2_sig_go_genes_muscle_mass3<-enrichGO(df_VO2_sig_ego_genes_muscle_mass3,org.Hs.eg.db,ont="all",pvalueCutoff = 0.05, keyType = "ENSEMBL",universe=rownames(feature_dat))
  dim(df_VO2_sig_go_genes_muscle_mass3)
  write.csv(data.frame(df_VO2_sig_go_genes_muscle_mass3),"VO2_go_AS_muscle_mass3.csv")
  
  
  VO2_AS_df<-data.frame(df_VO2_sig_go_genes_muscle_mass3)
  VO2_AS_granule<-VO2_AS_df[grepl("granule",VO2_AS_df$Description),]
  
  
  granule_annot<-data.frame()
  for(i in 1:nrow(VO2_AS_granule)){
  gene_ids<-strsplit(VO2_AS_granule[i,]$geneID,"/")[[1]]
  annot_granules<-annot[annot$Gene_stable_ID%in%gene_ids,]
  annot_granules<-annot_granules[match(annot_granules$Gene_stable_ID,gene_ids),]
  annot_granules$Count<-VO2_AS_granule$Count[i]
  annot_granules$Description<-VO2_AS_granule$Description[i]
  annot_granules$GO<-VO2_AS_granule$ID[i]
  annot_granules$Ontology<-VO2_AS_granule$ONTOLOGY[i]
  annot_granules$pvalue<-VO2_AS_granule$pvalue[i]
  annot_granules$p.adjust<-VO2_AS_granule$p.adjust[i]
  
  granule_annot<-rbind(granule_annot,annot_granules)
  }
  granule_annot$Category<-"VO2"
  
}

#kPCr
{
  all(metadata_muscle_mass3$name==colnames(events))
  beta_vec<-vector()
  p_vec<-vector()
  for(i in 1:nrow(events)){
    data_kPCr<-events[i,]
    new_meta<-metadata_muscle_mass3
    new_meta$data<-data.frame(t(data_kPCr))[,1]
    mod<-lm(data~kPCr+years+phase+sex+FiberRatio,data=new_meta)
    Beta<-summary(mod)$coef[2,1]  
    pvalue<-summary(mod)$coef[2,4]
    beta_vec<-c(beta_vec,Beta)
    p_vec<-c(p_vec,pvalue)
  }
  df_kPCr_muscle_mass3<-data.frame("row.names"=row.names(events),"pvalue"=p_vec,Beta=beta_vec)
  df_kPCr_sig_muscle_mass3<-subset(df_kPCr_muscle_mass3,pvalue<0.01)
  df_kPCr_sig_muscle_mass3$events<-gsub("ENSG.*?;(.*?)\\:.*","\\1",rownames(df_kPCr_sig_muscle_mass3))
  df_kPCr_count_muscle_mass3<-data.frame(table(df_kPCr_sig_muscle_mass3$events))
  colnames(df_kPCr_count_muscle_mass3)<-c("Events","Count")
  df_kPCr_count_muscle_mass3$Proportion<-df_kPCr_count_muscle_mass3$Count/sum(df_kPCr_count_muscle_mass3$Count)
  df_kPCr_count_muscle_mass3$Proportion<-round(100*df_kPCr_count_muscle_mass3$Proportion,2)
  df_kPCr_count_muscle_mass3%>%ggplot(aes(x=Events,y=Count))+geom_bar(stat="identity")+geom_text(aes(label=paste(Proportion,"%",sep=" ")),vjust=-0.2)+
    theme_classic()
  df_kPCr_sig_muscle_mass3$GeneID<-gsub(";.*","",rownames(df_kPCr_sig_muscle_mass3))
  df_kPCr_sig_muscle_mass3$Event_name<-rownames(df_kPCr_sig_muscle_mass3)
  df_kPCr_sig_muscle_mass3<-merge(df_kPCr_sig_muscle_mass3,annot,by.x="GeneID",by.y="Gene_stable_ID")
  df_kPCr_sig_muscle_mass3<-df_kPCr_sig_muscle_mass3[order(df_kPCr_sig_muscle_mass3$pvalue),]
  write.csv(df_kPCr_sig_muscle_mass3,"kPCr_AS_muscle_mass3.csv")
  
  table(df_kPCr_sig_muscle_mass3$events)

  df_kPCr_sig_ego_muscle_mass3<-subset(df_kPCr_muscle_mass3,pvalue<0.1)
  df_kPCr_sig_ego_genes_muscle_mass3<-gsub(";.*","",rownames(df_kPCr_sig_ego_muscle_mass3))
  df_kPCr_sig_go_genes_muscle_mass3<-enrichGO(df_kPCr_sig_ego_genes_muscle_mass3,org.Hs.eg.db,ont="all",pvalueCutoff = 0.05, keyType = "ENSEMBL",universe=rownames(feature_dat))
  dim(df_kPCr_sig_go_genes_muscle_mass3)
  write.csv(data.frame(df_kPCr_sig_go_genes_muscle_mass3),"kPCr_go_AS_muscle_mass3.csv")

    
  kPCr_AS_df<-data.frame(df_kPCr_sig_go_genes_muscle_mass3)
  kPCr_AS_granule<-kPCr_AS_df[grepl("granule",kPCr_AS_df$Description),]
  
  for(i in 1:nrow(kPCr_AS_granule)){
    gene_ids<-strsplit(kPCr_AS_granule[i,]$geneID,"/")[[1]]
    annot_granules<-annot[annot$Gene_stable_ID%in%gene_ids,]
    annot_granules<-annot_granules[match(annot_granules$Gene_stable_ID,gene_ids),]
    annot_granules$Count<-kPCr_AS_granule$Count[i]
    annot_granules$Description<-kPCr_AS_granule$Description[i]
    annot_granules$GO<-kPCr_AS_granule$ID[i]
    annot_granules$Ontology<-kPCr_AS_granule$ONTOLOGY[i]
    annot_granules$pvalue<-kPCr_AS_granule$pvalue[i]
    annot_granules$p.adjust<-kPCr_AS_granule$p.adjust[i]
    annot_granules$Category<-"kPCr"
    
    granule_annot<-rbind(granule_annot,annot_granules)
  }
  
  
  

}

#X5ADP Respirometry
{
  all(metadata_Respirometry_muscle_mass3$name==colnames(events_Respirometry))
  beta_vec<-vector()
  p_vec<-vector()
  for(i in 1:nrow(events)){
    data_Respirometry<-events_Respirometry[i,]
    new_meta<-metadata_Respirometry_muscle_mass3
    new_meta$data<-data.frame(t(data_Respirometry))[,1]
    mod<-lm(data~X5ADP+years+phase+sex+FiberRatio,data=new_meta)
    Beta<-summary(mod)$coef[2,1]  
    pvalue<-summary(mod)$coef[2,4]
    beta_vec<-c(beta_vec,Beta)
    p_vec<-c(p_vec,pvalue)
  }
  df_Respirometry_muscle_mass3<-data.frame("row.names"=row.names(events),"pvalue"=p_vec,Beta=beta_vec)
  df_Respirometry_sig_muscle_mass3<-subset(df_Respirometry_muscle_mass3,pvalue<0.01)
  df_Respirometry_sig_muscle_mass3$events<-gsub("ENSG.*?;(.*?)\\:.*","\\1",rownames(df_Respirometry_sig_muscle_mass3))
  df_Respirometry_count_muscle_mass3<-data.frame(table(df_Respirometry_sig_muscle_mass3$events))
  colnames(df_Respirometry_count_muscle_mass3)<-c("Events","Count")
  df_Respirometry_count_muscle_mass3$Proportion<-df_Respirometry_count_muscle_mass3$Count/sum(df_Respirometry_count_muscle_mass3$Count)
  df_Respirometry_count_muscle_mass3$Proportion<-round(100*df_Respirometry_count_muscle_mass3$Proportion,2)
  df_Respirometry_count_muscle_mass3%>%ggplot(aes(x=Events,y=Count))+geom_bar(stat="identity")+geom_text(aes(label=paste(Proportion,"%",sep=" ")),vjust=-0.2)+
    theme_classic()
  df_Respirometry_sig_muscle_mass3$GeneID<-gsub(";.*","",rownames(df_Respirometry_sig_muscle_mass3))
  df_Respirometry_sig_muscle_mass3$Event_name<-rownames(df_Respirometry_sig_muscle_mass3)
  df_Respirometry_sig_muscle_mass3<-merge(df_Respirometry_sig_muscle_mass3,annot,by.x="GeneID",by.y="Gene_stable_ID")
  df_Respirometry_sig_muscle_mass3<-df_Respirometry_sig_muscle_mass3[order(df_Respirometry_sig_muscle_mass3$pvalue),]
  write.csv(df_Respirometry_sig_muscle_mass3,"Respirometry_AS_muscle_mass3.csv")
  
  table(df_Respirometry_sig_muscle_mass3$events)

    df_Respirometry_sig_ego_muscle_mass3<-subset(df_Respirometry_muscle_mass3,pvalue<0.1)
  df_Respirometry_sig_ego_genes_muscle_mass3<-gsub(";.*","",rownames(df_Respirometry_sig_ego_muscle_mass3))
  df_Respirometry_sig_go_genes_muscle_mass3<-enrichGO(df_Respirometry_sig_ego_genes_muscle_mass3,org.Hs.eg.db,ont="all",pvalueCutoff = 0.05, keyType = "ENSEMBL",universe=rownames(feature_dat_Respirometry))
  dim(df_Respirometry_sig_go_genes_muscle_mass3)
  write.csv(data.frame(df_Respirometry_sig_go_genes_muscle_mass3),"Respirometry_go_AS_muscle_mass3.csv")
  
  Respirometry_AS_df<-data.frame(df_Respirometry_sig_go_genes_muscle_mass3)
  Respirometry_AS_granule<-Respirometry_AS_df[grepl("granule",Respirometry_AS_df$Description),]
  
  for(i in 1:nrow(Respirometry_AS_granule)){
    gene_ids<-strsplit(Respirometry_AS_granule[i,]$geneID,"/")[[1]]
    annot_granules<-annot[annot$Gene_stable_ID%in%gene_ids,]
    annot_granules<-annot_granules[match(annot_granules$Gene_stable_ID,gene_ids),]
    annot_granules$Count<-Respirometry_AS_granule$Count[i]
    annot_granules$Description<-Respirometry_AS_granule$Description[i]
    annot_granules$GO<-Respirometry_AS_granule$ID[i]
    annot_granules$Ontology<-Respirometry_AS_granule$ONTOLOGY[i]
    annot_granules$pvalue<-Respirometry_AS_granule$pvalue[i]
    annot_granules$p.adjust<-Respirometry_AS_granule$p.adjust[i]
    annot_granules$Category<-"Respirometry"
    
    granule_annot<-rbind(granule_annot,annot_granules)
  }
  
  write.csv(granule_annot,"Granule_data.csv")
  

  
  }

#VO2 high
{
    events_high<-events[,colnames(events)%in%metadata_VO2_high$name]
    all(metadata_VO2_high$name==colnames(events_high))
    beta_vec<-vector()
    p_vec<-vector()
    for(i in 1:nrow(events_high)){
      data_VO2_high<-events_high[i,]
      new_meta<-metadata_VO2_high
      new_meta$data<-data.frame(t(data_VO2_high))[,1]
      mod<-lm(data~years+phase+sex+FiberRatio,data=new_meta)
      Beta<-summary(mod)$coef[2,1]  
      pvalue<-summary(mod)$coef[2,4]
      beta_vec<-c(beta_vec,Beta)
      p_vec<-c(p_vec,pvalue)
    }
    df_VO2_high<-data.frame("row.names"=row.names(events),"pvalue"=p_vec,Beta=beta_vec)
    df_VO2_sig_high<-subset(df_VO2_high,pvalue<0.01)
    df_VO2_sig_high$events<-gsub("ENSG.*?;(.*?)\\:.*","\\1",rownames(df_VO2_sig_high))
    df_VO2_count_high<-data.frame(table(df_VO2_sig_high$events))
    colnames(df_VO2_count_high)<-c("Events","Count")
    df_VO2_count_high$Proportion<-df_VO2_count_high$Count/sum(df_VO2_count_high$Count)
    df_VO2_count_high$Proportion<-round(100*df_VO2_count_high$Proportion,2)
    df_VO2_count_high%>%ggplot(aes(x=Events,y=Count))+geom_bar(stat="identity")+geom_text(aes(label=paste(Proportion,"%",sep=" ")),vjust=-0.2)+
      theme_classic()
    df_VO2_sig_high$GeneID<-gsub(";.*","",rownames(df_VO2_sig_high))
    df_VO2_sig_high$Event_name<-rownames(df_VO2_sig_high)
    df_VO2_sig_high<-merge(df_VO2_sig_high,annot,by.x="GeneID",by.y="Gene_stable_ID")
    df_VO2_sig_high<-df_VO2_sig_high[order(df_VO2_sig_high$pvalue),]
    write.csv(df_VO2_sig_high,"VO2_AS_high.csv")
    
    table(df_VO2_sig_high$events)
    #table(df_VO2_sig$events)
    #table(df_kPCr_sig$events)
    #table(df_Respirometry_sig$events)
    
    
    df_VO2_sig_ego_high<-subset(df_VO2_high,pvalue<0.1)
    
    
    df_VO2_sig_ego_genes_high<-gsub(";.*","",rownames(df_VO2_sig_ego_high))
    df_VO2_sig_go_genes_high<-enrichGO(df_VO2_sig_ego_genes_high,org.Hs.eg.db,ont="all",pvalueCutoff = 0.05, keyType = "ENSEMBL",universe=rownames(feature_dat))
    dim(df_VO2_sig_go_genes_high)
    write.csv(data.frame(df_VO2_sig_go_genes_high),"VO2_go_AS_high.csv")
    
    
  }
  
#VO2 low
{
    events_low<-events[,colnames(events)%in%metadata_VO2_low$name]
    all(metadata_VO2_low$name==colnames(events_low))
    beta_vec<-vector()
    p_vec<-vector()
    for(i in 1:nrow(events_low)){
      data_VO2_low<-events_low[i,]
      new_meta<-metadata_VO2_low
      new_meta$data<-data.frame(t(data_VO2_low))[,1]
      mod<-lm(data~years+phase+sex+FiberRatio,data=new_meta)
      Beta<-summary(mod)$coef[2,1]  
      pvalue<-summary(mod)$coef[2,4]
      beta_vec<-c(beta_vec,Beta)
      p_vec<-c(p_vec,pvalue)
    }
    df_VO2_low<-data.frame("row.names"=row.names(events),"pvalue"=p_vec,Beta=beta_vec)
    df_VO2_sig_low<-subset(df_VO2_low,pvalue<0.01)
    df_VO2_sig_low$events<-gsub("ENSG.*?;(.*?)\\:.*","\\1",rownames(df_VO2_sig_low))
    df_VO2_count_low<-data.frame(table(df_VO2_sig_low$events))
    colnames(df_VO2_count_low)<-c("Events","Count")
    df_VO2_count_low$Proportion<-df_VO2_count_low$Count/sum(df_VO2_count_low$Count)
    df_VO2_count_low$Proportion<-round(100*df_VO2_count_low$Proportion,2)
    df_VO2_count_low%>%ggplot(aes(x=Events,y=Count))+geom_bar(stat="identity")+geom_text(aes(label=paste(Proportion,"%",sep=" ")),vjust=-0.2)+
      theme_classic()
    df_VO2_sig_low$GeneID<-gsub(";.*","",rownames(df_VO2_sig_low))
    df_VO2_sig_low$Event_name<-rownames(df_VO2_sig_low)
    df_VO2_sig_low<-merge(df_VO2_sig_low,annot,by.x="GeneID",by.y="Gene_stable_ID")
    df_VO2_sig_low<-df_VO2_sig_low[order(df_VO2_sig_low$pvalue),]
    write.csv(df_VO2_sig_low,"VO2_AS_low.csv")
    
    table(df_VO2_sig_low$events)

    df_VO2_sig_ego_low<-subset(df_VO2_low,pvalue<0.1)
    
    
    df_VO2_sig_ego_genes_low<-gsub(";.*","",rownames(df_VO2_sig_ego_low))
    df_VO2_sig_go_genes_low<-enrichGO(df_VO2_sig_ego_genes_low,org.Hs.eg.db,ont="all",pvalueCutoff = 0.05, keyType = "ENSEMBL",universe=rownames(feature_dat))
    dim(df_VO2_sig_go_genes_low)
    write.csv(data.frame(df_VO2_sig_go_genes_low),"VO2_go_AS_low.csv")
    
    
  }
  
}
  
# AS overlap
{
    highxmin_AS_for_dotplot<-read.csv("highxmin_AS_muscle_mass3.csv")
    highxmin_AS_for_dotplot<-highxmin_AS_for_dotplot|>dplyr::filter(Gene_name!="")
    highxmin_AS_for_dotplot_mean<-highxmin_AS_for_dotplot|>group_by(Gene_name)|>summarize(pvalue=mean(pvalue),Beta=mean(Beta))
    highxmin_AS_for_dotplot_mean$ID<-"highxmin"
    highxmin_AS_for_dotplot_mean_up<-highxmin_AS_for_dotplot_mean[highxmin_AS_for_dotplot_mean$Beta>0,]
    highxmin_AS_for_dotplot_mean_down<-highxmin_AS_for_dotplot_mean[highxmin_AS_for_dotplot_mean$Beta<0,]
    
    VO2_AS_for_dotplot<-read.csv("VO2_AS_muscle_mass3.csv")
    VO2_AS_for_dotplot<-VO2_AS_for_dotplot|>dplyr::filter(Gene_name!="")
    VO2_AS_for_dotplot_mean<-VO2_AS_for_dotplot|>group_by(Gene_name)|>summarize(pvalue=mean(pvalue),Beta=mean(Beta))
    VO2_AS_for_dotplot_mean$ID<-"VO2"
    VO2_AS_for_dotplot_mean_up<-VO2_AS_for_dotplot_mean[VO2_AS_for_dotplot_mean$Beta>0,]
    VO2_AS_for_dotplot_mean_down<-VO2_AS_for_dotplot_mean[VO2_AS_for_dotplot_mean$Beta<0,]
    
    kPCr_AS_for_dotplot<-read.csv("kPCr_AS_muscle_mass3.csv")
    kPCr_AS_for_dotplot<-kPCr_AS_for_dotplot|>dplyr::filter(Gene_name!="")
    kPCr_AS_for_dotplot_mean<-kPCr_AS_for_dotplot|>group_by(Gene_name)|>summarize(pvalue=mean(pvalue),Beta=mean(Beta))
    kPCr_AS_for_dotplot_mean$ID<-"kPCr"
    kPCr_AS_for_dotplot_mean_up<-kPCr_AS_for_dotplot_mean[kPCr_AS_for_dotplot_mean$Beta>0,]
    kPCr_AS_for_dotplot_mean_down<-kPCr_AS_for_dotplot_mean[kPCr_AS_for_dotplot_mean$Beta<0,]
    
    X5ADP_AS_for_dotplot<-read.csv("Respirometry_AS_muscle_mass3.csv")
    X5ADP_AS_for_dotplot<-X5ADP_AS_for_dotplot|>dplyr::filter(Gene_name!="")
    X5ADP_AS_for_dotplot_mean<-X5ADP_AS_for_dotplot|>group_by(Gene_name)|>summarize(pvalue=mean(pvalue),Beta=mean(Beta))
    X5ADP_AS_for_dotplot_mean$ID<-"X5ADP"
    X5ADP_AS_for_dotplot_mean_up<-X5ADP_AS_for_dotplot_mean[X5ADP_AS_for_dotplot_mean$Beta>0,]
    X5ADP_AS_for_dotplot_mean_down<-X5ADP_AS_for_dotplot_mean[X5ADP_AS_for_dotplot_mean$Beta<0,]
    
    
    
    AS_Gene_list_up<-rbindlist(list(highxmin_AS_for_dotplot_mean_up,
                                 VO2_AS_for_dotplot_mean_up,
                                 kPCr_AS_for_dotplot_mean_up,
                                 X5ADP_AS_for_dotplot_mean_up))
    
    AS_Gene_list_down<-rbindlist(list(highxmin_AS_for_dotplot_mean_down,
                                    VO2_AS_for_dotplot_mean_down,
                                    kPCr_AS_for_dotplot_mean_down,
                                    X5ADP_AS_for_dotplot_mean_down))
    
    
    AS_Gene_list_final_up<-AS_Gene_list_up%>%group_by(Gene_name)%>%summarize(n=n())
    AS_Gene_list_final_up<-AS_Gene_list_final_up[AS_Gene_list_final_up$n>2,]
    AS_Gene_finalized_up<-AS_Gene_list_up[AS_Gene_list_up$Gene_name%in%AS_Gene_list_final_up$Gene_name,]

    AS_Gene_list_final_down<-AS_Gene_list_down%>%group_by(Gene_name)%>%summarize(n=n())
    AS_Gene_list_final_down<-AS_Gene_list_final_down[AS_Gene_list_final_down$n>2,]
    AS_Gene_finalized_down<-AS_Gene_list_down[AS_Gene_list_down$Gene_name%in%AS_Gene_list_final_down$Gene_name,]
    
    AS_Gene_finalized<-rbind(AS_Gene_finalized_up,AS_Gene_finalized_down)
    common_AS_Gene<-unique(AS_Gene_finalized$Gene_name)
    
    AS_Gene_levels<-unique(AS_Gene_finalized$Gene_name)

    AS_Gene_finalized$ID<-factor(AS_Gene_finalized$ID,levels=c("highxmin","VO2","kPCr","X5ADP"))
    
    AS_Gene_finalized$Beta_sign<-"+"
    AS_Gene_finalized[AS_Gene_finalized$Beta<0,]$Beta_sign<-"-"
    AS_Gene_finalized<-AS_Gene_finalized|>group_by(Gene_name)|>mutate(mn=mean(Beta))
    AS_Gene_finalized<-AS_Gene_finalized[order(-AS_Gene_finalized$mn),]
    AS_Gene_finalized$Gene_name<-factor(AS_Gene_finalized$Gene_name,levels=unique(AS_Gene_finalized$Gene_name))
    AS_Gene_finalized%>%ggplot(aes(x=ID,y=Gene_name))+
  
      #scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
      geom_point(aes(size=pvalue),color="grey")+
      theme_minimal()+
      #scale_color_gradient2(low = 'blue',mid="red",high = 'red')+
      theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=13))+
      scale_y_discrete(labels=toupper)+
      scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "X5ADP"="Mit-O2flux"))+
      scale_color_discrete(guide="none")+
      scale_color_manual(values=c("grey","black"))+
      scale_size(trans="reverse")
    
    #AS_Gene_finalized
    ggsave("AS_Gene_finalized_muscle_mass3.pdf",height=7,width=10)
    
    ##Top 5
    highxmin_AS_for_dotplot_mean<-highxmin_AS_for_dotplot_mean[order(highxmin_AS_for_dotplot_mean$pvalue),]
    VO2_AS_for_dotplot_mean<-VO2_AS_for_dotplot_mean[order(VO2_AS_for_dotplot_mean$pvalue),]
    kPCr_AS_for_dotplot_mean<-kPCr_AS_for_dotplot_mean[order(kPCr_AS_for_dotplot_mean$pvalue),]
    X5ADP_AS_for_dotplot_mean<-X5ADP_AS_for_dotplot_mean[order(X5ADP_AS_for_dotplot_mean$pvalue),]
    n<-5
    AS_Gene_list<-rbindlist(list(highxmin_AS_for_dotplot_mean[1:n,],
                                    VO2_AS_for_dotplot_mean[1:n,],
                                    kPCr_AS_for_dotplot_mean[1:n,],
                                    X5ADP_AS_for_dotplot_mean[1:n,]))
    
    AS_Gene_list_final<-AS_Gene_list%>%group_by(Gene_name)%>%summarize(n=n())
    AS_Gene_list_final<-AS_Gene_list_final[AS_Gene_list_final$n>0,]
    AS_Gene_finalized<-AS_Gene_list[AS_Gene_list$Gene_name%in%AS_Gene_list_final$Gene_name,]
    
    common_AS_Gene<-unique(AS_Gene_finalized$Gene_name)
    
    AS_Gene_levels<-unique(AS_Gene_finalized$Gene_name)
    
    AS_Gene_finalized$ID<-factor(AS_Gene_finalized$ID,levels=c("highxmin","VO2","kPCr","X5ADP"))
    
    AS_Gene_finalized$Beta_sign<-"+"
    AS_Gene_finalized[AS_Gene_finalized$Beta<0,]$Beta_sign<-"-"
    AS_Gene_finalized<-AS_Gene_finalized|>group_by(Gene_name)|>mutate(mn=mean(Beta))
    AS_Gene_finalized<-AS_Gene_finalized[order(-AS_Gene_finalized$mn),]
    AS_Gene_finalized$Gene_name<-factor(AS_Gene_finalized$Gene_name,levels=unique(AS_Gene_finalized$Gene_name))
    AS_Gene_finalized%>%ggplot(aes(x=ID,y=Gene_name))+
      #scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
      geom_point(aes(size=pvalue),col="grey")+
      theme_minimal()+
      #scale_color_gradient2(low = 'blue',mid="red",high = 'red')+
      theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=13))+
      scale_y_discrete(labels=toupper)+
      scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "X5ADP"="Mit-O2flux"))+
      scale_color_discrete(guide="none")+
      #scale_color_manual(values=c("red","blue"))+
      scale_size(trans="reverse")
    
    
    
    
    
    
}

# AS Gene Dotplot
{
    VO2_high_AS_for_dotplot<-read.csv("VO2_AS_high.csv")
    VO2_high_AS_for_dotplot<-VO2_high_AS_for_dotplot|>dplyr::filter(Gene_name!="")
    VO2_high_AS_for_dotplot_mean<-VO2_high_AS_for_dotplot|>group_by(Gene_name)|>summarize(pvalue=mean(pvalue),Beta=mean(Beta))
    VO2_high_AS_for_dotplot_mean$ID<-"High"
    VO2_high_AS_for_dotplot_mean_up<-VO2_high_AS_for_dotplot_mean[VO2_high_AS_for_dotplot_mean$Beta>0,]
    VO2_high_AS_for_dotplot_mean_down<-VO2_high_AS_for_dotplot_mean[VO2_high_AS_for_dotplot_mean$Beta<0,]
    
    VO2_low_AS_for_dotplot<-read.csv("VO2_AS_low.csv")
    VO2_low_AS_for_dotplot<-VO2_low_AS_for_dotplot|>dplyr::filter(Gene_name!="")
    VO2_low_AS_for_dotplot_mean<-VO2_low_AS_for_dotplot|>group_by(Gene_name)|>summarize(pvalue=mean(pvalue),Beta=mean(Beta))
    VO2_low_AS_for_dotplot_mean$ID<-"Low"
    VO2_low_AS_for_dotplot_mean_up<-VO2_low_AS_for_dotplot_mean[VO2_low_AS_for_dotplot_mean$Beta>0,]
    VO2_low_AS_for_dotplot_mean_down<-VO2_low_AS_for_dotplot_mean[VO2_low_AS_for_dotplot_mean$Beta<0,]
    
    
    AS_Gene_list_up<-rbindlist(list(VO2_high_AS_for_dotplot_mean_up,
                                    VO2_low_AS_for_dotplot_mean_up))
    
    AS_Gene_list_down<-rbindlist(list(VO2_high_AS_for_dotplot_mean_down,
                                    VO2_low_AS_for_dotplot_mean_down))
    
        
    
    AS_Gene_list_final_up<-AS_Gene_list_up%>%group_by(Gene_name)%>%summarize(n=n())
    AS_Gene_list_final_up<-AS_Gene_list_final_up[AS_Gene_list_final_up$n>1,]
    AS_Gene_finalized_up<-AS_Gene_list_up[AS_Gene_list_up$Gene_name%in%AS_Gene_list_final_up$Gene_name,]
    
    AS_Gene_list_final_down<-AS_Gene_list_down%>%group_by(Gene_name)%>%summarize(n=n())
    AS_Gene_list_final_down<-AS_Gene_list_final_down[AS_Gene_list_final_down$n>1,]
    AS_Gene_finalized_down<-AS_Gene_list_down[AS_Gene_list_down$Gene_name%in%AS_Gene_list_final_down$Gene_name,]
    
    AS_Gene_finalized<-rbind(AS_Gene_finalized_up,AS_Gene_finalized_down)
    common_AS_Gene<-unique(AS_Gene_finalized$Gene_name)
    
    AS_Gene_levels<-unique(AS_Gene_finalized$Gene_name)
    
    AS_Gene_finalized$ID<-factor(AS_Gene_finalized$ID,levels=c("Low","High"))
    
    AS_Gene_finalized$Beta_sign<-"+"
    AS_Gene_finalized[AS_Gene_finalized$Beta<0,]$Beta_sign<-"-"
    AS_Gene_finalized<-AS_Gene_finalized|>group_by(Gene_name)|>mutate(mn=mean(Beta))
    AS_Gene_finalized<-AS_Gene_finalized[order(-AS_Gene_finalized$mn),]
    AS_Gene_finalized$Gene_name<-factor(AS_Gene_finalized$Gene_name,levels=unique(AS_Gene_finalized$Gene_name))
    AS_Gene_finalized%>%ggplot(aes(x=ID,y=Gene_name))+
      
      #scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
      geom_point(aes(size=pvalue,color=Beta_sign))+
      theme_minimal()+
      #scale_color_gradient2(low = 'blue',mid="red",high = 'red')+
      theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=13))+
      scale_y_discrete(labels=toupper)+
      #scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "Respirometry"="Mit-O2flux"))+
      scale_color_discrete(guide="none")+
      #scale_color_manual(values=c("red","blue"))+
      scale_size(trans="reverse")
    
    #AS_Gene_finalized
    ggsave("AS_Gene_High_Low_Overlap.pdf",height=7,width=10)
    
    ##Top 5
    VO2_high_AS_for_dotplot_mean<-VO2_high_AS_for_dotplot_mean[order(VO2_high_AS_for_dotplot_mean$pvalue),]
    VO2_low_AS_for_dotplot_mean<-VO2_low_AS_for_dotplot_mean[order(VO2_low_AS_for_dotplot_mean$pvalue),]
    
    n<-25
    AS_Gene_list<-rbindlist(list(VO2_high_AS_for_dotplot_mean[1:n,],
                                 VO2_low_AS_for_dotplot_mean[1:n,]))
    
    AS_Gene_list_final<-AS_Gene_list%>%group_by(Gene_name)%>%summarize(n=n())
    AS_Gene_list_final<-AS_Gene_list_final[AS_Gene_list_final$n>0,]
    AS_Gene_finalized<-AS_Gene_list[AS_Gene_list$Gene_name%in%AS_Gene_list_final$Gene_name,]
    
    common_AS_Gene<-unique(AS_Gene_finalized$Gene_name)
    
    AS_Gene_levels<-unique(AS_Gene_finalized$Gene_name)
    
    AS_Gene_finalized$ID<-factor(AS_Gene_finalized$ID,levels=c("Low","High"))
    
    AS_Gene_finalized$Beta_sign<-"+"
    AS_Gene_finalized[AS_Gene_finalized$Beta<0,]$Beta_sign<-"-"
    AS_Gene_finalized<-AS_Gene_finalized|>group_by(Gene_name)|>mutate(mn=mean(Beta))
    AS_Gene_finalized<-AS_Gene_finalized[order(-AS_Gene_finalized$mn),]
    AS_Gene_finalized$Gene_name<-factor(AS_Gene_finalized$Gene_name,levels=unique(AS_Gene_finalized$Gene_name))
    AS_Gene_finalized%>%ggplot(aes(x=ID,y=Gene_name))+
      #scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
      geom_point(aes(size=pvalue,color=Beta_sign))+
      theme_minimal()+
      #scale_color_gradient2(low = 'blue',mid="red",high = 'red')+
      theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=13))+
      scale_y_discrete(labels=toupper)+
      scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "Respirometry"="Mit-O2flux"))+
      scale_color_discrete(guide="none")+
      #scale_color_manual(values=c("red","blue"))+
      scale_size(trans="reverse")
    
    
    
    
    
    
}
}

#Analysis for Ontology Dotplot (for splicing)
{  
  #load
  {

  ORA_highxmin_muscle_mass3<-read.csv("highxmin_go_AS_muscle_mass3.csv")
  dim(ORA_highxmin_muscle_mass3)
  ORA_highxmin_muscle_mass3$Comparison<-"highxmin"
  
  ORA_VO2_muscle_mass3<-read.csv("VO2_go_AS_muscle_mass3.csv")
  dim(ORA_VO2_muscle_mass3)
  ORA_VO2_muscle_mass3$Comparison<-"VO2"
  
  ORA_kPCr_muscle_mass3<-read.csv("kPCr_go_AS_muscle_mass3.csv") 
  ORA_kPCr_muscle_mass3$Comparison<-"kPCr"  
  dim(ORA_kPCr_muscle_mass3)
  
  ORA_Respirometry_muscle_mass3<-read.csv("Respirometry_go_AS_muscle_mass3.csv") 
  dim(ORA_Respirometry_muscle_mass3)
  ORA_Respirometry_muscle_mass3$Comparison<-"Respirometry"
  
  
  ORA_VO2_high<-read.csv("VO2_go_AS_high.csv")
  dim(ORA_VO2_high)
  ORA_VO2_high$Comparison<-"VO2_high"
  
  ORA_VO2_low<-read.csv("VO2_go_AS_low.csv")
  dim(ORA_VO2_low)
  ORA_VO2_low$Comparison<-"VO2_low"
  

}

  ##Figures 4d and 4e.  Also figures S14.
  library(data.table)
  
  ORA_list<-rbindlist(list(ORA_highxmin_muscle_mass3,ORA_VO2_muscle_mass3,ORA_kPCr_muscle_mass3,ORA_Respirometry_muscle_mass3))
  ORA_list_final<-ORA_list%>%group_by(ID)%>%summarize(n=n())
  ORA_list_final<-ORA_list_final[ORA_list_final$n>2,]
  ORA_finalized<-ORA_list[ORA_list$ID%in%ORA_list_final$ID,]
  common_ORA<-unique(ORA_finalized$ID)
  
  ORA_levels<-unique(ORA_finalized$Description)
  ORA_mean<-ORA_finalized%>%group_by(Description)%>%summarize(mn=mean(p.adjust))
  ORA_mean<-ORA_mean[order(-ORA_mean$mn),]
  ORA_finalized$Description<-factor(ORA_finalized$Description,levels=ORA_mean$Description)
  
  ORA_finalized$Comparison<-factor(ORA_finalized$Comparison,levels=c("highxmin","VO2","kPCr","Respirometry"))
  
  ORA_finalized$numerator<-as.numeric(gsub("(\\d+)\\/.*","\\1",ORA_finalized$GeneRatio))
  ORA_finalized$denominator<-as.numeric(gsub("\\d+\\/(.*)","\\1",ORA_finalized$GeneRatio))
  ORA_finalized$GeneRatio<-ORA_finalized$numerator/ORA_finalized$denominator
  #Figure 4e
  ORA_finalized%>%ggplot(aes(x=Comparison,y=Description))+
    #scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
    geom_point(aes(col=p.adjust,size=GeneRatio))+
    theme_minimal()+
    scale_color_gradient2(low = 'red', mid = 'red', high = 'lightyellow')+
    theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=13))+
    scale_y_discrete(labels=toupper)+
    scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "Respirometry"="Mit-O2flux"))
  ORA_finalized
  ggsave("ORA_overlap_muscle_mass3.pdf",height=7,width=10)

###ORA top 20 each
  ORA_highxmin_muscle_mass3_2<-head(ORA_highxmin_muscle_mass3,5)
  ORA_VO2_muscle_mass3_2<-head(ORA_VO2_muscle_mass3,5)
  ORA_kPCr_muscle_mass3_2<-head(ORA_kPCr_muscle_mass3,5)
  ORA_Respirometry_muscle_mass3_2<-head(ORA_Respirometry_muscle_mass3,5)
  ORA_list<-rbindlist(list(ORA_highxmin_muscle_mass3_2,ORA_VO2_muscle_mass3_2,
                           ORA_kPCr_muscle_mass3_2,ORA_Respirometry_muscle_mass3_2))
  ORA_list_final<-ORA_list%>%group_by(ID)%>%summarize(n=n())
  ORA_list_final<-ORA_list_final[ORA_list_final$n>0,]
  ORA_finalized<-ORA_list[ORA_list$ID%in%ORA_list_final$ID,]
  common_ORA<-unique(ORA_finalized$ID)
  
  ORA_levels<-unique(ORA_finalized$Description)
  ORA_mean<-ORA_finalized%>%group_by(Description)%>%summarize(mn=mean(p.adjust))
  ORA_mean<-ORA_mean[order(-ORA_mean$mn),]
  ORA_finalized$Description<-factor(ORA_finalized$Description,levels=ORA_mean$Description)
  
  ORA_finalized$Comparison<-factor(ORA_finalized$Comparison,levels=c("highxmin","VO2","kPCr","Respirometry"))
  
  ORA_finalized$numerator<-as.numeric(gsub("(\\d+)\\/.*","\\1",ORA_finalized$GeneRatio))
  ORA_finalized$denominator<-as.numeric(gsub("\\d+\\/(.*)","\\1",ORA_finalized$GeneRatio))
  ORA_finalized$GeneRatio<-ORA_finalized$numerator/ORA_finalized$denominator
  #Figure 4d
  ORA_finalized%>%ggplot(aes(x=Comparison,y=Description))+
    #scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
    geom_point(aes(col=p.adjust,size=GeneRatio))+
    theme_minimal()+
    scale_color_gradient2(low = 'red', mid = 'red', high = 'lightyellow')+
    theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=13))+
    scale_y_discrete(labels=toupper)+
    scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "Respirometry"="Mit-O2flux"))
  ORA_finalized
  ggsave("ORA_overlap_muscle_mass3.pdf",height=7,width=10)
  
  
ORA_VO2_low<-head(ORA_VO2_low, 200)
ORA_VO2_high<-head(ORA_VO2_high,200)
ORA_list<-rbindlist(list(ORA_VO2_low,ORA_VO2_high))
ORA_list_final<-ORA_list%>%group_by(ID)%>%summarize(n=n())
ORA_list_final<-ORA_list_final[ORA_list_final$n>1,]
ORA_finalized<-ORA_list[ORA_list$ID%in%ORA_list_final$ID,]

common_ORA<-unique(ORA_finalized$ID)
ORA_levels<-unique(ORA_finalized$Description)
ORA_mean<-ORA_finalized%>%group_by(Description)%>%summarize(mn=mean(p.adjust))
ORA_mean<-ORA_mean[order(-ORA_mean$mn),]
ORA_finalized$Description<-factor(ORA_finalized$Description,levels=ORA_mean$Description)
ORA_finalized$Comparison<-factor(ORA_finalized$Comparison,levels=c("VO2_low", "VO2_high"))

ORA_finalized$numerator<-as.numeric(gsub("(\\d+)\\/.*","\\1",ORA_finalized$GeneRatio))
ORA_finalized$denominator<-as.numeric(gsub("\\d+\\/(.*)","\\1",ORA_finalized$GeneRatio))
ORA_finalized$GeneRatio<-ORA_finalized$numerator/ORA_finalized$denominator

#Common between VO2 high and low. Figure S14
ORA_finalized%>%ggplot(aes(x=Comparison,y=Description))+
#scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
geom_point(aes(col=p.adjust,size=GeneRatio))+
theme_minimal()+
scale_color_gradient2(low = 'red', mid = 'red', high = 'lightyellow')+
theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=10.5))+
scale_y_discrete(labels=toupper)+
  scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "Respirometry"="Mit-O2flux"))
ORA_finalized

ggsave("ORA_overlap_muscle_mass3.pdf",height=7,width=10)
  
  

ORA_VO2_low<-head(ORA_VO2_low,20)
ORA_VO2_high<-head(ORA_VO2_high,20)
ORA_list<-rbindlist(list(ORA_VO2_low,ORA_VO2_high))
ORA_list_final<-ORA_list%>%group_by(ID)%>%summarize(n=n())
ORA_list_final<-ORA_list_final[ORA_list_final$n>0,]
ORA_finalized<-ORA_list[ORA_list$ID%in%ORA_list_final$ID,]

common_ORA<-unique(ORA_finalized$ID)
ORA_levels<-unique(ORA_finalized$Description)
ORA_mean<-ORA_finalized%>%group_by(Description)%>%summarize(mn=mean(p.adjust))
ORA_mean<-ORA_mean[order(-ORA_mean$mn),]
ORA_finalized$Description<-factor(ORA_finalized$Description,levels=ORA_mean$Description)
ORA_finalized$Comparison<-factor(ORA_finalized$Comparison,levels=c("VO2_low", "VO2_high"))

ORA_finalized$numerator<-as.numeric(gsub("(\\d+)\\/.*","\\1",ORA_finalized$GeneRatio))
ORA_finalized$denominator<-as.numeric(gsub("\\d+\\/(.*)","\\1",ORA_finalized$GeneRatio))
ORA_finalized$GeneRatio<-ORA_finalized$numerator/ORA_finalized$denominator

#Top 20 in VO2 and High (not just overlap)--Figure S14
ORA_finalized%>%ggplot(aes(x=Comparison,y=Description))+
  #scale_x_continuous("Comparison",labels=as.character(Comparison),breaks=Comparison)+
  geom_point(aes(col=p.adjust,size=GeneRatio))+
  theme_minimal()+
  scale_color_gradient2(low = 'red', mid = 'red', high = 'lightyellow')+
  theme(axis.text.x = element_text(size=13),axis.text.y=element_text(size=10.5))+
  scale_y_discrete(labels=toupper)+
  scale_x_discrete(labels=c("highxmin"="PA", "VO2"="VO2", "kPCr"="kPCr", "Respirometry"="Mit-O2flux"))
ORA_finalized

ggsave("ORA_overlap_muscle_mass3.pdf",height=7,width=10)

}