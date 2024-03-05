# Meu tema

require(extrafont)

COR.1="black"
COR.2="black"

meutema <- function(){
  theme_minimal()+
    theme(
      axis.title = element_text(size=10, family = "AvantGarde",colour=COR.2), 
      axis.text = element_text(size=10, family = "AvantGarde",colour=COR.1),
      plot.title = element_text(size=20, family = "AvantGarde",colour=COR.1,face="bold",hjust=0.5),
      legend.text = element_text(size=10, family = "AvantGarde",colour=COR.1,face="bold"),
      legend.title = element_text(size=20, family = "AvantGarde",colour=COR.2,face="bold"),
      plot.background = element_rect(fill=NA,colour=NA),
      panel.background = element_rect(fill=NA,colour=NA),
      legend.background = element_rect(fill=NA,colour=NA),
      legend.box.background = element_rect(fill=NA,colour=NA),
      strip.background = element_rect(fill=COR.1),
      strip.text=element_text(family="AvantGarde",size=15))
}


