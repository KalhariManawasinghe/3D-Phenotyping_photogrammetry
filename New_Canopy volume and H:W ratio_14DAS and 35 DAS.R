library(readr)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(patchwork)


data <- read_csv(
  "Desktop/UOW/FinalUOW data sheet with raw data.csv"
)


data$Genotype <- factor(
  data$Genotype,
  levels=c(
    "AAC Brandon",
    "CDC Teal",
    "Alsen",
    "Gladius",
    "Chara",
    "Kukri"
  )
)


data$CA  <- as.numeric(data$CA)
data$V   <- as.numeric(data$V)
data$HW  <- as.numeric(data$HW)
data$DAS <- as.numeric(data$DAS)



D14 <- subset(data, DAS==14)

D35 <- subset(data, DAS==35)



D14 <- na.omit(
  D14[,c("CA","V","HW","Genotype")]
)

D35 <- na.omit(
  D35[,c("CA","V","HW","Genotype")]
)

check_normality <- function(df, variable){
  
  test <- shapiro.test(df[[variable]])
  
  data.frame(
    Trait=variable,
    W=round(test$statistic,3),
    P_value=round(test$p.value,4)
  )
  
}


normality_raw <- rbind(
  
  check_normality(D14,"V"),
  check_normality(D14,"HW"),
  
  check_normality(D35,"V"),
  check_normality(D35,"HW")
  
)


print(normality_raw)



D14 <- D14 %>%
  mutate(
    log_V = log(V),
    log_HW = log(HW + 0.01)
  )


D35 <- D35 %>%
  mutate(
    log_V = log(V),
    log_HW = log(HW + 0.01)
  )


normality_log <- rbind(
  
  check_normality(D14,"log_V"),
  check_normality(D14,"log_HW"),
  
  check_normality(D35,"log_V"),
  check_normality(D35,"log_HW")
  
)


print(normality_log)


cols <- c(
  
  "AAC Brandon"="#1B9E77",
  "CDC Teal"="#7570B3",
  "Alsen"="#D95F02",
  "Gladius"="#E7298A",
  "Chara"="#66A61E",
  "Kukri"="#A6761D"
  
)


shapes <- c(
  
  "AAC Brandon"=16,
  "CDC Teal"=1,
  "Alsen"=17,
  "Gladius"=15,
  "Chara"=18,
  "Kukri"=8
  
)


V_limits <- range(
  c(D14$log_V,D35$log_V),
  na.rm=TRUE
)

V_limits <- V_limits + c(-0.2,0.2)



HW_limits <- range(
  c(D14$log_HW,D35$log_HW),
  na.rm=TRUE
)

HW_limits <- HW_limits + c(-0.2,0.2)


make_plot <- function(df,response,ylab,panel_label,ylim_range){
  
  
  ggplot(
    df,
    aes(
      x=CA,
      y=.data[[response]]
    )
  )+
    
    
    geom_point(
      aes(
        colour=Genotype,
        shape=Genotype
      ),
      size=3.5,
      stroke=1
    )+
    
    
    geom_smooth(
      aes(group=1),
      method="lm",
      formula=y~x,
      colour="black",
      fill="grey70",
      linewidth=1,
      se=TRUE
    )+
    
    
    stat_poly_eq(
      aes(
        label=paste(
          after_stat(rr.label),
          after_stat(p.value.label),
          sep="~~~~"
        ),
        group=1
      ),
      formula=y~x,
      parse=TRUE,
      label.x=0.85,
      label.y= max(df[[response]]),
      size=5
    )+
    
    
    scale_colour_manual(
      values=cols
    )+
    
    
    scale_shape_manual(
      values=shapes
    )+
    
    
    # IMPORTANT: does not remove data
    coord_cartesian(
      ylim=ylim_range
    )+
    
    
    labs(
      x="Canopy architecture score",
      y=ylab,
      colour="Genotype",
      shape="Genotype"
    )+
    
    scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
    
    annotate(
      "text",
      x=-Inf,
      y=Inf,
      label=panel_label,
      hjust=-0.3,
      vjust=1.5,
      size=7,
      fontface="bold"
    )+
    
    
    theme_classic(
      base_size=15
    )+
    
    
    theme(
      
      panel.border=element_rect(
        colour="black",
        fill=NA,
        linewidth=0.8
      ),
      
      legend.position="right",
      
      axis.text=element_text(
        colour="black"
      )
      
    )
  
}



A <- make_plot(
  D14,
  "log_V",
  "ln(canopy volume, mm³)",
  "A",
  V_limits
)



B <- make_plot(
  D35,
  "log_V",
  "ln(canopy volume, mm³)",
  "B",
  V_limits
)



C <- make_plot(
  D14,
  "log_HW",
  "ln(height:width ratio)",
  "C",
  HW_limits
)



D <- make_plot(
  D35,
  "log_HW",
  "ln(height:width ratio)",
  "D",
  HW_limits
)


final_figure <-
  (A+B+C+D)+
  plot_layout(
    ncol=2,
    guides="collect"
  )


final_figure


ggsave(
  "~/Desktop/Figure_CanopyArchitecture_Volume_HW.tiff",
  final_figure,
  width=30,
  height=24,
  units="cm",
  dpi=600,
  compression="lzw"
)

