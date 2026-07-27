library(readr)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ragg)


data <- read_csv("Desktop/UOW/Leaf angle.csv")


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


data$CA <- as.numeric(data$CA)
data$LA <- as.numeric(data$LA)

data <- na.omit(
  data[,c("CA","LA","Genotype")]
)



check_normality <- function(df, variable){
  
  test <- shapiro.test(df[[variable]])
  
  data.frame(
    Trait = variable,
    W = round(test$statistic,3),
    P_value = round(test$p.value,4)
  )
}



normality_raw <- check_normality(
  data,
  "LA"
)

print(normality_raw)



data <- data %>%
  mutate(
    log_LA = log(LA)
  )



normality_log <- check_normality(
  data,
  "log_LA"
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

LA_CA_plot <- ggplot(
  data,
  aes(
    x=CA,
    y=log_LA
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
    fill="grey80",
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
    label.x= "middle",
    label.y="top",
    size=5
  )+
  
  
  scale_colour_manual(
    values=cols
  )+
  
  
  scale_shape_manual(
    values=shapes
  )+
  
  
  scale_x_continuous(
    limits=c(1,10),
    breaks=1:10
  )+
  
  
  labs(
    x="Canopy architecture score",
    y="ln(leaf angle, °)",
    colour="Genotype",
    shape="Genotype"
  )+
  
  
  theme_classic(
    base_size=14
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

LA_CA_plot

ggsave(
  "~/Desktop/Figure_LeafAngle_CanopyArchitecture.tiff",
  LA_CA_plot,
  device=ragg::agg_tiff,
  width=18.2,
  height=12,
  units="cm",
  dpi=600,
  compression="lzw"
)

model <- lm(log_LA ~ CA, data = data)
shapiro.test(residuals(model))

