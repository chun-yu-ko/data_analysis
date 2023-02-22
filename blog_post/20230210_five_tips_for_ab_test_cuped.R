# library -----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(colorspace)
library(ggthemes)
library(scales)

# Env. --------------------------------------------------------------------

showtext::showtext.auto(enable = TRUE)
showtext::showtext_opts(dpi = 300)

setwd("blog_post")

# Function ----------------------------------------------------------------

ggsave_bp = function(x){
  rstudioapi::getSourceEditorContext()[["path"]]%>%
    strsplit(., "/")%>%
    .[[1]]%>%
    .[length(.)]%>%
    substr(., 1, nchar(.)-2)%>%
    paste("output/", ., "_", x, ".png",sep="")%>%
    ggsave(., dpi = 300, width = 17.83, height = 9.11, unit = "cm")
}

create_ab_test_sample = function(name, ss, sd){
  data.frame(x = c(rnorm(ss, 0.04, sd), rnorm(ss, 0.02, sd)),
             z = c(rep("case",ss), rep("control",ss)),
             name = rep(name, ss))
}

# output ------------------------------------------------------------------


data.frame(x = c(rnorm(1000, 0.02, 0.02), rnorm(1000, 0.02, 0.01), rnorm(1000, 0.02, 0.005)),
           z = c(rep("情境一", 1000), rep("情境二", 1000), rep("情境三", 1000))%>%factor(., c("情境一", "情境二", "情境三")))%>%
  ggplot(aes(x = x, color = z, fill = z))+
  geom_vline(xintercept = 0, linetype = 2, size = .4, color = "orange")+
  geom_density(alpha = .5)+
  facet_wrap(~z, ncol = 3)+
  scale_x_continuous(labels = percent_format(1))+
  scale_color_discrete_qualitative("Warm")+
  scale_fill_discrete_qualitative("Warm")+
  labs(x = "兩組組間差異的分佈", y = "樣本數")+
  theme_fivethirtyeight(base_family = "wqy-microhei")+
  theme(legend.position = "none",
        axis.text.y = element_blank())

ggsave_bp("ab_text_3_scenario_of_difference_distrebution")









rbind(create_ab_test_sample("情境 1\n樣本少、變異大", 100, 0.01),
      create_ab_test_sample("情境 2\n樣本多、變異少", 500, 0.005))%>%
  ggplot(aes(x = x, color = z, fill = z))+
  geom_density(alpha = .5)+

  scale_color_discrete_diverging("Blue Red 2")+
  scale_fill_discrete_diverging("Blue Red 2")+
  facet_wrap(~name, ncol = 2)+
  labs(x = "Outcome 發生率的估計", y = "樣本數")+
  theme_fivethirtyeight(base_family = "wqy-microhei")+
  theme(legend.position = "none",
        axis.title = element_text())

ggsave_bp("ab_text_2_scenario_of_ss_sd")

