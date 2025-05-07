### PREPARATION ####

## Install and load required packages 

packages <- c("dplyr", "tidyr", "data.table", "psych","ggplot2", "ggcorrplot", "stringr", "ggrepel", "ragg", "systemfonts", "patchwork", "readr", "gridExtra")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

## load function
source("code/analyses/helper/filter_corrs.R") # function to filter correlations
source("code/analyses/helper/figures.R") # function to create figures

## load data 

# trait
keyboard_data_trait <- readRDS(file="data/results/cleaned/keyboard_data_trait_cleaned.rds") # all text
keyboard_data_trait_private <- readRDS(file="data/results/cleaned/keyboard_data_trait_private_cleaned.rds") # all private text
keyboard_data_trait_public <- readRDS(file="data/results/cleaned/keyboard_data_trait_public_cleaned.rds") # all public text

# week
keyboard_data_week_ema <- readRDS(file="data/results/cleaned/keyboard_data_week_ema_cleaned.rds")

# day
keyboard_data_day_ema <- readRDS(file="data/results/cleaned/keyboard_data_day_ema_cleaned.rds") 
keyboard_data_day_ema <- keyboard_data_day_ema %>% filter(es_count_day >= 3)

# moment
keyboard_data_moment_ema <- readRDS(file="data/results/cleaned/keyboard_data_moment_ema_cleaned.rds") 

## split data sets by gender for further analyses

# trait
keyboard_data_trait_men <- keyboard_data_trait %>% filter(gender == 1)
keyboard_data_trait_women <- keyboard_data_trait %>% filter(gender == 2)

# week
keyboard_data_week_ema_men <- keyboard_data_week_ema %>% filter(gender == 1)
keyboard_data_week_ema_women <- keyboard_data_week_ema %>% filter(gender == 2)

# day
keyboard_data_day_ema_men <- keyboard_data_day_ema %>% filter(gender == 1)
keyboard_data_day_ema_women <- keyboard_data_day_ema %>% filter(gender == 2)

# moment
keyboard_data_moment_ema_men <- keyboard_data_moment_ema %>% filter(gender == 1)
keyboard_data_moment_ema_women <- keyboard_data_moment_ema %>% filter(gender == 2)

#### COMPUTE DESCRIPTIVE STATS OF LANGUAGE FEATURES ####

# TO DO

#### COMPUTE CORRELATIONS OF LANGUAGE FEATURES AND AFFECT ####

# get vector with feature names from each feature group
features <- colnames(keyboard_data_trait)[match("duration_avg", colnames(keyboard_data_trait)):ncol(keyboard_data_trait)]

# typingdynamics <- colnames(keyboard_data_trait)[match("duration_avg", colnames(keyboard_data_trait)):match("words_share_action_datainput", colnames(keyboard_data_trait))]
# dictionaries <- colnames(keyboard_data_trait)[match("wordsentiment_match_rate", colnames(keyboard_data_trait)):match("LIWC_unknown_max", colnames(keyboard_data_trait))]
# emoticons <- colnames(keyboard_data_trait)[match("emoticon_count_sum", colnames(keyboard_data_trait)):match("heart1_max", colnames(keyboard_data_trait))]
# emoji <- colnames(keyboard_data_trait)[match("emoji_count_sum", colnames(keyboard_data_trait)):match("emoji_128652_max", colnames(keyboard_data_trait))]

## compute corrs with all features

## trait

targets_trait <- c("pa_panas", "na_panas")

# all text
cor_trait <- cor_table(keyboard_data_trait, targets_trait, features)
cor_trait_men <- cor_table(keyboard_data_trait_men, targets_trait, features)

# some cols are NA, will fix this later
exclude_cols <- which(colnames(keyboard_data_trait_women) %in% c("duration_word_avg", "duration_word_var", "duration_word_max"))
cor_trait_women <- cor_table(keyboard_data_trait_women[,-exclude_cols], targets_trait, setdiff(features, colnames(keyboard_data_trait_women)[exclude_cols]))

# private
cor_trait_private <- cor_table(keyboard_data_trait_private, targets_trait, features)
# cor_trait_private_men <- cor_table(keyboard_data_trait_private_men, targets_trait, features)
# cor_trait_private_women <- cor_table(keyboard_data_trait_private_women, targets_trait, features)

# public
cor_trait_public <- cor_table(keyboard_data_trait_public, targets_trait, features)
# cor_trait_public_men <- cor_table(keyboard_data_trait_public_men, targets_trait, features)
# cor_trait_public_women <- cor_table(keyboard_data_trait_public_women, targets_trait, features)

## weekly

targets_week <- c("valence_week", "arousal_week")

cor_week <- cor_table(keyboard_data_week_ema, targets_week, features)
cor_week_men <- cor_table(keyboard_data_week_ema_men, targets_week, features)
cor_week_women <- cor_table(keyboard_data_week_ema_women, targets_week, features)

## daily

targets_day <- c("valence_day", "arousal_day")

cor_day <- cor_table(keyboard_data_day_ema, targets_day, features)
cor_day_men <- cor_table(keyboard_data_day_ema_men, targets_day, features)
cor_day_women <- cor_table(keyboard_data_day_ema_women, targets_day, features)

## momentary

targets_moment <- c("valence", "arousal")

cor_moment <- cor_table(keyboard_data_moment_ema, targets_moment, features)
cor_moment_men <- cor_table(keyboard_data_moment_ema_men, targets_moment, features)
cor_moment_women <- cor_table(keyboard_data_moment_ema_women, targets_moment, features)

# save corrs

write.csv2(cor_moment, "data/results/insights/cor_moment.csv")


#### FIGURE 1: EMOTION DICTIONARIES ACROSS CONTEXTS ####


# create df with full names of direct emotion dictionaries

emotion_dictionaries_names <- data.frame(short = 
  c(
    "wordsentiment_avg",
    "wordsentiment_var",
    "wordsentiment_min",
    "wordsentiment_max",
    "LIWC_Posemo_avg",
    "LIWC_Negemo_avg",
    "LIWC_Anger_avg",
    "LIWC_Anx_avg",
    "LIWC_Sad_avg"
),
full =  c(
    "Word sentiment (M)",
    "Word sentiment (SD)",
    "Word sentiment (Min)",
    "Word sentiment (Max)",
    "Positive emotion",
    "Negative emotion",
    "Anger",
    "Anxiety",
    "Sadness"
  )
)

## filter out direct emotion dictionaries

# trait
cor_trait_emotion_dictionaries <- cor_trait[cor_trait$feature %in% emotion_dictionaries_names$short, ]
cor_trait_men_emotion_dictionaries <- cor_trait_men[cor_trait_men$feature %in% emotion_dictionaries_names$short, ]
cor_trait_women_emotion_dictionaries <- cor_trait_women[cor_trait_women$feature %in% emotion_dictionaries_names$short, ]

cor_trait_private_emotion_dictionaries <- cor_trait_private[cor_trait_private$feature %in% emotion_dictionaries_names$short, ]
cor_trait_private_men_emotion_dictionaries <- cor_trait_private_men[cor_trait_private_men$feature %in% emotion_dictionaries_names$short, ]
cor_trait_private_women_emotion_dictionaries <- cor_trait_private_women[cor_trait_private_women$feature %in% emotion_dictionaries_names$short, ]

cor_trait_public_emotion_dictionaries <- cor_trait_public[cor_trait_public$feature %in% emotion_dictionaries_names$short, ]
cor_trait_public_men_emotion_dictionaries <- cor_trait_public_men[cor_trait_public_men$feature %in% emotion_dictionaries_names$short, ]
cor_trait_public_women_emotion_dictionaries <- cor_trait_public_women[cor_trait_public_women$feature %in% emotion_dictionaries_names$short, ]

# week
cor_week_emotion_dictionaries <- cor_week[cor_week$feature %in% emotion_dictionaries_names$short & 
                                            cor_week$target == "valence_week", ]

cor_week_men_emotion_dictionaries <- cor_week_men[cor_week_men$feature %in% emotion_dictionaries_names$short & 
                                            cor_week_men$target == "valence_week", ]

cor_week_women_emotion_dictionaries <- cor_week_women[cor_week_women$feature %in% emotion_dictionaries_names$short & 
                                                    cor_week_women$target == "valence_week", ]

# day
cor_day_emotion_dictionaries <- cor_day[cor_day$feature %in% emotion_dictionaries_names$short & 
                                           cor_day$target == "valence_day", ]

cor_day_men_emotion_dictionaries <- cor_day_men[cor_day_men$feature %in% emotion_dictionaries_names$short & 
                                          cor_day_men$target == "valence_day", ]

cor_day_women_emotion_dictionaries <- cor_day_women[cor_day_men$feature %in% emotion_dictionaries_names$short & 
                                                  cor_day_women$target == "valence_day", ]

# moment
cor_moment_emotion_dictionaries <- cor_moment[cor_moment$feature %in% emotion_dictionaries_names$short & 
                                              cor_moment$target == "valence", ]

cor_moment_men_emotion_dictionaries <- cor_moment_men[cor_moment$feature %in% emotion_dictionaries_names$short & 
                                                cor_moment_men$target == "valence", ]

cor_moment_women_emotion_dictionaries <- cor_moment_women[cor_moment$feature %in% emotion_dictionaries_names$short & 
                                                        cor_moment_women$target == "valence", ]

## transform to correlation matrix for plotting

emotion_order <- c("wordsentiment_avg", "wordsentiment_min", "LIWC_Posemo_avg", "LIWC_Negemo_avg", "LIWC_Anger_avg", "LIWC_Anx_avg", "LIWC_Sad_avg")

cormat_trait_emotion_dictionaries <- transform_to_cormatrix(cor_trait_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_trait_men_emotion_dictionaries <- transform_to_cormatrix(cor_trait_men_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_trait_women_emotion_dictionaries <- transform_to_cormatrix(cor_trait_women_emotion_dictionaries, "r")[, emotion_order, drop = F]

cormat_trait_private_emotion_dictionaries <- transform_to_cormatrix(cor_trait_private_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_trait_public_emotion_dictionaries <- transform_to_cormatrix(cor_trait_public_emotion_dictionaries, "r")[, emotion_order, drop = F]

cormat_week_emotion_dictionaries <- transform_to_cormatrix(cor_week_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_week_men_emotion_dictionaries <- transform_to_cormatrix(cor_week_men_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_week_women_emotion_dictionaries <- transform_to_cormatrix(cor_week_women_emotion_dictionaries, "r")[, emotion_order, drop = F]

cormat_day_emotion_dictionaries <- transform_to_cormatrix(cor_day_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_day_men_emotion_dictionaries <- transform_to_cormatrix(cor_day_men_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_day_women_emotion_dictionaries <- transform_to_cormatrix(cor_day_women_emotion_dictionaries, "r")[, emotion_order, drop = F]

cormat_moment_emotion_dictionaries <- transform_to_cormatrix(cor_moment_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_moment_men_emotion_dictionaries <- transform_to_cormatrix(cor_moment_men_emotion_dictionaries, "r")[, emotion_order, drop = F]
cormat_moment_women_emotion_dictionaries <- transform_to_cormatrix(cor_moment_women_emotion_dictionaries, "r")[, emotion_order, drop = F]

## create plots 

trait_emotion_dictionaries_corplot = ggcorrplot(
  cormat_trait_emotion_dictionaries[, ncol(cormat_trait_emotion_dictionaries):1],
  method = "square",
  title = "All",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

trait_men_emotion_dictionaries_corplot = ggcorrplot(
  cormat_trait_men_emotion_dictionaries[, ncol(cormat_trait_men_emotion_dictionaries):1],
  method = "square",
  title = "Men",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 


trait_women_emotion_dictionaries_corplot = ggcorrplot(
  cormat_trait_women_emotion_dictionaries[, ncol(cormat_trait_women_emotion_dictionaries):1],
  method = "square",
  title = "Women",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 


trait_private_emotion_dictionaries_corplot = ggcorrplot(
  cormat_trait_private_emotion_dictionaries[, ncol(cormat_trait_private_emotion_dictionaries):1],
  method = "square",
  title = "Private",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

trait_public_emotion_dictionaries_corplot = ggcorrplot(
  cormat_trait_public_emotion_dictionaries[, ncol(cormat_trait_public_emotion_dictionaries):1],
  method = "square",
  title = "Public",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 


week_emotion_dictionaries_corplot = ggcorrplot(
  cormat_week_emotion_dictionaries[, ncol(cormat_week_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "All",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

week_men_emotion_dictionaries_corplot = ggcorrplot(
  cormat_week_men_emotion_dictionaries[, ncol(cormat_week_men_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "Men",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

week_women_emotion_dictionaries_corplot = ggcorrplot(
  cormat_week_women_emotion_dictionaries[, ncol(cormat_week_women_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "Women",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

day_emotion_dictionaries_corplot = ggcorrplot(
  cormat_day_emotion_dictionaries[, ncol(cormat_day_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "All",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
)

day_men_emotion_dictionaries_corplot = ggcorrplot(
  cormat_day_men_emotion_dictionaries[, ncol(cormat_day_men_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "Men",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
)

day_women_emotion_dictionaries_corplot = ggcorrplot(
  cormat_day_women_emotion_dictionaries[, ncol(cormat_day_women_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "Women",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
)

moment_emotion_dictionaries_corplot = ggcorrplot(
  cormat_moment_emotion_dictionaries[, ncol(cormat_moment_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "All",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

moment_men_emotion_dictionaries_corplot = ggcorrplot(
  cormat_moment_men_emotion_dictionaries[, ncol(cormat_moment_men_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "Men",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

moment_women_emotion_dictionaries_corplot = ggcorrplot(
  cormat_moment_women_emotion_dictionaries[, ncol(cormat_moment_women_emotion_dictionaries):1, drop = F],
  method = "square",
  title = "Women",
  legend.title = "Pearson\ncorrelation",
  lab = TRUE,
  lab_size = 3,
  ggtheme = theme_minimal,
  outline.color = "lightgray",
  colors = c("#6D9EC1", "white", "#E46726")
) +  theme(
  axis.text.x = ggplot2::element_text(size = 11),
  axis.text.y = ggplot2::element_text(size = 11),
  plot.title = element_text(size = 10)
) 

# combine plots into one figure by group

group_trait = (trait_emotion_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=c('Pos. Affect', 'Neg. Affect')) + scale_y_discrete(labels=c("Sadness", "Anxiety", "Anger", "Negative emotion", "Positive emotion", "Word sentiment (Min)", "Word sentiment (M)")) | 
  #trait_men_emotion_dictionaries_corplot  + theme(legend.position = "none") + scale_x_discrete(labels=c('Pos. Affect', 'Neg. Affect')) + scale_y_discrete (labels = element_blank()) | 
  #trait_women_emotion_dictionaries_corplot  + theme(legend.position = "none") + scale_x_discrete(labels=c('Pos. Affect', 'Neg. Affect')) + scale_y_discrete (labels = element_blank()) | 
  trait_private_emotion_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=c('Pos. Affect', 'Neg. Affect')) + scale_y_discrete (labels = element_blank()) | 
  trait_public_emotion_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=c('Pos. Affect', 'Neg. Affect')) + scale_y_discrete (labels = element_blank()))  +
  plot_annotation(title = "Trait", theme = theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))) 

group_week =   (week_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank())  +
                   week_men_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence'))+ scale_y_discrete (labels = element_blank())  +
                   week_women_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank())) &
  plot_annotation(title = "Week", theme = theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20)))  

group_day =   (day_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank())  +
                   day_men_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence'))+ scale_y_discrete (labels = element_blank())  +
                   day_women_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank())) &
  plot_annotation(title = "Day", theme = theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))) 

group_moment =   (moment_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank())  +
                    moment_men_emotion_dictionaries_corplot +  theme(legend.position = "none") + scale_x_discrete(labels=c('Valence'))+ scale_y_discrete (labels = element_blank())  +
                    moment_women_emotion_dictionaries_corplot  + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank())) &
  plot_annotation(title = "Moment", theme = theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))) 



group_state = (week_emotion_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels=element_blank()) + 
                 day_emotion_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank()) +
                 moment_emotion_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=c('Valence')) + scale_y_discrete (labels = element_blank()))  +
  plot_annotation(title = "States", theme = theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))) 



# save figures
png(file="figures/emotion_dictionaries_corplot_trait.png",width=500, height=500)
group_trait
dev.off()

png(file="figures/emotion_dictionaries_corplot_state.png",width=500, height=500)
group_state
dev.off()




# # Create a representative plot for extracting the legend
# legend_source_plot <- day_emotion_dictionaries_corplot + 
#   theme(legend.position = "right") # Ensure the plot has a legend
# 
# # Extract the legend
# legend <- cowplot::get_legend(legend_source_plot)
# 
# # Combine grouped figures without legends with single legend
# emotion_dictionaries_corplot_overview <- wrap_elements(group_trait) | wrap_elements(group_week) | wrap_elements(group_day) | wrap_elements(group_moment) | legend
# 
# emotion_dictionaries_corplot_overview  <- emotion_dictionaries_corplot_overview  + 
#   plot_layout(widths = c(3.6, 1.1, 1.1, 1.1, 0.5),
#               heights = c(0.5, 0.5))  # Ensuring equal heights for all rows
# 

# # save figure
# png(file="figures/emotion_dictionaries_corplot_overview.png",width=1000, height=500)
# emotion_dictionaries_corplot_overview 
# dev.off()



#### FIGURE 2: INDIRECT EMOTION DICTIONARIES ACROSS CONTEXTS RANKED ####

# load data
liwc.names = read_delim("data/helper/DE-LIWC2015.rimealiases", delim="\t", col_names=c("LIWC.cat","C.cat"))

# create df with full names of non-emotion dictionaries
liwc_names =  paste0("LIWC_", liwc.names$LIWC.cat, "_avg")
dictionaries_names <- liwc_names[!liwc_names %in% emotion_dictionaries_names$short]

## filter out direct non-emotion dictionaries

# trait
cor_trait_dictionaries <- cor_trait[cor_trait$feature %in% dictionaries_names , ]

cor_trait_men_dictionaries <- cor_trait_men[cor_trait_men$feature %in% dictionaries_names , ]
cor_trait_women_dictionaries <- cor_trait_women[cor_trait_women$feature %in% dictionaries_names , ]

cor_trait_private_dictionaries <- cor_trait_private[cor_trait_private$feature %in% dictionaries_names , ]
cor_trait_public_dictionaries <- cor_trait_public[cor_trait_public$feature %in% dictionaries_names , ]

# week
cor_week_dictionaries <- cor_week[cor_week$feature %in% dictionaries_names & 
                                    cor_week$target == "valence_week", ]

cor_week_men_dictionaries <- cor_week_men[cor_week_men$feature %in% dictionaries_names & 
                                    cor_week_men$target == "valence_week", ]

cor_week_women_dictionaries <- cor_week_women[cor_week_women$feature %in% dictionaries_names & 
                                            cor_week_women$target == "valence_week", ]

# day
cor_day_dictionaries <- cor_day[cor_day$feature %in% dictionaries_names & 
                                    cor_day$target == "valence_day", ]

cor_day_men_dictionaries <- cor_day_men[cor_day_men$feature %in% dictionaries_names & 
                                  cor_day_men$target == "valence_day", ]

cor_day_women_dictionaries <- cor_day_women[cor_day_women$feature %in% dictionaries_names & 
                                          cor_day_women$target == "valence_day", ]

# moment
cor_moment_dictionaries <- cor_moment[cor_moment$feature %in% dictionaries_names & 
                                    cor_moment$target == "valence", ]

cor_moment_men_dictionaries <- cor_moment_men[cor_moment_men$feature %in% dictionaries_names & 
                                        cor_moment_men$target == "valence", ]

cor_moment_women_dictionaries <- cor_moment_women[cor_moment_women$feature %in% dictionaries_names & 
                                                cor_moment_women$target == "valence", ]

# show top features for each target

# trait
pa_panas_table <- cor_trait_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_table <- cor_trait_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_men_table <- cor_trait_men_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_men_table <- cor_trait_men_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_women_table <- cor_trait_women_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_women_table <- cor_trait_women_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_private_table <- cor_trait_private_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "private") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_private_table <- cor_trait_private_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "private") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_public_table <- cor_trait_public_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "public") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_public_table <- cor_trait_public_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "public") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# week
valence_week_table <- cor_week_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

valence_week_men_table <- cor_week_men_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

valence_week_women_table <- cor_week_women_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# day 
valence_day_table <- cor_day_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

valence_day_men_table <- cor_day_men_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

valence_day_women_table <- cor_day_women_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# moment
valence_moment_table <- cor_moment_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

valence_moment_men_table <- cor_moment_men_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

valence_moment_women_table <- cor_moment_women_dictionaries %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# create a combined table
dictionaries_cor_table <- rbind(pa_panas_table , na_panas_table,
                              pa_panas_men_table , na_panas_men_table,
                              pa_panas_women_table , na_panas_women_table,
                              pa_panas_private_table , na_panas_private_table,
                              pa_panas_public_table , na_panas_public_table,
                              valence_week_table , valence_week_men_table, valence_week_women_table,
                              valence_day_table , valence_day_men_table, valence_day_women_table,
                              valence_moment_table , valence_moment_men_table, valence_moment_women_table) 

# save table
write.csv2(dictionaries_cor_table, "data/results/insights/dictionaries_cor_table.csv")



#### FIGURE 3: EMOJI + EMOTICON VALENCE RANKINGS ACROSS CONTEXTS ####

# read in df
emoji_df <- readRDS("data/helper/emoji_df.RData")

# create df with full names of non-emotion dictionaries
emoji_names =  paste0(emoji_df$variable_name, "_avg")


## filter out emoji correlations

# trait
cor_trait_emoji <- cor_trait[cor_trait$feature %in% emoji_names , ]
cor_trait_men_emoji <- cor_trait_men[cor_trait_men$feature %in% emoji_names , ]
cor_trait_women_emoji <- cor_trait_women[cor_trait_women$feature %in% emoji_names , ]

cor_trait_private_emoji <- cor_trait_private[cor_trait_private$feature %in% emoji_names , ]
cor_trait_public_emoji <- cor_trait_public[cor_trait_public$feature %in% emoji_names , ]

# week
cor_week_emoji <- cor_week[cor_week$feature %in% emoji_names & 
                             cor_week$target == "valence_week", ]

cor_week_men_emoji <- cor_week_men[cor_week_men$feature %in% emoji_names & 
                             cor_week_men$target == "valence_week", ]

cor_week_women_emoji <- cor_week_women[cor_week_women$feature %in% emoji_names & 
                                     cor_week_women$target == "valence_week", ]

# day
cor_day_emoji <- cor_day[cor_day$feature %in% emoji_names & 
                           cor_day$target == "valence_day", ]

cor_day_men_emoji <- cor_day_men[cor_day_men$feature %in% emoji_names & 
                           cor_day_men$target == "valence_day", ]

cor_day_women_emoji <- cor_day_women[cor_day_women$feature %in% emoji_names & 
                                   cor_day_women$target == "valence_day", ]

# moment
cor_moment_emoji <- cor_moment[cor_moment$feature %in% emoji_names & 
                               cor_moment$target == "valence", ]

cor_moment_men_emoji <- cor_moment_men[cor_moment_men$feature %in% emoji_names & 
                                 cor_moment_men$target == "valence", ]

cor_moment_women_emoji <- cor_moment_women[cor_moment_women$feature %in% emoji_names & 
                                 cor_moment_women$target == "valence", ]

# create ranking figures 

# trait
pa_panas_emoji_table <- cor_trait_emoji%>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_emoji_table <- cor_trait_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_men_emoji_table <- cor_trait_men_emoji%>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_men_emoji_table <- cor_trait_men_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_women_emoji_table <- cor_trait_women_emoji%>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_women_emoji_table <- cor_trait_women_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_private_emoji_table <- cor_trait_private_emoji%>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "private") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_private_emoji_table <- cor_trait_private_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "private") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

pa_panas_public_emoji_table <- cor_trait_public_emoji%>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "pa_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "public") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

na_panas_public_emoji_table <- cor_trait_public_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  filter(target == "na_panas") %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "public") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# week

week_emoji_table <- cor_week_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

week_men_emoji_table <- cor_week_men_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

week_women_emoji_table <- cor_week_women_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# day

day_emoji_table <- cor_day_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

day_men_emoji_table <- cor_day_men_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

day_women_emoji_table <- cor_day_women_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# moment 

moment_emoji_table <- cor_moment_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "all") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

moment_men_emoji_table <- cor_moment_men_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "men") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

moment_women_emoji_table <- cor_moment_women_emoji %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(abs_r = abs(r)) %>%
  arrange(desc(abs_r)) %>%
  mutate(feature = gsub("_avg$", "", feature)) %>%
  left_join(emoji_df, by = c("feature" = "variable_name")) %>%
  mutate(feature = emoji, group = "women") %>%
  select(target, group, feature, r, ci_lower, ci_upper, p_value)

# create a combined table
emoji_cor_table <- rbind(pa_panas_emoji_table , na_panas_emoji_table,
                              pa_panas_men_emoji_table , na_panas_men_emoji_table,
                              pa_panas_women_emoji_table , na_panas_women_emoji_table,
                              pa_panas_private_emoji_table , na_panas_private_emoji_table,
                              pa_panas_public_emoji_table , na_panas_public_emoji_table,
                              week_emoji_table , week_men_emoji_table, week_women_emoji_table,
                              day_emoji_table , day_men_emoji_table, day_women_emoji_table,
                              moment_emoji_table , moment_men_emoji_table, moment_women_emoji_table) 

write.csv2(emoji_cor_table, "data/results/insights/emoji_cor_table.csv")

# this causes some emoji printing issues



### DISTRIBUTION OF AFFECT OUTCOMES ACROSS DATA SETS (APPENDIX) ####

hist_pa_affect_trait <- ggplot(keyboard_data_trait, aes(x=pa_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,75))+ 
  scale_x_continuous(name = "Positive Trait Affect (all text)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_na_affect_trait <- ggplot(keyboard_data_trait, aes(x=na_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,75))+ 
  scale_x_continuous(name = "Negative Trait Affect (all text)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_pa_affect_trait_private <- ggplot(keyboard_data_trait_private, aes(x=pa_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,40))+ 
  scale_x_continuous(name = "Positive Trait Affect (private)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_na_affect_trait_private <- ggplot(keyboard_data_trait_private, aes(x=na_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,40))+ 
  scale_x_continuous(name = "Negative Trait Affect (private)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_pa_affect_trait_public <- ggplot(keyboard_data_trait_public, aes(x=pa_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,25))+ 
  scale_x_continuous(name = "Positive Trait Affect (public)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_na_affect_trait_public <- ggplot(keyboard_data_trait_public, aes(x=na_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,25))+ 
  scale_x_continuous(name = "Negative Trait Affect (public)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)


hist_valence_affect_week <- ggplot(keyboard_data_trait_es_week, aes(x=valence_week)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,150))+
  scale_x_continuous(name = "Valence (weekly)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_week <- ggplot(keyboard_data_trait_es_week, aes(x=arousal_week)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,150))+
  scale_x_continuous(name = "Arousal (weekly)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_valence_affect_day <- ggplot(keyboard_data_trait_es_day, aes(x=valence_day)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,1000))+
  scale_x_continuous(name = "Valence (daily)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_day <- ggplot(keyboard_data_trait_es_day, aes(x=arousal_day)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,1000))+
  scale_x_continuous(name = "Arousal (daily)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_valence_affect_moment <- ggplot(keyboard_data_trait_es_threehrs, aes(x=valence)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,350))+
  scale_x_continuous(name = "Valence (momentary)", breaks = c(1:6)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_moment <- ggplot(keyboard_data_trait_es_threehrs, aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,350))+
  scale_x_continuous(name = "Arousal (momentary)", breaks = c(1:6)) +
  theme_minimal(base_size = 20)

hist_valence_affect_moment_diff <- ggplot(keyboard_data_trait_es_threehrs, aes(x=diff_valence)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,450))+
  scale_x_continuous(name = "Valence Fluct. (momentary)", seq(-4, 4, 1)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_moment_diff <- ggplot(keyboard_data_trait_es_threehrs, aes(x=diff_arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,450))+
  scale_x_continuous(name = "Arousal Fluct. (momentary)", seq(-4, 4, 1)) +
  theme_minimal(base_size = 20)

# arrange histograms (2x5 matrix)

affect_dist_overview <- 
  (hist_pa_affect_trait + hist_na_affect_trait ) /
  (hist_pa_affect_trait_private + hist_na_affect_trait_private) /
  (hist_pa_affect_trait_public + hist_na_affect_trait_public ) /
  (hist_valence_affect_week + hist_arousal_affect_week) / 
  (hist_valence_affect_day + hist_arousal_affect_day) /
  (hist_valence_affect_moment + hist_arousal_affect_moment) /
  (hist_valence_affect_moment_diff + hist_arousal_affect_moment_diff)

# save figure
png(file="figures/affect_dist_overview.png",width=1000, height=1500)
affect_dist_overview 
dev.off()

# FINISH