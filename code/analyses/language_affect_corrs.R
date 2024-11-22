### PREPARATION ####

## Install and load required packages 

packages <- c("dplyr", "tidyr", "data.table", "psych","ggplot2", "ggcorrplot", "stringr", "ggrepel", "ragg", "systemfonts", "patchwork")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

## load function
source("r_code/functions/filter_corrs.R") # function to filter correlations
source("r_code/functions/figures.R") # function to create figures

## load data 

# trait
affect_language <- readRDS(file="data/affect_language_features/affect_language_500.RData") # all text
affect_language_private <- readRDS(file="data/affect_language_features/affect_language_private_500.RData") # all private text
affect_language_public <- readRDS(file="data/affect_language_features/affect_language_public_500.RData") # all public text

# week
affect_language_es_week <- readRDS(file="data/affect_language_features/affect_language_es_week_500.RData") %>% ungroup() %>% as.data.frame()

# day
affect_language_es_day <- readRDS(file="data/affect_language_features/affect_language_es_day_100.RData") %>% ungroup()  %>% as.data.frame()

# moment
affect_language_es_threehrs <- readRDS(file="data/affect_language_features/affect_language_es_threehrs_100.RData") 

## split data sets by gender

# trait
affect_language_men <- affect_language %>% filter(Demo_GE1 == 1)
affect_language_women <- affect_language %>% filter(Demo_GE1 == 2)

affect_language_private_men <- affect_language_private %>% filter(Demo_GE1 == 1)
affect_language_private_women <- affect_language_private %>% filter(Demo_GE1 == 2)

affect_language_public_men <- affect_language_public %>% filter(Demo_GE1 == 1)
affect_language_public_women <- affect_language_public %>% filter(Demo_GE1 == 2)

# week
affect_language_es_week_men <- affect_language_es_week %>% filter(Demo_GE1 == 1)
affect_language_es_week_women <- affect_language_es_week %>% filter(Demo_GE1 == 2)

# day
affect_language_es_day_men <- affect_language_es_day %>% filter(Demo_GE1 == 1)
affect_language_es_day_women <- affect_language_es_day %>% filter(Demo_GE1 == 2)

# moment
affect_language_es_threehrs_men <- affect_language_es_threehrs %>% filter(Demo_GE1 == 1)
affect_language_es_threehrs_women <- affect_language_es_threehrs %>% filter(Demo_GE1 == 2)

#### COMPUTE CORRELATIONS ####

## trait

targets_trait <- c("pa_panas", "na_panas")
features <- colnames(affect_language)[match("typing_sessions_count", colnames(affect_language)):match("emoticon_laughing", colnames(affect_language))]

# all text
cor_trait <- cor_table(affect_language, targets_trait, features)
cor_trait_men <- cor_table(affect_language_men, targets_trait, features)
cor_trait_women <- cor_table(affect_language_women, targets_trait, features)

# private
cor_trait_private <- cor_table(affect_language_private, targets_trait, features)
cor_trait_private_men <- cor_table(affect_language_private_men, targets_trait, features)
cor_trait_private_women <- cor_table(affect_language_private_women, targets_trait, features)

# public
cor_trait_public <- cor_table(affect_language_public, targets_trait, features)
cor_trait_public_men <- cor_table(affect_language_public_men, targets_trait, features)
cor_trait_public_women <- cor_table(affect_language_public_women, targets_trait, features)

## weekly

targets_week <- c("valence_week", "arousal_week")

cor_week <- cor_table(affect_language_es_week, targets_week, features)
cor_week_men <- cor_table(affect_language_es_week_men, targets_week, features)
cor_week_women <- cor_table(affect_language_es_week_women, targets_week, features)

## daily

targets_day <- c("valence_day", "arousal_day")

cor_day <- cor_table(affect_language_es_day, targets_day, features)
cor_day_men <- cor_table(affect_language_es_day_men, targets_day, features)
cor_day_women <- cor_table(affect_language_es_day_women, targets_day, features)

## momentary

targets_moment <- c("valence", "arousal", "diff_valence", "diff_arousal")

cor_moment <- cor_table(affect_language_es_threehrs, targets_moment, features)
cor_moment_men <- cor_table(affect_language_es_threehrs_men, targets_moment, features)
cor_moment_women <- cor_table(affect_language_es_threehrs_women, targets_moment, features)

#### INVESTIGATE FEATURE GROUPS ####

# get vector with feature names from each group
typingdynamics <- colnames(affect_language)[match("typing_sessions_duration_md", colnames(affect_language)):match("share_words_removed_sd", colnames(affect_language))]
dictionaries <- colnames(affect_language)[match("wordsentiment_md", colnames(affect_language)):match("LIWC_You_Formal", colnames(affect_language))]
emojiemoticons <- colnames(affect_language)[match("emoji_session_mean", colnames(affect_language)):match("emoticon_laughing", colnames(affect_language))]

## trait - all text

# typing dynamics
cor_group(cor_trait, typingdynamics, "trait_typingdynamics")

# dictionaries - split by gender and combined
cor_group(cor_trait, dictionaries, "trait_dictionaries")
cor_group(cor_trait_men, dictionaries, "trait_men_dictionaries")
cor_group(cor_trait_women, dictionaries, "trait_women_dictionaries")

# emoji & emoticons - split by gender and combined
cor_group(cor_trait, emojiemoticons, "trait_emojiemoticons")
cor_group(cor_trait_men, emojiemoticons, "trait_men_emojiemoticons")
cor_group(cor_trait_women, emojiemoticons, "trait_women_emojiemoticons")

## trait - private

# typing dynamics
cor_group(cor_trait_private, typingdynamics, "trait_private_typingdynamics")

# dictionaries - split by gender and combined
cor_group(cor_trait_private, dictionaries, "trait_private_dictionaries")
cor_group(cor_trait_private_men, dictionaries, "trait_private_men_dictionaries")
cor_group(cor_trait_private_women, dictionaries, "trait_private_women_dictionaries")

# emoji & emoticons - split by gender and combined
cor_group(cor_trait_private, emojiemoticons, "trait_private_emojiemoticons")
cor_group(cor_trait_private_men, emojiemoticons, "trait_private_men_emojiemoticons")
cor_group(cor_trait_private_women, emojiemoticons, "trait_private_women_emojiemoticons")

## trait - public

# typing dynamics
cor_group(cor_trait_public, typingdynamics, "trait_public_typingdynamics")

# dictionaries - split by gender and combined
cor_group(cor_trait_public, dictionaries, "trait_public_dictionaries")
cor_group(cor_trait_public_men, dictionaries, "trait_public_men_dictionaries")
cor_group(cor_trait_public_women, dictionaries, "trait_public_women_dictionaries")

# emoji & emoticons - split by gender and combined
cor_group(cor_trait_public, emojiemoticons, "trait_public_emojiemoticons")
cor_group(cor_trait_public_men, emojiemoticons, "trait_public_men_emojiemoticons")
cor_group(cor_trait_public_women, emojiemoticons, "trait_public_women_emojiemoticons")

## weekly

# typing dynamics
cor_group(cor_week, typingdynamics, "week_typingdynamics")

# dictionaries - split by gender and combined
cor_group(cor_week, dictionaries, "week_typingdynamics")
cor_group(cor_week_men, dictionaries, "week_men_typingdynamics")
cor_group(cor_week_women, dictionaries, "week_women_typingdynamics")

# emoji & emoticons - split by gender and combined
cor_group(cor_week, emojiemoticons, "week_emojiemoticons")
cor_group(cor_week_men, emojiemoticons, "week_men_emojiemoticons")
cor_group(cor_week_women, emojiemoticons, "week_women_emojiemoticons")

## daily

# typing dynamics
cor_group(cor_day, typingdynamics, "day_typingdynamics")

# dictionaries - split by gender and combined
cor_group(cor_day, dictionaries, "day_dictionaries")
cor_group(cor_day_men, dictionaries, "day_men_dictionaries")
cor_group(cor_day_women, dictionaries, "day_women_dictionaries")

# emoji & emoticons - split by gender and combined
cor_group(cor_day, emojiemoticons, "day_emojiemoticons")
cor_group(cor_day_men, emojiemoticons, "day_men_emojiemoticons")
cor_group(cor_day_women, emojiemoticons, "day_women_emojiemoticons")

## momentary

# typing dynamics
cor_group(cor_moment, typingdynamics, "moment_typingdynamics")

# dictionaries - split by gender and combined
cor_group(cor_moment, dictionaries, "moment_dictionaries")
cor_group(cor_moment_men, dictionaries, "moment_men_dictionaries")
cor_group(cor_moment_women, dictionaries, "moment_women_dictionaries")

# emoji & emoticons - split by gender and combined
cor_group(cor_moment, emojiemoticons, "moment_emojiemoticons")
cor_group(cor_moment_men, emojiemoticons, "moment_men_emojiemoticons")
cor_group(cor_moment_women, emojiemoticons, "moment_women_emojiemoticons")

#### FIGURE: DICTIONARIES ACROSS CONTEXTS ####

# read in data
 
trait_men_dictionaries <- read.csv2(file="results/insights/trait_men_dictionaries.csv")
trait_women_dictionaries <- read.csv2(file="results/insights/trait_women_dictionaries.csv")

trait_private_men_dictionaries <- read.csv2(file="results/insights/trait_private_men_dictionaries.csv")
trait_private_women_dictionaries <- read.csv2(file="results/insights/trait_private_women_dictionaries.csv")

trait_public_men_dictionaries <- read.csv2(file="results/insights/trait_public_men_dictionaries.csv")
trait_public_women_dictionaries <- read.csv2(file="results/insights/trait_public_women_dictionaries.csv")

# create df with full names of dictionaries and group
names_dictionaries_df <- data.frame(
  short = c("wordsentiment_md", "wordsentiment_sd", "wordsentiment_min", "wordsentiment_max",
            "LIWC_Posemo",
            "LIWC_Affect", "LIWC_Negemo", "LIWC_Anger", "LIWC_Anx", "LIWC_Sad", 
            "LIWC_I", "LIWC_We", "LIWC_You_Sing", "LIWC_You_Plur", "LIWC_Other", "LIWC_SheHe",   # function words
            "LIWC_Adj", "LIWC_Interrog", "LIWC_Prep", "LIWC_Conj", # grammar
            "LIWC_Social", "LIWC_Family", "LIWC_Friends", "LIWC_Female", "LIWC_Male", # social processes
            "LIWC_Cogproc", "LIWC_Compare",  "LIWC_Cause", "LIWC_Certain" , "LIWC_Insight", # cognitive processes
            "LIWC_Percept", "LIWC_See", "LIWC_Hear", "LIWC_Feel", # perceptual processes
            "LIWC_Bio", "LIWC_Body", "LIWC_Health" , "LIWC_Death", "LIWC_Ingest", # biological processes
            "LIWC_Drives", "LIWC_Affiliation", "LIWC_Power", "LIWC_Reward", "LIWC_Achieve", # drives
            "LIWC_FocusPresent", "LIWC_FocusPast", "LIWC_FocusFuture", # time orientation
            "LIWC_Relative", "LIWC_Motion", "LIWC_Space",  "LIWC_Time", # relativity
            "LIWC_Home", "LIWC_Relig", "LIWC_Work", "LIWC_Leisure", "LIWC_Money", # personal concerns
            "LIWC_Informal", "LIWC_Assent", "LIWC_Filler", "LIWC_Netspeak", "LIWC_Monflu", "LIWC_Swear"# informal language
  ),
  full =   c("Word sentiment (Md)", 
             "Negative emotion", ">Anger", ">Anxiety", # sentiment
             "1st person singular", "1st person plural", "2nd person singular", "2nd person plural", "3rd person", "3rd person singular",  # function words
             "Common adjectives", "Interrogatives", "Prepositions",  "Conjunctions", # grammar
             "Male references", # social processes
             "Comparisons",  "Causation" , "Certainty", "Insight",  # cognitive processes
             "Seeing", # perceptual processes
             "Biological processes", ">Body", ">Health" , ">Death", ">Ingestion", # biological processes
             "Drives", ">Affiliation", ">Power", ">Reward", # drive
             "Home", "Religion", "Work", # personal
             "Present focus", # time orientation
             "Relativity", ">Motion", ">Space", # relativity
             "Informal language", ">Assent", ">Filler words", ">Netspeak", ">Swear words" # informal
  ),
  group = c(
  )
)

# Create a data frame with three columns for LIWC categories
df <- data.frame(
  short_name = c("wordsentiment_md", "wordsentiment_sd", "wordsentiment_min", "wordsentiment_max",
                 "LIWC_Funct", "LIWC_Pronoun", "LIWC_Ppron", "LIWC_I", "LIWC_We", "LIWC_You", "LIWC_Shehe", "LIWC_They", "LIWC_Ipron", "LIWC_Article", 
                 "LIWC_Prep", "LIWC_Auxverb", "LIWC_Adverb", "LIWC_Conj", "LIWC_Negate", "LIWC_Verb", "adj", "compare", "interrog", "number", "quant", 
                 "LIWC_Affect", "LIWC_Posemo", "LIWC_Negemo", "LIWC_Anx", "LIWC_Anger", "LIWC_Sad", 
                 "LIWC_Social", "LIWC_Family", "LIWC_Friend", "LIWC_Female", "LIWC_Male", 
                 "LIWC_Cogproch", "LIWC_Insight", "LIWC_Cause", "discrep", "tentat", "certain", "inhib", "incl", "excl", 
                 "LIWC_Percept", "LIWC_See", "LIWC_Hear", "LIWC_Feel", 
                 "LIWC_Bio", "LIWC_Body", "LIWC_Health", "LIWC_Sexual", "LIWC_Ingest", 
                 "relativ", "motion", "space", "time", 
                 "work", "achieve", "leisure", "home", "money", "relig", "death", 
                 "LIWC_Informal", "LIWC_Assent", "LIWC_Filler", "LIWC_Netspeak", "LIWC_Monflu", "LIWC_Swear" # informal language 
  ),
  full_name = c("Function Words", "Pronouns", "Personal Pronouns", "I", "We", "You", "He/She", "They", "Impersonal Pronouns", "Articles", "Prepositions", "Auxiliary Verbs", "Adverbs", "Conjunctions", "Negations", "Verbs", "Adjectives", "Comparisons", "Interrogatives", "Numbers", "Quantifiers", "Affective Processes", "Positive Emotions", "Negative Emotions", "Anxiety", "Anger", "Sadness", "Social Processes", "Family", "Friends", "Female References", "Male References", "Cognitive Processes", "Insight", "Causation", "Discrepancies", "Tentative", "Certainty", "Inhibition", "Inclusion", "Exclusion", "Perception", "See", "Hear", "Feel", "Biological Processes", "Body", "Health", "Sexual", "Ingestion", "Relativity", "Motion", "Space", "Time", "Work", "Achievement", "Leisure", "Home", "Money", "Religion", "Death", "Assent", "Nonfluencies", "Fillers"),
  category_group = c(rep("Language Style", 21), rep("Affective Processes", 3), rep("Social Processes", 5), rep("Cognitive Processes", 12), rep("Biological Processes", 7), rep("Personal Concerns", 10), "Other")
)

# Add the prefix "LIWC_" to each variable in the "short_name" column
df$short_name <- paste("LIWC_", df$short_name, sep = "")

# View the modified data frame
df





# filter out those dictionaries that are significant for any context

trait_men_dictionaries_filtered <- trait_men_dictionaries %>% group_by(feature) %>% filter(any(p_value < 0.05)) %>% ungroup()
trait_women_dictionaries_filtered <- trait_women_dictionaries %>% group_by(feature) %>% filter(any(p_value < 0.05)) %>% ungroup()

trait_private_men_dictionaries_filtered <- trait_private_men_dictionaries %>% group_by(feature) %>% filter(any(p_value < 0.05)) %>% ungroup()
trait_private_women_dictionaries_filtered <- trait_private_women_dictionaries %>% group_by(feature) %>% filter(any(p_value < 0.05)) %>% ungroup()

trait_public_men_dictionaries_filtered <- trait_public_men_dictionaries %>% group_by(feature) %>% filter(any(p_value < 0.05)) %>% ungroup()
trait_public_women_dictionaries_filtered <- trait_public_women_dictionaries %>% group_by(feature) %>% filter(any(p_value < 0.05)) %>% ungroup()

# # get significant dictionaries across contexts
# dictionaries_names <- unique(c(unique(trait_men_dictionaries_filtered$feature), 
#                                unique(trait_women_dictionaries_filtered$feature), 
#                                unique(trait_private_men_dictionaries_filtered$feature),
#                                unique(trait_private_women_dictionaries_filtered$feature),
#                                unique(trait_public_men_dictionaries_filtered$feature),
#                                unique(trait_public_women_dictionaries_filtered$feature)))
# 
# # create one figure with corrs for trait across contexts to get an overview of all corrs (before deep dive)
# 
# ## create one corr plot across contexts
# 
# # only keep significant dictionaries (before p adjustment, mark those in bold that are still significant after p adjustment)
# 
# # reorder according to liwc groups
# dictionaries_ordered <- c("wordsentiment_md", "LIWC_Negemo", "LIWC_Anger", "LIWC_Anx", # sentiment
#                  "LIWC_I", "LIWC_We", "LIWC_You_Sing", "LIWC_You_Plur", "LIWC_Other", "LIWC_SheHe",   # function words
#                  "LIWC_Adj", "LIWC_Interrog", "LIWC_Prep", "LIWC_Conj", # grammar
#                  "LIWC_Male", # social processes
#                  "LIWC_Compare",  "LIWC_Cause", "LIWC_Certain" , "LIWC_Insight", # cognitive processes
#                  "LIWC_See",  # perceptual processes
#                  "LIWC_Bio", "LIWC_Body", "LIWC_Health" , "LIWC_Death", "LIWC_Ingest", # biological processes
#                  "LIWC_Drives", "LIWC_Affiliation", "LIWC_Power", "LIWC_Reward",  # drive
#                  "LIWC_Home", "LIWC_Relig", "LIWC_Work", # personal
#                  "LIWC_FocusPresent", # time orientation
#                  "LIWC_Relativ", "LIWC_Motion", "LIWC_Space", # relativity
#                  "LIWC_Informal", "LIWC_Assent", "LIWC_Filler", "LIWC_Netspeak", "LIWC_Swear"# informal 
#                  )

# # get relevant dictionaries in correct order (reverse order for plotting)
# trait_men_dictionaries_plot <- trait_men_dictionaries %>% filter (feature %in% dictionaries_ordered) %>% arrange(factor(feature, levels = rev(dictionaries_ordered)))
# trait_women_dictionaries_plot <- trait_women_dictionaries %>% filter (feature %in% dictionaries_ordered) %>% arrange(factor(feature, levels = rev(dictionaries_ordered)))
# 
# trait_private_men_dictionaries_plot <- trait_private_men_dictionaries %>% filter (feature %in% dictionaries_ordered) %>% arrange(factor(feature, levels = rev(dictionaries_ordered)))
# trait_private_women_dictionaries_plot <- trait_private_women_dictionaries %>% filter (feature %in% dictionaries_ordered) %>% arrange(factor(feature, levels = rev(dictionaries_ordered)))
# 
# trait_public_men_dictionaries_plot <- trait_public_men_dictionaries %>% filter (feature %in% dictionaries_ordered) %>% arrange(factor(feature, levels = rev(dictionaries_ordered)))
# trait_public_women_dictionaries_plot <- trait_public_women_dictionaries %>% filter (feature %in% dictionaries_ordered) %>% arrange(factor(feature, levels = rev(dictionaries_ordered)))

# transform to correlation matrix for plotting

# r
trait_men_dictionaries_mat <- transform_to_cormatrix(trait_men_dictionaries_filtered, "r")
trait_women_dictionaries_mat <- transform_to_cormatrix(trait_women_dictionaries_filtered, "r")

trait_private_men_dictionaries_mat <- transform_to_cormatrix(trait_private_men_dictionaries_filtered, "r")
trait_private_women_dictionaries_mat <- transform_to_cormatrix(trait_private_women_dictionaries_filtered, "r")

trait_public_men_dictionaries_mat <- transform_to_cormatrix(trait_public_men_dictionaries_filtered, "r")
trait_public_women_dictionaries_mat <- transform_to_cormatrix(trait_public_women_dictionaries_filtered, "r")

# p values
trait_men_dictionaries_pmat <- transform_to_cormatrix(trait_men_dictionaries_filtered, "p_value")
trait_women_dictionaries_pmat <- transform_to_cormatrix(trait_women_dictionaries_filtered, "p_value")

trait_private_men_dictionaries_pmat <- transform_to_cormatrix(trait_private_men_dictionaries_filtered, "p_value")
trait_private_women_dictionaries_pmat <- transform_to_cormatrix(trait_private_women_dictionaries_filtered, "p_value")

trait_public_men_dictionaries_pmat <- transform_to_cormatrix(trait_public_men_dictionaries_filtered, "p_value")
trait_public_women_dictionaries_pmat <- transform_to_cormatrix(trait_public_women_dictionaries_filtered, "p_value")

# corrected p values
trait_men_dictionaries_padjmat <- transform_to_cormatrix(trait_men_dictionaries_filtered, "p_adjust") 
trait_women_dictionaries_padjmat <- transform_to_cormatrix(trait_women_dictionaries_filtered, "p_adjust")

trait_private_men_dictionaries_padjmat <- transform_to_cormatrix(trait_private_men_dictionaries_filtered, "p_adjust")
trait_private_women_dictionaries_padjmat <- transform_to_cormatrix(trait_private_women_dictionaries_filtered, "p_adjust")

trait_public_men_dictionaries_padjmat <- transform_to_cormatrix(trait_public_men_dictionaries_filtered, "p_adjust")
trait_public_women_dictionaries_padjmat <- transform_to_cormatrix(trait_public_women_dictionaries_filtered, "p_adjust")

# check for sign corrected p values
trait_men_dictionaries_padjcheck <- trait_men_dictionaries_padjmat < .05
trait_women_dictionaries_padjcheck <- trait_women_dictionaries_padjmat < .05

trait_private_men_dictionaries_padjcheck <- trait_private_men_dictionaries_padjmat < .05
trait_private_women_dictionaries_padjcheck <- trait_private_women_dictionaries_padjmat < .05

trait_public_men_dictionaries_padjcheck <- trait_public_men_dictionaries_padjmat < .05
trait_public_women_dictionaries_padjcheck <- trait_public_women_dictionaries_padjmat < .05

# create plots for each group

trait_men_dictionaries_corplot = ggcorrplot(trait_men_dictionaries_mat, 
                                                                method = "square", 
                                                                title = "All\nm\nn=232", 
                                                                legend.title = "Pearson\ncorrelation", 
                                                                lab = TRUE, 
                                                                lab_size = 3, 
                                                                sig.level = 0.05,
                                                                insig = "blank",
                                                                #pch.col = "grey75",
                                                                p.mat = trait_men_dictionaries_pmat,
                                            #tl.cex = ifelse(trait_men_dictionaries_padjcheck, 1.5, 1),
                                            
                                                                ggtheme = theme_minimal, 
                                                                outline.color = "lightgray",
                                                                colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 11),
           axis.text.y = ggplot2::element_text(size = 11),
           plot.title = element_text(size=10)) 


trait_women_dictionaries_corplot = ggcorrplot(trait_women_dictionaries_mat, 
                                         method = "square", 
                                         title = "All\nf\nn=182", 
                                         legend.title = "Pearson\ncorrelation", 
                                         lab = TRUE, 
                                         lab_size = 3,
                                         sig.level = 0.05,
                                         insig = "blank",
                                         #pch.col = "grey40",
                                         p.mat = trait_women_dictionaries_pmat,
                                         #tl.cex = ifelse(trait_women_dictionaries_padjcheck, 1.5, 1),
                                         
                                         ggtheme = theme_minimal, 
                                         outline.color = "lightgray",
                                         colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 11),
           axis.text.y = ggplot2::element_text(size = 11),
           plot.title = element_text(size=10)) 

trait_private_men_dictionaries_corplot = ggcorrplot(trait_private_men_dictionaries_mat, 
                                          method = "square", 
                                          title = "Private\nm\nn=152", 
                                          legend.title = "Pearson\ncorrelation", 
                                          lab = TRUE, 
                                          lab_size = 3,
                                          sig.level = 0.05,
                                          insig = "blank",
                                          #pch.col = "grey40",
                                          p.mat = trait_private_men_dictionaries_pmat,
                                          #tl.cex = ifelse(trait_private_men_dictionaries_padjcheck, 1.5, 1),
                                          
                                          ggtheme = theme_minimal, 
                                          outline.color = "lightgray",
                                          colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 11),
           axis.text.y = ggplot2::element_text(size = 11),
           plot.title = element_text(size=10)) 

trait_private_women_dictionaries_corplot = ggcorrplot(trait_private_women_dictionaries_mat, 
                                                 method = "square", 
                                                 title = "Private\nf\nn=111", 
                                                 legend.title = "Pearson\ncorrelation", 
                                                 lab = TRUE, 
                                                 lab_size = 3,
                                                 sig.level = 0.05,
                                                 insig = "blank",
                                                 #pch.col = "grey40",
                                                 p.mat = trait_private_women_dictionaries_pmat,
                                                 #tl.cex = ifelse(trait_private_women_dictionaries_padjcheck, 1.5, 1),
                                                 
                                                 ggtheme = theme_minimal, 
                                                 outline.color = "lightgray",
                                                 colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 11),
           axis.text.y = ggplot2::element_text(size = 11),
           plot.title = element_text(size=10)) 


trait_public_men_dictionaries_corplot = ggcorrplot(trait_public_men_dictionaries_mat, 
                                                 method = "square", 
                                                 title = "Public\nm\nn=53", 
                                                 legend.title = "Pearson\ncorrelation", 
                                                 lab = TRUE, 
                                                 lab_size = 3,
                                                 sig.level = 0.05,
                                                 insig = "blank",
                                                 #pch.col = "grey40",
                                                 p.mat = trait_public_men_dictionaries_pmat,
                                                 #tl.cex = ifelse(trait_public_men_dictionaries_padjcheck, 1.5, 1),
                                                 ggtheme = theme_minimal, 
                                                 outline.color = "lightgray",
                                                 colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 11),
           axis.text.y = ggplot2::element_text(size = 11),
           plot.title = element_text(size=10)) 

trait_public_women_dictionaries_corplot = ggcorrplot(trait_public_women_dictionaries_mat, 
                                                   method = "square", 
                                                   title = "Public\nf\nn=65", 
                                                   legend.title = "Pearson\ncorrelation", 
                                                   lab = TRUE, 
                                                   lab_size = 3,
                                                   sig.level = 0.05,
                                                   insig = "blank",
                                                   #pch.col = "grey40",
                                                   p.mat = trait_public_women_dictionaries_pmat,
                                                   #tl.cex = ifelse(trait_public_women_dictionaries_padjcheck, 1.5, 1),
                                                   ggtheme = theme_minimal, 
                                                   outline.color = "lightgray",
                                                   colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 11),
           axis.text.y = ggplot2::element_text(size = 11),
           plot.title = element_text(size=10)) 


  


# combine plots into one figure

trait_dictionaries_plot_overview <- 
  trait_men_dictionaries_corplot  + theme(legend.position = "none") + scale_x_discrete(labels=rev(c('Pos. Affect', 'Neg. Affect'))) + scale_y_discrete(labels=rev(names_vector)) + 
  trait_women_dictionaries_corplot  + theme(legend.position = "none") + scale_x_discrete(labels=rev(c('Pos. Affect', 'Neg. Affect'))) + scale_y_discrete(labels=rev(names_vector)) + 
  trait_private_men_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=rev(c('Pos. Affect', 'Neg. Affect'))) + scale_y_discrete(labels=rev(names_vector)) +
  trait_private_women_dictionaries_corplot + theme(legend.position = "none")+ scale_x_discrete(labels=rev(c('Pos. Affect', 'Neg. Affect'))) + scale_y_discrete(labels=rev(names_vector)) +
  trait_public_men_dictionaries_corplot + theme(legend.position = "none") + scale_x_discrete(labels=rev(c('Pos. Affect', 'Neg. Affect'))) + scale_y_discrete(labels=rev(names_vector)) +
  trait_public_women_dictionaries_corplot + scale_x_discrete(labels=rev(c('Pos. Affect', 'Neg. Affect'))) + scale_y_discrete(labels=rev(names_vector)) +
  plot_layout(ncol = 6)

# save figure
png(file="figures/trait_dictionaries_plot_overview.png",width=1000, height=1000)
trait_dictionaries_plot_overview
dev.off()

#### FIGURE: DEEP DIVE DICTIONARIES ACROSS CONTEXTS ####

## only do this for selected categories from the overview!

# create vector with names of dictionary categories to create the figures for
target_dictionaries <- c("LIWC_I", "LIWC_We", # personal pronouns
                   "LIWC_Posemo", "LIWC_Negemo", "LIWC_Sad", "LIWC_Anx", "LIWC_Anger",  # emotion dictionaries
                   "wordsentiment_md", "wordsentiment_sd", "wordsentiment_min", # sentiws 
                   "LIWC_FocusPast", "LIWC_FocusPresent", "LIWC_FocusFuture", # time orientation
                   "LIWC_Social","LIWC_Friend", # social processes
                   "LIWC_Body", "LIWC_Health", # biological processes
                   "LIWC_Work","LIWC_Leisure", # personal concerns
                   "LIWC_Informal", "LIWC_Swear", "LIWC_Netspeak", # informal language
                   "LIWC_Compare", "LIWC_Interrog" # grammar
)


# create figures for contexts (public vs. private)
context_figures(target_dictionaries)

# create one figure with most relevant dictionary categories for contexts

# load selected plots

plot_context_LIWC_I <- readRDS("figures/r_files/plot_context_LIWC_I.RData")
plot_context_LIWC_We <- readRDS("figures/r_files/plot_context_LIWC_We.RData")

plot_context_LIWC_Negemo <- readRDS("figures/r_files/plot_context_LIWC_Negemo.RData")
plot_context_LIWC_Posemo <- readRDS("figures/r_files/plot_context_LIWC_Posemo.RData")
plot_context_wordsentiment_md <- readRDS("figures/r_files/plot_context_wordsentiment_md.RData")

plot_context_LIWC_Compare <- readRDS("figures/r_files/plot_context_LIWC_Compare.RData")

# arrange plots (2x3 matrix)

context_dictionaries_deepdive <- 
  (plot_context_LIWC_Posemo + theme(legend.position = "none", axis.title.x =element_blank()) + labs(title = "Positive emotion") +
  plot_context_LIWC_Negemo + theme(legend.position = "none", axis.title.x =element_blank(), axis.title.y=element_blank()) + labs(title = "Negative emotion") ) /
  (plot_context_wordsentiment_md + theme(legend.position = "none", legend.title = element_blank(), axis.title.x =element_blank()) + labs(title = "Word sentiment (Md)") +
  plot_context_LIWC_Compare + theme(legend.title = element_blank(),  axis.title.y=element_blank(), axis.title.x =element_blank()) + labs(title = "Comparisons") ) /
    (plot_context_LIWC_I  + theme(legend.position = "none") + labs(title = "1st person singular") +
    plot_context_LIWC_We + theme(legend.position = "none", axis.title.y=element_blank()) + labs(title = "1st person plural") )


# save figure
png(file="figures/context_dictionaries_deepdive.png",width=1000, height=1200)
context_dictionaries_deepdive
dev.off()

#### FIGURE: DICTIONARIES ACROSS TIME ####

## create one corr plot across contexts

# get relevant liwc cats across contexts
# dictionaries_liwc_cats_time <- unique(c(colnames(cor_language_affect_es_week_dictionaries_filtered),
#                                    colnames(cor_language_affect_es_day_dictionaries_filtered),
#                                    colnames(cor_language_affect_es_threehrs_dictionaries_filtered)))

# get liwc cats eg for at least two time frames?
# count how often each cat occurs 
all_cats = c(colnames(cor_language_affect_es_week_dictionaries_filtered),
  colnames(cor_language_affect_es_day_dictionaries_filtered),
  colnames(cor_language_affect_es_threehrs_dictionaries_filtered))

mintwo_cats = table(all_cats) >= 2

final_cats_time <- names(which(mintwo_cats == TRUE))

# extract corrs 
sub_matrix_week <- cor_language_affect_es_week_dictionaries$r[c("valence_week", "arousal_week"), final_cats_time] # subset matrix with all correlations 
sub_matrix_day <- cor_language_affect_es_day_dictionaries$r[c("valence_day", "arousal_day"), final_cats_time] # subset matrix with all correlations 
sub_matrix_threehrs <- cor_language_affect_es_threehrs_dictionaries$r[c("valence", "arousal", "diff_valence", "diff_arousal"), final_cats_time] # subset matrix with all correlations 

# reorder according to liwc groups
order_vector_time <- c("wordsentiment_md", "LIWC_Affect",  "LIWC_Posemo", "LIWC_Anx", # sentiment
                  "LIWC_I", "LIWC_We", "LIWC_You_Total", "LIWC_You_Plur", "LIWC_Other", "LIWC_SheHe", "LIWC_They" ,   # function words
                   "LIWC_Interrog",  # grammar
                  "LIWC_Family",  # social processes
                   # cognitive processes
                  "LIWC_Percept", "LIWC_See", "LIWC_Hear", # perceptual processes
                  "LIWC_Death", "LIWC_Ingest",  # biological processes
                  "LIWC_Drives", "LIWC_Affiliation", "LIWC_Achiev", # drive
                  # personal
                  "LIWC_Time")  # time orientation
                  # relativity
                  # informal 

sub_matrix_week <- sub_matrix_week[, rev(order_vector_time)]
sub_matrix_day <- sub_matrix_day[, rev(order_vector_time)]
sub_matrix_threehrs <- sub_matrix_threehrs[, rev(order_vector_time)]

# create plot

affect_dictionaries_week_plot = ggcorrplot(sub_matrix_week, 
                                      method = "square", 
                                      title = "Week", 
                                      legend.title = "Pearson\ncorrelation", 
                                      lab = TRUE, 
                                      lab_size = 4, 
                                      ggtheme = theme_minimal, 
                                      outline.color = "lightgray",
                                      colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 15),
           axis.text.y = ggplot2::element_text(size = 15),
           plot.title = element_text(size=18)) 

affect_dictionaries_day_plot = ggcorrplot(sub_matrix_day, 
                                              method = "square", 
                                              title = "Day", 
                                              legend.title = "Pearson\ncorrelation", 
                                              lab = TRUE, 
                                              lab_size = 4, 
                                              ggtheme = theme_minimal, 
                                              outline.color = "lightgray",
                                              colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 15),
           axis.text.y = ggplot2::element_text(size = 15),
           plot.title = element_text(size=18)) 

affect_dictionaries_threehrs_plot = ggcorrplot(sub_matrix_threehrs, 
                                             method = "square", 
                                             title = "Moment", 
                                             legend.title = "Pearson\ncorrelation", 
                                             lab = TRUE, 
                                             lab_size = 4, 
                                             ggtheme = theme_minimal, 
                                             outline.color = "lightgray",
                                             colors = c("#6D9EC1", "white", "#E46726")
) +  theme(axis.text.x = ggplot2::element_text(size = 15),
           axis.text.y = ggplot2::element_text(size = 15),
           plot.title = element_text(size=18)) 

# create vector with names 
names_vector_time <- c("Word sentiment (Md)", "Affective processes",  "Positive emotion", "Anxiety", # sentiment
                       "1st person singular", "1st person plural", "2nd person", "2nd person plural", "3rd person", "3rd person singular", "3rd person plural" ,   # function words
                       "Interrogatives",  # grammar
                       "Family",  # social processes
                       # cognitive processes
                       "Perceptual processes", "Seeing", "Hearing", # perceptual processes
                       "Death", "Ingestion",  # biological processes
                       "Drives", "Affiliation", "Achievement", # drive
                       # personal
                       "Time")  # time orientation
# relativity
# informal 


# combine into one figure

affect_dictionaries_time_plot_overview <- 
  affect_dictionaries_week_plot  + theme(legend.position = "none") + scale_x_discrete(labels=c('Valence', 'Arousal')) + scale_y_discrete(labels=rev(names_vector_time))+ 
  affect_dictionaries_day_plot + theme(legend.position = "none") + scale_x_discrete(labels=c('Valence', 'Arousal'))  +  scale_y_discrete(labels = NULL) +
  affect_dictionaries_threehrs_plot + scale_x_discrete(labels=c('Valence', "Valence Fluct.", 'Arousal', "Arousal Fluct.")) + scale_y_discrete(labels = NULL)


# save figure
png(file="figures/affect_dictionaries_time_plot_overview.png",width=800, height=1200)
affect_dictionaries_time_plot_overview
dev.off()


## deep dive - time 

# create figures for time frames (weekly, daily, momentary)
time_figures(target_dictionaries) #here are still some bugs for niche categories

# create one figure with most relevant dictionary categories for time frames

# load figures
plot_time_LIWC_Negemo <- readRDS("figures/r_files/plot_time_LIWC_Negemo.RData")
plot_time_LIWC_Posemo <- readRDS("figures/r_files/plot_time_LIWC_Posemo.RData")
plot_time_wordsentiment_md <- readRDS("figures/r_files/plot_time_wordsentiment_md.RData")
plot_time_wordsentiment_sd <- readRDS("figures/r_files/plot_time_wordsentiment_sd.RData")

plot_time_LIWC_I <- readRDS("figures/r_files/plot_time_LIWC_I.RData")
plot_time_LIWC_We <- readRDS("figures/r_files/plot_time_LIWC_We.RData")

# arrange plots (3x2 matrix)

time_dictionaries_deepdive <- 
  (plot_time_LIWC_Posemo  + labs(title = "Positive emotion")  + theme(axis.title.x =element_blank()) +
     plot_time_LIWC_Negemo + labs(title = "Negative emotion") + theme(axis.title.x =element_blank(), axis.title.y=element_blank())) /
  (plot_time_wordsentiment_md  + labs(title = "Word sentiment (Md)")  + theme(axis.title.x =element_blank()) +
     plot_time_wordsentiment_sd + labs(title = "Word sentiment (SD)") + theme(axis.title.x =element_blank(), axis.title.y=element_blank())) /
  (plot_time_LIWC_I  + labs(title = "1st person singular")  + 
     plot_time_LIWC_We + labs(title = "1st person plural") + theme(legend.position = "none", axis.title.y=element_blank()))

# save figure
png(file="figures/time_dictionaries_deepdive.png",width=1000, height=1200)
time_dictionaries_deepdive
dev.off()

#### FIGURES: EMOJI RANKING FOR TRAIT AFFECT SPLIT BY GENDER ####

# read data

trait_men_emojiemoticons <- read.csv2(file="results/insights/trait_men_emojiemoticons.csv")
trait_women_emojiemoticons <- read.csv2(file="results/insights/trait_women_emojiemoticons.csv")

# split by gender, show top10 per group
# this will be a total of four columns, one for pa and na per gender



#### FIGURES: EMOJI VALENCE-AROUSAL GRID ####

## read in data

# # week
# week_men_emojiemoticons <- read.csv2(file="results/insights/week_men_emojiemoticons.csv")
# week_women_emojiemoticons <- read.csv2(file="results/insights/week_women_emojiemoticons.csv")
# 
# # day
# day_men_emojiemoticons <- read.csv2(file="results/insights/day_men_emojiemoticons.csv")
# day_women_emojiemoticons <- read.csv2(file="results/insights/day_women_emojiemoticons.csv")

# moment
moment_men_emojiemoticons <- read.csv2(file="results/insights/moment_men_emojiemoticons.csv")
moment_women_emojiemoticons <- read.csv2(file="results/insights/moment_women_emojiemoticons.csv")

## plot emoji and emoticons in affect grid for different time windows, split by gender, show all, show grid for moment in final paper, rest in appendix

emoji_figures(moment_men_emojiemoticons, c("valence", "arousal"), "moment_men")
emoji_figures(moment_women_emojiemoticons, c("valence", "arousal"), "moment_women")
emoji_figures(moment_men_emojiemoticons, c("diff_valence", "diff_arousal"), "diff_moment_men")
emoji_figures(moment_women_emojiemoticons, c("diff_valence", "diff_arousal"), "diff_moment_women")

## load emoji plots

plot_emoji_moment_men <- readRDS("figures/r_files/plot_emoji_moment_men.RData")
plot_emoji_moment_women <- readRDS("figures/r_files/plot_emoji_moment_women.RData")
plot_emoji_diff_moment_men <- readRDS("figures/r_files/plot_emoji_diff_moment_men.RData")
plot_emoji_diff_moment_women <- readRDS("figures/r_files/plot_emoji_diff_moment_women.RData")

emoji_preferences_moment <- 
  ((plot_emoji_moment_men + labs(title = "Momentary Affect (Men)", x = "Valence", y = "Arousal"))+
  (plot_emoji_moment_women + labs(title = "Momentary Affect (Women)", x = "Valence", y = "Arousal")))/
  ((plot_emoji_diff_moment_men + labs(title = "Momentary Fluctuation from Affect Baseline (Men)", x = "Valence Fluct.", y = "Arousal Fluct.")) +
  (plot_emoji_diff_moment_women + labs(title = "Momentary Fluctuation from Affect Baseline (Women)", x = "Valence Fluct.", y = "Arousal Fluct.")))

# save figure
agg_png(file="figures/emoji_preferences_moment.png",width=1500, height=1500)
emoji_preferences_moment
dev.off()


### DISTRIBUTION OF AFFECT OUTCOMES ACROSS DATA SETS ####

hist_pa_affect_trait <- ggplot(affect_language, aes(x=pa_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,75))+ 
  scale_x_continuous(name = "Positive Trait Affect (all text)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_na_affect_trait <- ggplot(affect_language, aes(x=na_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,75))+ 
  scale_x_continuous(name = "Negative Trait Affect (all text)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_pa_affect_trait_private <- ggplot(affect_language_private, aes(x=pa_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,40))+ 
  scale_x_continuous(name = "Positive Trait Affect (private)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_na_affect_trait_private <- ggplot(affect_language_private, aes(x=na_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,40))+ 
  scale_x_continuous(name = "Negative Trait Affect (private)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_pa_affect_trait_public <- ggplot(affect_language_public, aes(x=pa_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,25))+ 
  scale_x_continuous(name = "Positive Trait Affect (public)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)

hist_na_affect_trait_public <- ggplot(affect_language_public, aes(x=na_panas)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,25))+ 
  scale_x_continuous(name = "Negative Trait Affect (public)", limits = c(1,5)) + 
  theme_minimal(base_size = 20)



hist_valence_affect_week <- ggplot(affect_language_es_week, aes(x=valence_week)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,150))+
  scale_x_continuous(name = "Valence (weekly)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_week <- ggplot(affect_language_es_week, aes(x=arousal_week)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,150))+
  scale_x_continuous(name = "Arousal (weekly)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_valence_affect_day <- ggplot(affect_language_es_day, aes(x=valence_day)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,1000))+
  scale_x_continuous(name = "Valence (daily)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_day <- ggplot(affect_language_es_day, aes(x=arousal_day)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,1000))+
  scale_x_continuous(name = "Arousal (daily)", breaks = seq(0, 6, 1)) +
  theme_minimal(base_size = 20)

hist_valence_affect_moment <- ggplot(affect_language_es_threehrs, aes(x=valence)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,350))+
  scale_x_continuous(name = "Valence (momentary)", breaks = c(1:6)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_moment <- ggplot(affect_language_es_threehrs, aes(x=arousal)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,350))+
  scale_x_continuous(name = "Arousal (momentary)", breaks = c(1:6)) +
  theme_minimal(base_size = 20)

hist_valence_affect_moment_diff <- ggplot(affect_language_es_threehrs, aes(x=diff_valence)) + 
  geom_histogram() + 
  scale_y_continuous(name = element_blank(), limits = c(0,450))+
  scale_x_continuous(name = "Valence Fluct. (momentary)", seq(-4, 4, 1)) +
  theme_minimal(base_size = 20)

hist_arousal_affect_moment_diff <- ggplot(affect_language_es_threehrs, aes(x=diff_arousal)) + 
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