# Install and load required packages 

packages <- c( "dplyr", "tidyr", "ggplot2", "gridExtra") # also try patchwork?
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results

bmr_egemaps_content <- readRDS("results/bmr_egemap_content.RData")
bmr_egemaps_sad <- readRDS("results/bmr_egemap_sad.RData")
bmr_egemaps_arousal <- readRDS("results/bmr_egemap_arousal.RData")

# load df
affect_acoustics <- readRDS("data/affect_acoustics.RData")

####  DATA AMOUNT EFFECS ON PREDICTION PERFORMANCE: TRAIT AFFECT ####

## create plot showing the prediction error on y axis and number of word typed score on x-axis with multiple curves for each target

## get predictions from rf (best performing algo) for pa
aggr_content = bmr_egemaps_content$aggregate()
rr_content = aggr_content$resample_result[[3]]
predictions_content <- as.data.table(rr_content$prediction())

# rename columns
colnames(predictions_content)[colnames(predictions_content) == 'truth'] <- 'truth_content'
colnames(predictions_content)[colnames(predictions_content) == 'response'] <- 'response_content'

## get predictions from rf (best performing algo) for na
aggr_sad = bmr_egemaps_sad$aggregate()
rr_sad = aggr_sad$resample_result[[3]]
predictions_sad <- as.data.table(rr_sad$prediction())

# rename columns
colnames(predictions_sad)[colnames(predictions_sad) == 'truth'] <- 'truth_sad'
colnames(predictions_sad)[colnames(predictions_sad) == 'response'] <- 'response_sad'

## get predictions from rf (best performing algo) for arousal
aggr_arousal = bmr_egemaps_arousal$aggregate()
rr_arousal = aggr_arousal$resample_result[[3]]
predictions_arousal <- as.data.table(rr_arousal$prediction())

# rename columns
colnames(predictions_arousal)[colnames(predictions_arousal) == 'truth'] <- 'truth_arousal'
colnames(predictions_arousal)[colnames(predictions_arousal) == 'response'] <- 'response_arousal'

# create one df
predictions_wordstyped <- cbind(predictions_content, predictions_sad, predictions_arousal, affect_acoustics$Voice.only.duration.in.seconds)

# rename column for voice duration
colnames(predictions_wordstyped)[colnames(predictions_wordstyped) == 'V4'] <- 'wordstyped'

# compute prediction error 
predictions_wordstyped$error_content <- abs(predictions_wordstyped$truth_content - predictions_wordstyped$response_content)
predictions_wordstyped$error_sad <- abs(predictions_wordstyped$truth_sad - predictions_wordstyped$response_sad)
predictions_wordstyped$error_arousal <- abs(predictions_wordstyped$truth_arousal - predictions_wordstyped$response_arousal)

head(predictions_wordstyped)

# convert to long format
predictions_wordstyped_long <-  gather(predictions_wordstyped, condition, error, error_content, error_sad, error_arousal)

# create plot
wordstyped_error_plot <- ggplot(data = predictions_wordstyped_long[,c("wordstyped", "error", "condition")], 
                          aes_string(x= "wordstyped" , y= "error", color = "condition")) +
  #geom_point() +
  stat_smooth(method='loess', se = T, span = 1)+
  scale_x_continuous("Number of word typed") +
  scale_y_continuous("Absolute prediction error") + 
  theme_minimal(base_size = 20)

wordstyped_error_plot

# save plot

png(file="figures/performance_voiceamount.png",width=1000, height=800)

dev.off()

####  DATA AMOUNT EFFECS ON PREDICTION PERFORMANCE: STATE AFFECT ####

# fill from above

# FINISH