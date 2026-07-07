library(psych)

# read in data
wave3 <- read.csv2("data/raw/wave3_2021_02_19.csv")

# explore colnames
colnames(wave3)

## compute affect scores from PANAS columns

# positive affect (5-point Likert scale from 1-5)
wave3$pa_panas <- 
  (wave3$PANAS_POS1 + 
  wave3$PANAS_POS2 + 
  wave3$PANAS_POS3 + 
  wave3$PANAS_POS4 + 
  wave3$PANAS_POS5 + 
  wave3$PANAS_POS6 + 
  wave3$PANAS_POS7 + 
  wave3$PANAS_POS8 + 
  wave3$PANAS_POS9 +   
  wave3$PANAS_POS10) /10

# negative affect (5-point Likert scale from 1-5)
wave3$na_panas <- 
  (wave3$PANAS_NEG1 + 
     wave3$PANAS_NEG2 + 
     wave3$PANAS_NEG3 + 
     wave3$PANAS_NEG4 + 
     wave3$PANAS_NEG5 + 
     wave3$PANAS_NEG6 + 
     wave3$PANAS_NEG7 + 
     wave3$PANAS_NEG8 + 
     wave3$PANAS_NEG9 +   
     wave3$PANAS_NEG10) /10

# create new df with panas scores and user id

panas_df <- wave3[, c("pa_panas", "na_panas", "p_0001")]

## remove duplicates that were created when participants used the "back" button - in these cases we use the first entry
panas_df <- panas_df[!duplicated(panas_df$p_0001),]

## descriptives
hist(panas_df$pa_panas)
describe(panas_df$pa_panas)

hist(panas_df$na_panas)
describe(panas_df$na_panas)

# compute cronbach's alpha 
alpha_pa <- alpha(subset(wave3, select = c(PANAS_POS1, PANAS_POS2, PANAS_POS3, PANAS_POS4, PANAS_POS5, PANAS_POS6, PANAS_POS7, PANAS_POS8, PANAS_POS9, PANAS_POS10)))
alpha_na <- alpha(subset(wave3, select = c(PANAS_NEG1, PANAS_NEG2, PANAS_NEG3, PANAS_NEG4, PANAS_NEG5, PANAS_NEG6, PANAS_NEG7, PANAS_NEG8, PANAS_NEG9, PANAS_NEG10)))

# save df

saveRDS(panas_df, "data/helper/panas.rds")

# finish