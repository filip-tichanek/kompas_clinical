# Import packages and custom functions ----

if (TRUE) {
  rm(list = ls())
}
if (TRUE) {
  suppressWarnings(suppressMessages({
    library(tidyverse)
    library(lme4)
    library(stringr)
    library(ggpubr)
    library(emmeans)
    library(gtsummary)
    library(car)
    library(sjPlot)
    library(flextable)
    library(openxlsx)
    library(mgcv)
    library(pROC)
    library(cowplot)
    library(boot)
    library(glmnet)
    library(projpred)
    library(arm)
    library(corrplot)
    library(lubridate)
    library(kableExtra)
    library(ggdist)
    library(bayesplot)
    library(quantreg)
    library(performance)
    library(pheatmap)
    library(gridExtra)
    library(grid)
    library(robustlmm)
    library(ggrepel)
    library(patchwork)
    library(mice)

    # Functions clashes
    select <- dplyr::select
    rename <- dplyr::rename
    mutate <- dplyr::mutate
    recode <- dplyr::recode
    summarize <- dplyr::summarize
    count <- dplyr::count
  }))
}

getwd()
setwd("/home/ticf/GitRepo/ticf/368_MOCA_kompas_clinical/")
source("r/368_functions.R")

# Data ----

## Import ----

dat <- read.xlsx("data/KOMPAS_clinical data_b_correct.xlsx")

## Wrangling ----

### Replace values for Supplementation with 0 and 1

dat <- dat %>%
  mutate(
    across(`aAL_child`:`aSUP_OTH`, ~ as.numeric(if_else(is.na(.), "0", "1"))),
    aAL_adult = as.numeric(if_else(is.na(aAL_adult), "0", "1")),
    aBREAKS = as.numeric(aBREAKS) - 1
  ) %>%
  mutate(
    aBreastFeed_full_stopped = as.numeric(if_else(
      (aAGE * 12) - aBreastFeed_full < 2, 0, 1
    )),
    aBreastFeed_total_stopped = as.numeric(if_else(
      (aAGE * 12) - aBreastFeed_total < 2, 0, 1
    )),
    dev_delay = as.numeric(if_else(aDEV == "2", 1, 0))
  ) %>%
  mutate(
    aBreastFeed_total_stopped =
      if_else(aBreastFeed_full_stopped == 0, 0,
        aBreastFeed_total_stopped
      ),
    aBreastFeed_total =
      if_else(aBreastFeed_total_stopped == 0, aAGE * 12,
        aBreastFeed_total
      )
  ) %>%
  mutate(
    aBreastFeed_total_stopped =
      if_else(aAGE > 18, 1, aBreastFeed_total_stopped)
  ) %>%
  select(-aDEV) %>%
  filter(!is.na(aAGE))

names(dat) <- names(dat) %>%
  str_replace_all("-", "_") %>%
  str_replace_all("/", "_per_")



### Create 4 datasets according to age groups:
#
# - (1) all children
#
# - (2) children > 3 years old
#
# - (3) children < 3 years old
#
# - (4) adults

#### All children

dat_child_all <- dat %>%
  filter(CHILD == "Y") %>%
  select(
    ID, FAM,
    aAGE, SEX, GRP,
    aBreastFeed_full, aBreastFeed_total,
    aBreastFeed_full_stopped, aBreastFeed_total_stopped,
    dev_delay,
    aMASS_Perc:`aM_per_H_PERC`,
    aGLY:aPTH,
    aCros:aFOLAT,
    aIGF1, aUr_Ca:aP_per_Krea, 
    aAL_child, aBREAKS,
    aSUP_VEG1:aSUP_OTH,
    aBiW
  ) %>%
  mutate_if(is.character, factor) %>%
  mutate(log2_age = log2(aAGE))


## Imputation of `aBreastFeed_total` on the basis of age ----

### select data without missing values for `aBreastFeed_total_stopped`
mod_dat <- dat_child_all %>%
  filter(!is.na(aBreastFeed_total_stopped))

### use selected data to run model predicting `aBreastFeed_total_stopped` using `age`
imp_model <- gam(aBreastFeed_total_stopped ~ aAGE,
  data = mod_dat,
  family = binomial(link = "logit")
)

summary(imp_model)
-coef(imp_model)[1] / coef(imp_model)[2]


# Given a relatively small number of children with missing value for `aBreastFeed_total_stopped` and
# its strong correlation with `aAGE`, we will provide single imputation on the basis of `aAGE`
#
# Calculating 1.4518 x 1.772882 gives 2.57387. Counting it with intercept we get exactly 0 on logit
# scale, i.e. 50% probability. Thus, the age of 1.77 years is the time when, on average, children stop
# to drink a breast milk. For mixed-effects models  and elastic net analysis, we will
# consider the children with unknown `aBreastFeed_total_stopped` status to be "= 1" when the age is
# over the 1.773 years.
#
# At first, conserve the original values (including the missing `aBreastFeed_total_stopped`)

dat_child_all_tosum <- dat_child_all %>%
  select(-log2_age)

if (file.exists("data/dat_child_all_tosum.txt") == FALSE) {
  write.table(dat_child_all_tosum, "data/dat_child_all_tosum.txt")
}

dat_child_all_tosum <- read.table("data/dat_child_all_tosum.txt") %>%
  mutate_if(is.character, factor)

dat_child_old_tosum <- dat_child_all_tosum %>%
  filter(aAGE > 3)

dat_child_young_tosum <- dat_child_all_tosum %>%
  filter(aAGE <= 3)


### Lets to wrangle and save data with imputed `aBreastFeed_total_stopped` values

dat_child_all <- dat_child_all %>%
  mutate(
    aBreastFeed_total_stopped =
      if_else(is.na(aBreastFeed_total_stopped) & (aAGE > -coef(imp_model)[1] / coef(imp_model)[2]),
        1, aBreastFeed_total_stopped
      )
  ) %>%
  mutate(
    aBreastFeed_total_stopped =
      if_else(is.na(aBreastFeed_total_stopped) & (aAGE <- coef(imp_model)[1] / coef(imp_model)[2]),
        0, aBreastFeed_total_stopped
      )
  )


if (file.exists("data/dat_child_all.txt") == FALSE) {
  write.table(dat_child_all, "data/dat_child_all.txt")
}

dat_child_all <- read.table("data/dat_child_all.txt") %>%
  mutate_if(is.character, factor) %>%
  mutate(aBreastFeed_full_duration = aBreastFeed_full_stopped * aBreastFeed_full)


### Children > 3 y.o.


dat_child_old <- dat_child_all %>%
  filter(aAGE > 3)

dat_child_old %>% summary()


### Children < 3 y.o.


dat_child_young <- dat_child_all %>%
  filter(aAGE <= 3)

dat_child_young %>% summary()


### Adults

dat_adult <- dat %>%
  filter(CHILD == "N") %>%
  select(
    ID, FAM,
    aAGE, SEX, GRP,
    `aBMI`:`aFFM`,
    aSBP:aPTH,
    aCros:aFOLAT,
    aUr_Ca:aP_per_Krea,
    aAL_adult,
    `aSUP_VEG1`:`aSUP_OTH`,
    aBREAKS, -aIGF1
  ) %>%
  mutate_if(is.character, factor)

dat_adult %>% summary()


### Saving data table

if (file.exists("data/dat_adult.txt") == FALSE) {
  write.table(dat_adult, "data/dat_adult.txt")
}

dat_adult <- read.table("data/dat_adult.txt") %>%
  mutate_if(is.character, factor)


# Food intake data ----
if (file.exists("data/dat_food.txt") == FALSE) {
  dat_food <- read.xlsx("gitignore/data/EXPORT KOMPAS.xlsx")

  dat_food <- dat_food %>%
    select(
      GRP, AGE,
      `Calories(kcal)`,
      `Carbohydrates(g)`,
      `Fat(g)`,
      `Proteins(g)`,
      `Fiber(g)`,
      `Sugars(g)`,
      `Saturated.fats.(SAFA)(g)`,
      `Cholesterol(mg)`,
      `Phosphorus(mg)`,
      `Magnesium(mg)`,
      `Zinc(mg)`,
      `Selenium(μg)`,
      `Iron(mg)`,
      `Calcium(mg)`,
      `Iodine(μg)`
    ) %>%
    mutate(Age_category = if_else(AGE < 18, "Ch > 3 yrs", "Adult")) %>%
    mutate(Age_category = factor(if_else(AGE < 3,
      "Ch < 3 yrs",
      Age_category
    ))) %>%
    select(-AGE)



  write.table(dat_food, "data/dat_food.txt")
}

dat_food <- read.table("data/dat_food.txt") %>%
  mutate(
    Age_category = factor(Age_category),
    GRP = factor(GRP)
  )
