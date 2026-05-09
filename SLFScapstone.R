#SLFS data imported as SLFS17_1
library(readr)
SLFS17_1Acapstone <- read_csv("C:/Users/hurst/Downloads/SLFS17_1Acapstone.csv")
View(SLFS17_1Acapstone)
#rename data frame SLFS
SLFS <- SLFS17_1Acapstone

#examine available states by viewing variable setNames
table(SLFS$STNAME)

#create subset of SLFS data to look closely at Colorado schools
colorado_slfs <- subset(SLFS, STNAME == "Colorado")

#exclude charters
coloradopub_slfs <- subset(colorado_slfs, CHARTER_TEXT == "No")

#exclude schools not open at the beginning of the school year
coloradopub_slfs_1 <- subset(coloradopub_slfs, SY_STATUS == 1)

#include "regular schools" only, SCH_TYPE=1
coloradopublic_regular <- subset (coloradopub_slfs_1, SCH_TYPE == 1)

#reduce dataframe to include only the variables necessary for analysis
#include flags
coloradoSLFSreduced <- coloradopublic_regular[, c(
  "NCESSCH", "SCH_NAME", "STNAME", 
  "MEMBER", "LEAID", "LEA_NAME", "FTE", "LEVEL", "LOCALE", 
  "Z39S", "Z40S", "Z33S",
  "V13S", "V11S", "V17S",
  "FL_Z39S", "FL_Z40S", "FL_Z33S", "FL_V11S", "FL_V13S", "FL_V17S"
)]

#exclude schools with FTE less than 1
coloSLFSreduced <- subset (coloradoSLFSreduced, FTE > 0)

#run basic summary statistics of newly reduced dataset
summary(coloSLFSreduced)
names(coloSLFSreduced)

#create instructional personnel cost variable
coloSLFSreduced$instructional_personnel <-
  coloSLFSreduced$Z39S +
  coloSLFSreduced$Z40S +
  coloSLFSreduced$Z33S +
  coloSLFSreduced$V13S

#create student support personnel cost variable
coloSLFSreduced$student_support_personnel <- coloSLFSreduced$V11S

#create administrative personnel cost variable
coloSLFSreduced$admin_personnel <- coloSLFSreduced$V17S

# total core personnel spending (admin, instructional, and support)
coloSLFSreduced$core_personnel <-
  coloSLFSreduced$instructional_personnel +
  coloSLFSreduced$student_support_personnel +
  coloSLFSreduced$admin_personnel

#create proportion variables (RQ3)
coloSLFSreduced$instructional_prop <-
  coloSLFSreduced$instructional_personnel / coloSLFSreduced$core_personnel
coloSLFSreduced$student_support_prop <-
  coloSLFSreduced$student_support_personnel / coloSLFSreduced$core_personnel
coloSLFSreduced$admin_prop <-
  coloSLFSreduced$admin_personnel / coloSLFSreduced$core_personnel

#avoid divide by zero for proportions
coloSLFSreduced <- subset(coloSLFSreduced, core_personnel > 0)

#check proportions sum = 1
summary(
  coloSLFSreduced$instructional_prop +
    coloSLFSreduced$admin_prop +
    coloSLFSreduced$student_support_prop
)
  
#create derived variables for RQ1 (per-puil personnel costs)
coloSLFSreduced$instruction_per_pupil <-coloSLFSreduced$instructional_personnel / coloSLFSreduced$MEMBER
coloSLFSreduced$student_support_per_pupil <- coloSLFSreduced$student_support_personnel / coloSLFSreduced$MEMBER
coloSLFSreduced$admin_per_pupil <- coloSLFSreduced$admin_personnel / coloSLFSreduced$MEMBER

#create derived variable for RQ2 (average teacher pay)
coloSLFSreduced$pay_per_fte <- coloSLFSreduced$Z39S / coloSLFSreduced$FTE

#avoid divide by zero for member
coloSLFSreduced <- subset(coloSLFSreduced, MEMBER > 0)

# add LOCALE Labels
coloSLFSreduced$LOCALE <- factor(coloSLFSreduced$LOCALE,
                                 levels = c("11","12","13", "21", "22", "23", "31", "32", "33", "41", "42", "43"),
                                 labels = c("City - Large", "City - Midsize", "City - Small",
                                            "Suburb - Large", "Suburb - Midsize", "Suburb - Small",
                                            "Town - Fringe", "Town - Distant", "Town - Remote",
                                            "Rural - Fringe", "Rural - Distant", "Rural - Remote"))
summary(coloSLFSreduced)
View(coloSLFSreduced)

#Load packages for analysis and visualization
library("psych")
library("ggplot2")
library("dplyr")

# Frequency tables for Locale and Level
table(coloSLFSreduced$LOCALE)
table(coloSLFSreduced$LEVEL)

# select key variables
desc_vars <- coloSLFSreduced[, c(
  "instruction_per_pupil",
  "admin_per_pupil",
  "student_support_per_pupil",
  "pay_per_fte",
  "instructional_prop",
  "admin_prop",
  "student_support_prop"
)]

# descriptive statistics for key variables
describe(desc_vars)

# Means and standard deviations by LOCALE
coloSLFSreduced %>%
  group_by(LOCALE) %>%
  summarise( 
    mean_instruction = mean(instruction_per_pupil, na.rm = TRUE),
    sd_instruction = sd(instruction_per_pupil, na.rm = TRUE),
             
    mean_admin = mean(admin_per_pupil, na.rm = TRUE),
    sd_admin = sd(admin_per_pupil, na.rm = TRUE),
    
    mean_support = mean(student_support_per_pupil, na.rm = TRUE),
    sd_support = sd(student_support_per_pupil, na.rm = TRUE)
)

# descriptive statistics for proportions
 coloSLFSreduced %>%
   group_by(LOCALE)%>%
   summarize(
     mean_instr_prop = mean(instructional_prop, na.rm = TRUE),
     sd_instr_prop = sd(instructional_prop, na.rm = TRUE),
     
     mean_admin_prop = mean(admin_prop, na.rm = TRUE),
     sd_admin_prop = sd(admin_prop, na.rm = TRUE),
     
     mean_student_support_prop = mean(student_support_prop, na.rm = TRUE),
     sd_student_support_prop = sd(student_support_prop, na.rm = TRUE)
     
   )
 
# Boxplots for personnel spending by locale (instructional, admin, student support)
 ggplot(coloSLFSreduced, aes(x = factor(LOCALE), y = instruction_per_pupil)) +
   geom_boxplot()+
   labs(x = "Locale", y = "Instruction Personnel Spending per Pupil")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 ggplot(coloSLFSreduced, aes(x = factor(LOCALE), y = admin_per_pupil)) +
   geom_boxplot()+
   labs(x = "Locale", y = "Administrative Personnel Spending Per Pupil")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 ggplot(coloSLFSreduced, aes(x = factor(LOCALE), y = student_support_per_pupil)) +
   geom_boxplot()+
   labs(x = "Locale", y = "Student Support Personnel Spending Per Pupil")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# outliers have been entered correctly and will be retained

 #Run ANOVA for RQ1: Is school locale associated with variation in per-pupil
#instructional, student support, and administrative personnel expenditures?

anova_instr <- aov(instruction_per_pupil ~ LOCALE, data = coloSLFSreduced)
summary(anova_instr)

anova_admin <- aov(admin_per_pupil ~ LOCALE, data = coloSLFSreduced)
summary(anova_admin)

anova_support <- aov(student_support_per_pupil ~ LOCALE, data = coloSLFSreduced)
summary(anova_support)

# All significant - run Tukey test to see which locales differ
TukeyHSD(anova_instr)
TukeyHSD(anova_admin)
TukeyHSD(anova_support)

#assign Tukey results to data frame and sort by p-value
tukey_instr <- as.data.frame(TukeyHSD(anova_instr)$LOCALE)
tukey_instr_sorted <- tukey_instr %>%
  arrange(`p adj`)
View(tukey_instr_sorted)

tukey_admin <- as.data.frame(TukeyHSD(anova_admin)$LOCALE)
tukey_admin_sorted <- tukey_admin %>%
  arrange(`p adj`)
View(tukey_admin_sorted)
  
tukey_support <- as.data.frame(TukeyHSD(anova_support)$LOCALE)
tukey_support_sorted <- tukey_support %>%
  arrange(`p adj`)
View(tukey_support_sorted)

# Research Question 2

# Is there an association between teacher salary and locale?
#anova teacher salary by locale
anova_pay_per_FTE <- aov(pay_per_fte ~ LOCALE, 
                         data = coloSLFSreduced)
summary(anova_pay_per_FTE)

#significant - run Tukey comparison of means
TukeyHSD(anova_pay_per_FTE)

# Assign Tukey results to data frame and sort by p-values
tukey_pay_per_FTE <- as.data.frame(TukeyHSD(anova_pay_per_FTE)$LOCALE)
tukey_pay_per_FTE_sorted <- tukey_pay_per_FTE %>%
  arrange(`p adj`)
View(tukey_pay_per_FTE_sorted)

# most significant differences indicate that Town-Remote locales pay less
#list schools in locale "Town - Remote"
locale33 <- subset(coloSLFSreduced, LOCALE == "Town - Remote")

#Research Question 3
# Is there an association between school locale and the proportion of resources
# allocated to instructional, administrative, or student support functions?

anova_instr_prop <- aov(instructional_prop ~ LOCALE, data = coloSLFSreduced)
summary(anova_instr_prop)

anova_admin_prop <- aov(admin_prop ~ LOCALE, data = coloSLFSreduced)
summary(anova_admin_prop)

anova_support_prop <- aov(student_support_prop ~ LOCALE, data = coloSLFSreduced)
summary(anova_support_prop)

# All relationships were significant - run Tukey
TukeyHSD(anova_instr_prop)
TukeyHSD(anova_admin_prop)
TukeyHSD(anova_support_prop)

# Assign Tukey results to new data frames and sort by p-values
tukey_instr_prop <- as.data.frame(TukeyHSD(anova_instr_prop)$LOCALE)
tukey_instr_prop_sorted <- tukey_instr_prop %>%
  arrange(`p adj`)
View(tukey_instr_prop_sorted)

tukey_admin_prop <- as.data.frame(TukeyHSD(anova_admin_prop)$LOCALE)
tukey_admin_prop_sorted <- tukey_admin_prop %>%
  arrange(`p adj`)
View(tukey_admin_prop_sorted)

tukey_support_prop <- as.data.frame(TukeyHSD(anova_support_prop)$LOCALE)
tukey_support_prop_sorted <- tukey_support_prop %>%
  arrange(`p adj`)
View(tukey_support_prop_sorted)

