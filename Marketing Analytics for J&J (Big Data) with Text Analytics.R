#______________________________________________________________________________________________________________#

#Cleaning R

##Clearing the environment and plots
rm(list = ls()) 
dev.off()  # But only if there IS a plot
cat("\014")  # ctrl+L

#______________________________________________________________________________________________________________#

#As there could be problems with the package and import of NRC lexicon for VAD.
#Kindly run this first before running anything.

if (!require("textdata")) install.packages("textdata")
library(textdata) #for valence and arousal
###Load NRC VAD lexicon
jnjdata_vad = textdata::lexicon_nrc_vad()
###Initially run until here and if successful, run now from line 39

#_____________#

##Only run when there is an error in downloading the lexicon
##If there is an error when the VAD lexicon does not exist, kindly run:
textdata::lexicon_nrc_vad(delete=TRUE)
textdata::lexicon_nrc_vad()
###Load NRC VAD lexicon
jnjdata_vad = textdata::lexicon_nrc_vad()

#______________________________________________________________________________________________________________#

#Initialising the packages and the files

##Utility Packages
library(tidyverse)
library(gridExtra)
library(broom)
require(pacman)
library(caTools)
library(ggplot2)
library(reshape2)
library(psych)
library(ipred)
library(car)
library(lubridate)
library(hms)
library(cowplot)
library(huxtable)
if (!require("fastDummies")) install.packages("fastDummies")
library(fastDummies)

##Variable Analysis Packages
if (!require("caret")) install.packages('caret', dependencies = TRUE)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, gmodels,caret,ROCR)
pacman::p_load(pacman, tidyverse, gmodels,ROCR, rpart, rpart.plot,caret)
if (!require("modelr")) install.packages("modelr")
if (!require("gt")) install.packages("gt")
library(gt)
library(modelr)
library(corrplot)
if (!require("nortest")) install.packages("nortest")
if (!require("regclass")) install.packages("regclass")
library(nortest)
library(regclass)

##Text Analysis Packages
library(quanteda)
library(readtext)
library(tm)
library(remotes)
library(quanteda.dictionaries)
if (!require("syuzhet")) install.packages("syuzhet")
library(syuzhet) #sentiment analysis
if (!require("wordcloud")) install.packages("wordcloud")
library(wordcloud) 
if (!require("SnowballC")) install.packages("SnowballC")
library(SnowballC) #text stemming 

##Validation and Predictions
library(gvlma)
library(lmtest)   
library(sandwich)

jnjdata = read.csv("jnj-data.csv", header=T, na.strings = c("", "NA"), stringsAsFactors = FALSE)

#______________________________________________________________________________________________________________#

#Data Cleaning and Processing

##Finding out Null Values
sum(is.na(jnjdata))
sum(jnjdata$Video.Length == "N/A")
sum(jnjdata == "N/A")
sum(jnjdata == " ")
sum(jnjdata == "")
###No null values found within the dataset except inside Video Length

view(jnjdata)
str(jnjdata)

##Analysing if there are unique values in certain columns that are expected to have the same values, seeking typos, etc.
jnjdata %>% select("Page.Name","Page.Category","Page.Admin.Top.Country","Page.Description","Page.Created") %>% unique()
###No Typos in the columns

#_____________#

##Changing the NA values from Video Length
jnjdata$Video.Length = str_replace(jnjdata$Video.Length,"N/A","00:00:00")
###Converting the null values into 0 as this means there is no video length or no video.
###We confirm that all values with 0 seconds are non-videos.
sum(jnjdata$Video.Length == "00:00:00")

#_____________#

##Converting to necessary formats and operationalising the variables
jnj.factors = c("Type")
jnj.numerical = c("Likes.at.Posting","Followers.at.Posting","Likes","Comments","Shares",
                  "Love","Wow","Haha","Sad","Angry","Care","Total.Views")
jnj.dates = c("Page.Created","Post.Created")
jnj.dateonly = c("Post.Created.Date")
jnj.datetime = c("Post.Created.Time")

jnjdata[jnj.factors] = lapply(jnjdata[jnj.factors],as.factor)
jnjdata[jnj.numerical] = lapply(jnjdata[jnj.numerical],as.numeric)
jnjdata[jnj.dates] = lapply(jnjdata[jnj.dates],ymd_hms)
jnjdata[jnj.dateonly] = lapply(jnjdata[jnj.dateonly],ymd)
jnjdata[jnj.datetime] = lapply(jnjdata[jnj.datetime],as_hms)
jnjdata$Video.Length = as.numeric(as_hms(jnjdata$Video.Length))
###All data have been converted to the necessary format

#_____________#

##Creating variables from existing ones
jnjdata_b = jnjdata
###Creating a copy of the data set for cleaner processing

#_____________#

##Renaming the variables of interests and some key metrics
names(jnjdata_b)[names(jnjdata_b) == "Likes.at.Posting"] = "brand_likesAtPosting"
names(jnjdata_b)[names(jnjdata_b) == "Followers.at.Posting"] = "brand_followsAtPosting"
names(jnjdata_b)[names(jnjdata_b) == "Likes"] = "eng_likes"
names(jnjdata_b)[names(jnjdata_b) == "Comments"] = "eng_comments"
names(jnjdata_b)[names(jnjdata_b) == "Shares"] = "eng_shares"

#_____________#

##Operationalising the true value of engagement dependent variables (Trefzger et al., 2016)
jnjdata_b = mutate(jnjdata_b, eng_likes_adj = (eng_likes / brand_likesAtPosting)* 1000000)
jnjdata_b = mutate(jnjdata_b, eng_comments_adj = (eng_comments / brand_likesAtPosting)* 1000000)
jnjdata_b = mutate(jnjdata_b, eng_shares_adj = (eng_shares / brand_likesAtPosting)* 1000000)

##Gained Likes and Followers from previous post time using lead (1) values as the dates are in descending order
jnjdata_b = mutate(jnjdata_b, brand_likes_gained = brand_likesAtPosting - lead(brand_likesAtPosting))
jnjdata_b = mutate(jnjdata_b, brand_follows_gained = brand_followsAtPosting - lead(brand_followsAtPosting))
###Since we used lagged values, we have NA values at the first row. We will be making this 0 for model purposes
jnjdata_b$brand_likes_gained[nrow(jnjdata_b)] = 0
jnjdata_b$brand_follows_gained[nrow(jnjdata_b)] = 0

#_____________#

##Operationalising time effects variables

##Extracting month, weekday, hour from post creation date
jnjdata_b = mutate(jnjdata_b, 
                   time_post_year = as.factor(year(Post.Created)),
                   time_post_month = as.factor(month(Post.Created)), 
                   time_post_weekday = as.factor(weekdays(Post.Created)),
                   time_post_hour = as.numeric(hour(Post.Created.Time)),
                   time_post_time_of_day = as.numeric(seconds(Post.Created.Time))
)

##Further extracting more details for time effects

###Creating a factor variable to consider the impact of the pandemic (Wang & Deng, 2022)
jnjdata_b = mutate(jnjdata_b, time_post_period = as.factor(ifelse(Post.Created < "2020-03-11","Pre-Pandemic",
                                                                  ifelse(Post.Created < "2021-03-01","Pandemic","Post-Pandemic"))))
###Creating the peak and low hours based on the time posted (Pletikosa Cvijikj & Michahelles, 2013)
jnjdata_b = mutate(jnjdata_b, time_post_hour_peak = as.factor(ifelse(time_post_hour >= 4 & time_post_hour <= 15,1,0)))

#####Creating the dummy variables for the pandmic periods
jnjdata_b = mutate(jnjdata_b,
                   time_post_prepandemic = ifelse(time_post_period == "Pre-Pandemic",1,0),
                   time_post_pandemic = ifelse(time_post_period == "Pandemic",1,0),
                   time_post_postpandemic = ifelse(time_post_period == "Post-Pandemic",1,0))

#####Creating the dummy variables for the weekdays
jnjdata_b = mutate(jnjdata_b,
                   time_post_week_mon = ifelse(time_post_weekday == "Monday",1,0),
                   time_post_week_tues = ifelse(time_post_weekday == "Tuesday",1,0),
                   time_post_week_wed = ifelse(time_post_weekday == "Wednesday",1,0),
                   time_post_week_thu = ifelse(time_post_weekday == "Thursday",1,0),
                   time_post_week_fri = ifelse(time_post_weekday == "Friday",1,0),
                   time_post_week_sat = ifelse(time_post_weekday == "Saturday",1,0),
                   time_post_week_sun = ifelse(time_post_weekday == "Sunday",1,0)
)

#####Creating the weekend/weekday factor
jnjdata_b = mutate(jnjdata_b, time_post_weekend = ifelse(time_post_weekday == "Saturday" | time_post_weekday == "Sunday",1,0))

#####Creating the dummy variables for the months
jnjdata_b = mutate(jnjdata_b,
                   time_post_month_jan = ifelse(time_post_month == 1,1,0),
                   time_post_month_feb = ifelse(time_post_month == 2,1,0),
                   time_post_month_mar = ifelse(time_post_month == 3,1,0),
                   time_post_month_apr = ifelse(time_post_month == 4,1,0),
                   time_post_month_may = ifelse(time_post_month == 5,1,0),
                   time_post_month_jun = ifelse(time_post_month == 6,1,0),
                   time_post_month_july = ifelse(time_post_month == 7,1,0),
                   time_post_month_aug = ifelse(time_post_month == 8,1,0),
                   time_post_month_sep = ifelse(time_post_month == 9,1,0),
                   time_post_month_oct = ifelse(time_post_month == 10,1,0),
                   time_post_month_nov = ifelse(time_post_month == 11,1,0),
                   time_post_month_dec = ifelse(time_post_month == 12,1,0))

#_____________#

##Operationalising reaction variables

##Creating percentage of different reactions
jnjdata_b = mutate(jnjdata_b, reaction_total = Love+Wow+Haha+Sad+Angry+Care)

###Categorising the reactions based on positive and negative (Larsson, 2018)
jnjdata_b = mutate(jnjdata_b, 
                   reaction_pos = Love + Care + Haha + Wow,
                   reaction_neg = Sad + Angry)
jnjdata_b = mutate(jnjdata_b, 
                   reaction_pos_per = reaction_pos/reaction_total,
                   reaction_neg_per = reaction_neg/reaction_total)

#_____________#

##Operationalising post types and content

###Making factors into dummy variables
summary(jnjdata_b$Type)
#####The distribution of content type seems skewed on some levels, so we will be dividing this into: video, photo, and others. 
jnjdata_b = mutate(jnjdata_b,
                   type_photo = ifelse(Type == "Photo",1,0),
                   type_others = ifelse(Type == "Link" | Type == "Status" | Type == "Youtube",1,0),
                   type_video = ifelse(Type == "Live Video Complete" | Type == "Live Video Scheduled" | Type == "Native Video",1,0))

###Renaming video length and total views
colnames(jnjdata_b)[colnames(jnjdata_b) == 'Video.Length'] = 'type_video_length'
colnames(jnjdata_b)[colnames(jnjdata_b) == 'Total.Views'] = 'type_video_views'

##Rechecking  Null Values
sum(is.na(jnjdata_b))
colSums(is.na(jnjdata_b)) # for columns
###No null values on each column

#______________________________________________________________________________________________________________#

#Text Analysis and Creating Relevant Columns

##Setting the corpus
jnjdata_corpus = corpus(jnjdata_b$Message)
jnj_tokeninfo = summary(jnjdata_corpus)

##Pre-processing to remove unnecessary characters and words
###Extracting the words
jnjdata_tokens = tokens(jnjdata_corpus,what="word") 
###Identifying  compounds
jnjdata_tokens = jnjdata_tokens |> tokens_compound(pattern = phrase(c("can't wait", "do it")))

###Removing symbols, numbers, etc.
jnjdata_tokens = jnjdata_tokens |> 
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE,
         split_hyphens = FALSE,
         split_tags = FALSE,
         padding = FALSE) 
###Making words into lower case for smoother process
jnjdata_tokens = jnjdata_tokens |> tokens_tolower()
###Removing stopwords or words without much meaningful attribute for the analysis
jnjdata_tokens = jnjdata_tokens |> tokens_remove(stopwords("en"))
###Before removing hash tags, we hope to collect them for a separate analysis as social media sites run accessibility with hastags
jnjdata_tokenhash = jnjdata_tokens |> tokens_select(pattern = c("#\\w+['’]\\w*", "#\\w+"), valuetype = "regex")
###Removing contractions, possessive apostrophes, hashtags, mentions, etc.
jnjdata_tokens = jnjdata_tokens |> tokens_remove(pattern = c("\\b\\w+['’]\\w*\\b", "#\\w+['’]\\w*", "#\\w+", "@\\w+", "^.$"), valuetype = "regex")
emoji_pattern = "[\\p{So}\\p{Sk}\\p{Sc}\\p{Sm}]"
jnjdata_tokens = jnjdata_tokens |> tokens_select(pattern = emoji_pattern, selection = "remove", valuetype = "regex")

##Viewing  the top words or features
###Converting the texts as well as the hastags
jnjdata_dfm = dfm(jnjdata_tokens)
jnjdata_hashdfm = dfm(jnjdata_tokenhash)
###Viewing the top 10 words in the content
topfeatures(jnjdata_dfm, 10)
###Viewing the top 10 hashtags
topfeatures(jnjdata_hashdfm, 10)

##Stemming the words to its base form
jnjdata_stemdfm = tokens_wordstem(jnjdata_tokens, language = quanteda_options("language_stemmer")) |> dfm()
###We will not be stemming the hastags as social media treats them differently and R only see hashtags as one

##Getting the word count per row
jnj_wordcount = data.frame(ntoken(jnjdata_stemdfm)) %>% rename("text_wordcount"="ntoken.jnjdata_stemdfm.")

#_____________#

##Analysing the locomotion and promotion words

###Setting up the dictionaries
dict_RM_unstemmed = dictionary(list(locomotion_unstemmed = c("Can't wait", "Do it", "Done",
                                                             "Dynami*", "Go", "Going", "Hurr*",
                                                             "Momentum", "Motion", "Mov*",
                                                             "Obsticle*", "Proceed*", "Urg*"),
                                    assessment_unstemmed = c("Accura*", "Calculat*", "Careful*",
                                                             "Consider*", "Criti*", "Exhaustive",
                                                             "Methodical", "Meticulous*",
                                                             "Procrastinat*", "Reconsider*", "Right",
                                                             "Ruminat*", "Unsure")))
dict_RM_stemmed = dictionary(list(locomotion_stemmed = c("Act", "Chang", "Dare",
                                                         "Doer", "Drive", "Elimin",
                                                         "Fast", "Flow", "Get",
                                                         "Initiat", "Launch", "Lead",
                                                         "Make", "Mobil", "Quick",
                                                         "Reduc", "Reject", "Remov",
                                                         "Smooth", "Speed", "Start"),
                                  assessment_stemmed = c("Alternat", "Assess","Compar",
                                                         "Consult","Correct", "Detail",
                                                         "Evaluat", "Examin", "Judg",
                                                         "Observ", "Perfect", "Ponder", 
                                                         "Question","Reflect", "Regret",
                                                         "Review", "Think", "Thorough",
                                                         "Thought", "True", "Truth")))
dict_RF = dictionary(list(promotion = c("Accomplish","Achievement",
                                        "Advancement",
                                        "Aspiration", "Aspire", "Attain", "Desire", 
                                        "Earn", "Expand", "Gain", 
                                        "Grow", "Hope", "Hoping","Ideal", 
                                        "Improve", "Increase", "Momentum", 
                                        "Obtain", "Optimistic",
                                        "Progress","Promoting",
                                        "Promotion", "Speed", "Swift", "Toward",
                                        "Velocity", "Wish"),
                          prevention = c("Accuracy", "Afraid", "Careful", 
                                         "Anxious", "Avoid", "Conservative", 
                                         "Defend", "Duty", "Escape", "Escaping",
                                         "Evade", "Fail", "Fear", "Loss","Obligation",
                                         "Ought", "Pain", "Prevent", "Protect",
                                         "Responsible", "Risk", "Safety", "Security",
                                         "Threat", "Vigilance")))

###Extracting the data using the dicitonaries for promotion, prevention, locomotion, assessment
#### For RM (unstemmed) words
jnjdata_stemdfm_rm_un = dfm_lookup(jnjdata_stemdfm, dictionary = dict_RM_unstemmed)
#### For RM (stemmed) words
jnjdata_stemdfm_rm_sm = dfm_lookup(jnjdata_stemdfm, dictionary = dict_RM_stemmed)
#### For RF words
jnjdata_stemdfm_rf = dfm_lookup(jnjdata_stemdfm, dictionary = dict_RF)

###Binding the data into the main dataset
jnjdata_stemdfm_rm_un_df = convert(jnjdata_stemdfm_rm_un, to = "data.frame")
jnjdata_stemdfm_rm_sm_df = convert(jnjdata_stemdfm_rm_sm, to = "data.frame")
jnjdata_stemdfm_rf_df = convert(jnjdata_stemdfm_rf, to = "data.frame")

jnj_rmrf = cbind(jnjdata_stemdfm_rm_un_df, jnjdata_stemdfm_rm_sm_df, jnjdata_stemdfm_rf_df, jnj_wordcount)
jnj_rmrf = subset(jnj_rmrf, select = c(locomotion_unstemmed, assessment_unstemmed,
                                       locomotion_stemmed, assessment_stemmed,
                                       promotion, prevention,
                                       text_wordcount))

###Computing the percentage of locomotion, assessment, promotion, prevention
jnj_rmrf$locomotion_percent = (jnj_rmrf$locomotion_unstemmed + jnj_rmrf$locomotion_stemmed) / jnj_rmrf$text_wordcount
jnj_rmrf$assessment_percent = (jnj_rmrf$assessment_unstemmed + jnj_rmrf$assessment_stemmed) / jnj_rmrf$text_wordcount
jnj_rmrf$promotion_percent = (jnj_rmrf$promotion / jnj_rmrf$text_wordcount)
jnj_rmrf$prevention_percent = (jnj_rmrf$prevention / jnj_rmrf$text_wordcount)

###Copying the data into the main dataset
jnjdata_b[, c("text_RMT_loc_per", "text_RMT_ass_per","text_RMT_prom_per","text_RMT_prev_per","text_wordcount")] = jnj_rmrf[, c("locomotion_percent", "assessment_percent",
                                                                                                                               "promotion_percent","prevention_percent","text_wordcount")]

###Creating locomotion predominance
jnjdata_b = mutate(jnjdata_b, 
                   text_RMT_loc_predominance = text_RMT_loc_per-text_RMT_ass_per)

#_____________#

##Analysing emotion across the content using the NRC Word-Emotion Association Lexicon

jnj_rowedtokens = sapply(jnjdata_tokens, function(x) paste(unlist(x), collapse = " "))
###Utilising the cleaned tokens, we will be making them for each rows using the paste function
jnj_sentiment = get_nrc_sentiment(jnj_rowedtokens)
###We will apply the NRC Sentiment with this formula
jnj_sentiment$text_wordcount = as.numeric(jnj_rmrf$text_wordcount)

###Computing the percentage of emotions
jnj_sentiment$anger_per = (jnj_sentiment$anger / jnj_sentiment$text_wordcount)
jnj_sentiment$anticipate_per = (jnj_sentiment$anticipation / jnj_sentiment$text_wordcount)
jnj_sentiment$disgust_per = (jnj_sentiment$disgust / jnj_sentiment$text_wordcount)
jnj_sentiment$fear_per = (jnj_sentiment$fear / jnj_sentiment$text_wordcount)
jnj_sentiment$joy_per = (jnj_sentiment$joy / jnj_sentiment$text_wordcount)
jnj_sentiment$sad_per = (jnj_sentiment$sad / jnj_sentiment$text_wordcount)
jnj_sentiment$surprise_per = (jnj_sentiment$surprise / jnj_sentiment$text_wordcount)
jnj_sentiment$trust_per = (jnj_sentiment$trust / jnj_sentiment$text_wordcount)
jnj_sentiment$negative_per = (jnj_sentiment$negative / jnj_sentiment$text_wordcount)
jnj_sentiment$positive_per = (jnj_sentiment$positive / jnj_sentiment$text_wordcount)

###Copying the data into the main dataset
jnjdata_b[, c("text_NRC_anger_per", "text_NRC_anticipate_per","text_NRC_disgust_per","text_NRC_fear_per","text_NRC_joy_per","text_NRC_sad_per",
              "text_NRC_surprise_per","text_NRC_trust_per","text_NRC_neg_per","text_NRC_pos_per")] = jnj_sentiment[,12:21]

#_____________#

##Analysing the sentiment scores through Syuzhet Vignette  under the default setting Syuzhet

jnj_syuzhet = get_sentiment(jnj_rowedtokens, method = "syuzhet") %>% as.data.frame()
colnames(jnj_syuzhet)[1] = "text_syuzhet_value"

###The cleaned rowed tokens will still be used
###Copying the data into the main dataset
jnjdata_b[, c("text_syuzhet_value")] = jnj_syuzhet[, c("text_syuzhet_value")]

###Analysing Syuzhet 
summary(jnjdata_b$text_syuzhet_value)

####To further improve this analysis, we hope to categorise the results across two categories: negative and positive.
####What this means is that negative ones equal to content with more negative words than positives, rather than focus on how many negative words there are (NRC)
####We include 0 with positive.

jnjdata_b = mutate(jnjdata_b, 
                   text_syuzhet_neg = ifelse(text_syuzhet_value < 0,1,0),
                   text_syuzhet_pos = ifelse(text_syuzhet_value >= 0,1,0))

#_____________#

##Analysing the valence and arousal scores through The NRC Valence, Arousal, and Dominance Lexicon (Mohammad, Saif M.)

###Creating a data frame for the words
jnjdata_tokens_df = convert(jnjdata_stemdfm, to = "data.frame")

###Pivoting the dataframe in preparation to combine with the VAD
jnjdata_tokens_df_long = jnjdata_tokens_df %>%
  pivot_longer(cols = -doc_id, names_to = "Word", values_to = "Count")

###Combining the VAD lexicon with the dataframe to apply the score
jnjdata_arousal_combine = jnjdata_tokens_df_long %>%
  left_join(jnjdata_vad, by = "Word")

###Getting the mean VAD score as there are multiple instances for each sentence
jnjdata_arousal_combine = mutate(jnjdata_arousal_combine,
                                 Valence_counted = Valence*Count,
                                 Arousal_counted = Arousal*Count,
                                 Dominance_counted = Dominance*Count)

###Finalising the dataframe with grouping by the text in preparation to export it to the main data
jnjdata_arousal_final = jnjdata_arousal_combine %>% na.omit() %>% group_by(doc_id) %>% 
  summarise(Valence_mean = sum(Valence_counted)/sum(Count),
            Arousal_mean = sum(Arousal_counted)/sum(Count),
            Dominance_mean = sum(Dominance_counted)/sum(Count),
            .groups = 'drop') %>% as.data.frame()
####The final VAD dataframe seemed to be organised differently
####Reorganising the datframe
jnjdata_arousal_final$doc_id = as.numeric(gsub("text",'',jnjdata_arousal_final$doc_id))
jnjdata_arousal_final = jnjdata_arousal_final[order(jnjdata_arousal_final$doc_id),]
####Resetting index
row.names(jnjdata_arousal_final) = NULL

###Copying the data into the main dataset
####We only want valence and arousal as these were the ones found impacting engagement based on our related literatures.
jnjdata_b[, c("text_VAD_valence","text_VAD_arousal")] = jnjdata_arousal_final[, c("Valence_mean","Arousal_mean")]
str(jnjdata_b)
#_____________#

##Subsetting the columns
###As there are a lot of columns, we will be sub-setting the dataset to only include the columns needed.
colnames(jnjdata_b)
jnjdata_c = subset(jnjdata_b, select = c("eng_likes","eng_shares","eng_comments","eng_likes_adj","eng_shares_adj","eng_comments_adj",
                                         "brand_likes_gained","brand_follows_gained",
                                         "time_post_year","time_post_month","time_post_weekday","time_post_hour","time_post_time_of_day","time_post_period","time_post_weekend",
                                         "reaction_pos_per","reaction_neg_per",
                                         "text_RMT_loc_per","text_RMT_ass_per","text_RMT_prom_per","text_RMT_prev_per","text_RMT_loc_predominance","text_wordcount",
                                         "text_NRC_anger_per","text_NRC_anticipate_per","text_NRC_disgust_per","text_NRC_fear_per",
                                         "text_NRC_joy_per","text_NRC_sad_per","text_NRC_surprise_per",
                                         "text_NRC_trust_per","text_NRC_neg_per","text_NRC_pos_per",
                                         "text_syuzhet_value","text_syuzhet_neg", "text_syuzhet_pos", "text_VAD_valence","text_VAD_arousal",
                                         "time_post_hour_peak","time_post_prepandemic","time_post_pandemic","time_post_postpandemic","time_post_week_mon","time_post_week_tues",
                                         "time_post_week_wed", "time_post_week_thu","time_post_week_fri","time_post_week_sat","time_post_week_sun","time_post_month_jan","time_post_month_feb",
                                         "time_post_month_mar","time_post_month_apr","time_post_month_may","time_post_month_jun","time_post_month_july","time_post_month_aug","time_post_month_sep",
                                         "time_post_month_oct","time_post_month_nov","time_post_month_dec",
                                         "type_photo","type_others","type_video","type_video_length","type_video_views"))

##Rechecking  Null Values for the new set
sum(is.na(jnjdata_c))
colSums(is.na(jnjdata_c)) # for columns
###No null values on each column

#______________________________________________________________________________________________________________#

#Preliminary exploration of the dependent variables

## Checking descriptive statistics in case there are unnatural values or out of range
jnj_numvars = as.list(colnames(dplyr::select_if(jnjdata_c, is.numeric)))
jnjdata_c %>% dplyr::select_if(is.numeric) %>% describe()
###Here, we note the high dispersed variables: brand_follows_gained and brand_likes_gained, as well as the dependent variables.
###We will consider data transformations.

##Checking normality across the numeric variables through Anderson-Darling test
jnjdata_AD_results = sapply(jnjdata_c, function(x) {
  if (is.numeric(x)) {
    ad_test = ad.test(x)
    return(ad_test$p.value)
  } else {
    return(NA) # For non-numeric columns, return NA
  }
})

jnjdata_AD_results
###It seems that most of our variables are non-normal; we will apply the necessary transformations.
###The only one with a normal distribution is the VAD arousal values.

## Fixing non-normality of the dependent variables

jnj_depplot = list()
jnj_depvars = c("eng_likes","eng_shares","eng_comments","eng_likes_adj","eng_shares_adj","eng_comments_adj")

par(mfrow = c(3,3))
for (var_name in jnj_depvars) {
  plot = ggplot(data = jnjdata_c, aes(x = .data[[var_name]])) +
    geom_histogram()
  jnj_depplot[[var_name]] = plot
}

jnj_crossdepplot = gridExtra::grid.arrange(grobs = jnj_depplot, ncol = 3)

###Though the distribution seem the same across adjusted and non-adjusted values, we will be utilising the adjusted values.

###Furter basing on the visual results for normality, we need to transform the data.
###We then create logarithmic values for the data.
###The 0.1 is added as there are zero values for Comments and Shares. We use .5 as using 1 might affect the distribution of comments with near 1 values.

jnjdata_c = mutate(jnjdata_c,eng_likes_log = log(eng_likes_adj))
jnjdata_c = mutate(jnjdata_c,eng_comments_log = log(eng_comments_adj+1))
jnjdata_c = mutate(jnjdata_c,eng_shares_log = log(eng_shares_adj+1))

jnj_depplot = list()
jnj_depvars = c("eng_likes_log","eng_comments_log","eng_shares_log")

par(mfrow = c(3,3))
for (var_name in jnj_depvars) {
  plot = ggplot(data = jnjdata_c, aes(x = .data[[var_name]])) +
    geom_histogram()
  jnj_depplot[[var_name]] = plot
}

jnj_crossdepplot = gridExtra::grid.arrange(grobs = jnj_depplot, ncol = 2)

###Re-evaluating the three new variables based on descriptive statistics, comparing their old ones.

jnjdata_c %>% dplyr::select("eng_likes_adj","eng_likes_log","eng_comments_adj","eng_comments_log","eng_shares_adj","eng_shares_log") %>% describe()

###Using the log transformation allowed our dependent variables to have acceptable and better distribution, kurtosis and skewness values.
##To further validate this, we use the Anderson-Darling test and compare to the previous non-transformed.

###For Like
ad.test(jnjdata_c$eng_likes_adj)
ad.test(jnjdata_c$eng_likes_log)
###For Comment
ad.test(jnjdata_c$eng_comments_adj)
ad.test(jnjdata_c$eng_comments_log)
###For Shares
ad.test(jnjdata_c$eng_shares_adj)
ad.test(jnjdata_c$eng_shares_log)

####Though the dependent variables are still significant in the test, the critical value got lower but a huge amount. 
####Visually, log transformations provide better distribution than the non-transformed.

###We will use the log values in the models.

#______________________________________________________________________________________________________________#

#Preliminary exploration of the predictors

##Creating a list of non-dummy numerical variables to check normality
jnj_numvars_nondum = c("reaction_pos_per","reaction_neg_per","reaction_pos_per","reaction_neg_per","text_RMT_loc_per",
                       "text_RMT_ass_per","text_RMT_prom_per","text_RMT_prev_per","text_wordcount","text_NRC_anger_per",
                       "text_NRC_anticipate_per","text_NRC_disgust_per","text_NRC_fear_per","text_NRC_joy_per","text_NRC_sad_per",
                       "text_NRC_surprise_per","text_NRC_trust_per","text_NRC_neg_per","text_NRC_pos_per","text_syuzhet_value",
                       "text_VAD_valence","text_VAD_arousal","type_video_length","type_video_views","text_RMT_loc_predominance","time_post_time_of_day"
                       )

##Checking the histograms of non-dummy numerical variables
jnj_predplot = list()

for (var_name in jnj_numvars_nondum) {
  plot = ggplot(data = jnjdata_c, aes(x = .data[[var_name]])) +
    geom_histogram()
  jnj_predplot[[var_name]] = plot
}

###Showing the plots
jnj_crosspredplot = marrangeGrob(grobs=jnj_predplot, nrow=4, ncol=4)
jnj_crosspredplot

##Since the non-dummy numerical variables have non-normal distribution, except for VAD arousal values, we will transform them correspondingly.
###As we have a lot of percentage-based variables, we'll use logit transformation for them and log for non-percentage continuous.

###Creating a small value to replace 0 and 1
epsilon = 0.000001

###For Positive Reactions
#### Adjust 0s and 1s
jnjdata_c$reaction_pos_per_adj = ifelse(jnjdata_c$reaction_pos_per == 0, epsilon,
                                        ifelse(jnjdata_c$reaction_pos_per == 1, 1 - epsilon, jnjdata_c$reaction_pos_per))
####Apply logit transformation
jnjdata_c$reaction_pos_per_logit = log(jnjdata_c$reaction_pos_per_adj / (1 - jnjdata_c$reaction_pos_per_adj))

####Checking the histogram
par(mfrow = c(2,1))
hist(jnjdata_c$reaction_pos_per)
hist(jnjdata_c$reaction_pos_per_logit)
ad.test(jnjdata_c$reaction_pos_per)
ad.test(jnjdata_c$reaction_pos_per_logit)
####Though significant, the AD critical value is much lower.

###We now then apply this to other independent variables.

###For Negative Reactions
jnjdata_c$reaction_neg_per_adj = ifelse(jnjdata_c$reaction_neg_per == 0, epsilon,
                                        ifelse(jnjdata_c$reaction_neg_per == 1, 1 - epsilon, jnjdata_c$reaction_neg_per))
jnjdata_c$reaction_neg_per_logit = log(jnjdata_c$reaction_neg_per_adj / (1 - jnjdata_c$reaction_neg_per_adj))

### For RMT Locomotion Percentage
jnjdata_c$text_RMT_loc_per_adj = ifelse(jnjdata_c$text_RMT_loc_per == 0, epsilon,
                                        ifelse(jnjdata_c$text_RMT_loc_per == 1, 1 - epsilon, jnjdata_c$text_RMT_loc_per))
jnjdata_c$text_RMT_loc_per_logit = log(jnjdata_c$text_RMT_loc_per_adj / (1 - jnjdata_c$text_RMT_loc_per_adj))

###For RMT Assessment Percentage
jnjdata_c$text_RMT_ass_per_adj = ifelse(jnjdata_c$text_RMT_ass_per == 0, epsilon,
                                        ifelse(jnjdata_c$text_RMT_ass_per == 1, 1 - epsilon, jnjdata_c$text_RMT_ass_per))
jnjdata_c$text_RMT_ass_per_logit = log(jnjdata_c$text_RMT_ass_per_adj / (1 - jnjdata_c$text_RMT_ass_per_adj))

###For RMT Promotion Percentage
jnjdata_c$text_RMT_prom_per_adj = ifelse(jnjdata_c$text_RMT_prom_per == 0, epsilon,
                                         ifelse(jnjdata_c$text_RMT_prom_per == 1, 1 - epsilon, jnjdata_c$text_RMT_prom_per))
jnjdata_c$text_RMT_prom_per_logit = log(jnjdata_c$text_RMT_prom_per_adj / (1 - jnjdata_c$text_RMT_prom_per_adj))

###For RMT Prevention Percentage
jnjdata_c$text_RMT_prev_per_adj = ifelse(jnjdata_c$text_RMT_prev_per == 0, epsilon,
                                         ifelse(jnjdata_c$text_RMT_prev_per == 1, 1 - epsilon, jnjdata_c$text_RMT_prev_per))
jnjdata_c$text_RMT_prev_per_logit = log(jnjdata_c$text_RMT_prev_per_adj / (1 - jnjdata_c$text_RMT_prev_per_adj))

###For NRC Anger Percentage
jnjdata_c$text_NRC_anger_per_adj = ifelse(jnjdata_c$text_NRC_anger_per == 0, epsilon,
                                          ifelse(jnjdata_c$text_NRC_anger_per == 1, 1 - epsilon, jnjdata_c$text_NRC_anger_per))
jnjdata_c$text_NRC_anger_per_logit = log(jnjdata_c$text_NRC_anger_per_adj / (1 - jnjdata_c$text_NRC_anger_per_adj))

###For NRC Anticipation Percentage
jnjdata_c$text_NRC_anticipate_per_adj = ifelse(jnjdata_c$text_NRC_anticipate_per == 0, epsilon,
                                               ifelse(jnjdata_c$text_NRC_anticipate_per == 1, 1 - epsilon, jnjdata_c$text_NRC_anticipate_per))
jnjdata_c$text_NRC_anticipate_per_logit = log(jnjdata_c$text_NRC_anticipate_per_adj / (1 - jnjdata_c$text_NRC_anticipate_per_adj))

###For NRC Disgust Percentage
jnjdata_c$text_NRC_disgust_per_adj = ifelse(jnjdata_c$text_NRC_disgust_per == 0, epsilon,
                                            ifelse(jnjdata_c$text_NRC_disgust_per == 1, 1 - epsilon, jnjdata_c$text_NRC_disgust_per))
jnjdata_c$text_NRC_disgust_per_logit = log(jnjdata_c$text_NRC_disgust_per_adj / (1 - jnjdata_c$text_NRC_disgust_per_adj))

###For NRC Fear Percentage
jnjdata_c$text_NRC_fear_per_adj = ifelse(jnjdata_c$text_NRC_fear_per == 0, epsilon,
                                         ifelse(jnjdata_c$text_NRC_fear_per == 1, 1 - epsilon, jnjdata_c$text_NRC_fear_per))
jnjdata_c$text_NRC_fear_per_logit = log(jnjdata_c$text_NRC_fear_per_adj / (1 - jnjdata_c$text_NRC_fear_per_adj))

###For NRC Joy Percentage
jnjdata_c$text_NRC_joy_per_adj = ifelse(jnjdata_c$text_NRC_joy_per == 0, epsilon,
                                        ifelse(jnjdata_c$text_NRC_joy_per == 1, 1 - epsilon, jnjdata_c$text_NRC_joy_per))
jnjdata_c$text_NRC_joy_per_logit = log(jnjdata_c$text_NRC_joy_per_adj / (1 - jnjdata_c$text_NRC_joy_per_adj))

###For NRC Sadness Percentage
jnjdata_c$text_NRC_sad_per_adj = ifelse(jnjdata_c$text_NRC_sad_per == 0, epsilon,
                                        ifelse(jnjdata_c$text_NRC_sad_per == 1, 1 - epsilon, jnjdata_c$text_NRC_sad_per))
jnjdata_c$text_NRC_sad_per_logit = log(jnjdata_c$text_NRC_sad_per_adj / (1 - jnjdata_c$text_NRC_sad_per_adj))

### For NRC Surprise Percentage
jnjdata_c$text_NRC_surprise_per_adj = ifelse(jnjdata_c$text_NRC_surprise_per == 0, epsilon,
                                             ifelse(jnjdata_c$text_NRC_surprise_per == 1, 1 - epsilon, jnjdata_c$text_NRC_surprise_per))

jnjdata_c$text_NRC_surprise_per_logit = log(jnjdata_c$text_NRC_surprise_per_adj / (1 - jnjdata_c$text_NRC_surprise_per_adj))

### For NRC Trust Percentage
jnjdata_c$text_NRC_trust_per_adj = ifelse(jnjdata_c$text_NRC_trust_per == 0, epsilon,
                                          ifelse(jnjdata_c$text_NRC_trust_per == 1, 1 - epsilon, jnjdata_c$text_NRC_trust_per))

jnjdata_c$text_NRC_trust_per_logit = log(jnjdata_c$text_NRC_trust_per_adj / (1 - jnjdata_c$text_NRC_trust_per_adj))

### For NRC Negative Percentage
jnjdata_c$text_NRC_neg_per_adj = ifelse(jnjdata_c$text_NRC_neg_per == 0, epsilon,
                                        ifelse(jnjdata_c$text_NRC_neg_per == 1, 1 - epsilon, jnjdata_c$text_NRC_neg_per))

jnjdata_c$text_NRC_neg_per_logit = log(jnjdata_c$text_NRC_neg_per_adj / (1 - jnjdata_c$text_NRC_neg_per_adj))

### For NRC Positive Percentage
jnjdata_c$text_NRC_pos_per_adj = ifelse(jnjdata_c$text_NRC_pos_per == 0, epsilon,
                                        ifelse(jnjdata_c$text_NRC_pos_per == 1, 1 - epsilon, jnjdata_c$text_NRC_pos_per))

jnjdata_c$text_NRC_pos_per_logit = log(jnjdata_c$text_NRC_pos_per_adj / (1 - jnjdata_c$text_NRC_pos_per_adj))

### For NRC Positive Percentage
jnjdata_c$text_NRC_pos_per_adj = ifelse(jnjdata_c$text_NRC_pos_per == 0, epsilon,
                                        ifelse(jnjdata_c$text_NRC_pos_per == 1, 1 - epsilon, jnjdata_c$text_NRC_pos_per))

jnjdata_c$text_NRC_pos_per_logit = log(jnjdata_c$text_NRC_pos_per_adj / (1 - jnjdata_c$text_NRC_pos_per_adj))

### For Locomotion Predominance
###As Locomotion Predominance value has negative values, we will push the negative values into above 0
jnjdata_c = mutate(jnjdata_c,text_RMT_loc_predominance_nonneg = text_RMT_loc_predominance+abs(min(text_RMT_loc_predominance))+epsilon)

jnjdata_c$text_RMT_loc_predominance_adj = ifelse(jnjdata_c$text_RMT_loc_predominance_nonneg == 0, epsilon,
                                        ifelse(jnjdata_c$text_RMT_loc_predominance_nonneg == 1, 1 - epsilon, jnjdata_c$text_RMT_loc_predominance_nonneg))

jnjdata_c$text_RMT_loc_predominance_logit = log(jnjdata_c$text_RMT_loc_predominance_adj / (1 - jnjdata_c$text_RMT_loc_predominance_adj))

#_____________#

###As there are non-percentage variables, we will be applying the ordinary log transformation with minimum value above 0.
####These are: word count, syuzhet value, VAD valence, video length, video views and time of day (secs).

####Applying log for word count as fortunately its minimum is 7
jnjdata_c = mutate(jnjdata_c,text_wordcount_log = log(text_wordcount))
####This is also the same for VAD valence 
jnjdata_c = mutate(jnjdata_c,text_VAD_valence_log = log(text_VAD_valence))
####For video length
jnjdata_c = mutate(jnjdata_c,type_video_length_log = log(type_video_length+epsilon))
####For video views 
jnjdata_c = mutate(jnjdata_c,type_video_views_log = log(type_video_views+epsilon))
####For time of the day
jnjdata_c = mutate(jnjdata_c,time_post_time_of_day_log = log(time_post_time_of_day+epsilon))

###As syuzhet value has negative values, we will push the negative values into above 0
jnjdata_c = mutate(jnjdata_c,text_syuzhet_value_log = log(text_syuzhet_value+abs(min(text_syuzhet_value))+epsilon))
###By adding the absolute value of the minimum, we will make sure that all are at least 0, and by adding epsilon, everything is above 0

###As there are a lot of columns, we will be sub-setting the dataset to only include the columns needed.

jnjdata_d = subset(jnjdata_c, select = c("eng_likes_log","eng_comments_log","eng_shares_log",
                                         "time_post_year","time_post_month","time_post_weekday","time_post_hour","time_post_period","time_post_weekend",
                                         "reaction_pos_per_logit","reaction_neg_per_logit",
                                         "text_RMT_loc_per_logit","text_RMT_ass_per_logit","text_RMT_prom_per_logit","text_RMT_prev_per_logit","text_wordcount_log",
                                         "text_NRC_anger_per_logit","text_NRC_anticipate_per_logit","text_NRC_disgust_per_logit","text_NRC_fear_per_logit",
                                         "text_NRC_joy_per_logit","text_NRC_sad_per_logit","text_NRC_surprise_per_logit",
                                         "text_NRC_trust_per_logit","text_NRC_neg_per_logit","text_NRC_pos_per_logit",
                                         "text_syuzhet_value_log","text_syuzhet_neg", "text_syuzhet_pos", "text_VAD_valence_log","text_VAD_arousal",
                                         "time_post_hour_peak","time_post_prepandemic","time_post_pandemic","time_post_postpandemic","time_post_week_mon","time_post_week_tues",
                                         "time_post_week_wed", "time_post_week_thu","time_post_week_fri","time_post_week_sat","time_post_week_sun","time_post_month_jan","time_post_month_feb",
                                         "time_post_month_mar","time_post_month_apr","time_post_month_may","time_post_month_jun","time_post_month_july","time_post_month_aug","time_post_month_sep",
                                         "time_post_month_oct","time_post_month_nov","time_post_month_dec",
                                         "type_photo","type_others","type_video","type_video_length_log",
                                         "type_video_views_log","text_RMT_loc_predominance_logit","time_post_time_of_day_log"))

#_____________#

## Checking correlation across variables
###We will be using Spearman’s rank correlation as most variables are non-normal
corr_matrix = cor(dplyr::select_if(jnjdata_d, is.numeric),method="s")
corr_test = cor.mtest(dplyr::select_if(jnjdata_d, is.numeric), conf.level=0.95)
corr_title= "Correlation Matrix Across Variables"
par(mfrow = c(1,1))
corrplot(corr_matrix, method="color", number.cex=.35, tl.cex = .5, tl.srt=75, 
         tl.col="black", col=COL2('RdBu', 10), addCoef.col = "white", p.mat = corr_test$p, title=corr_title, pch.cex = .6)

###Drilling down to only the dependent variables:
corrplot(corr_matrix[,grep("eng_likes_log", colnames(corr_matrix)):grep("eng_shares_log", colnames(corr_matrix))], method="color", number.cex=.35, tl.cex = .5, tl.srt=75, 
         tl.col="black", col=COL2('RdBu', 10), addCoef.col = "white", p.mat = corr_test$p[,grep("eng_likes_log", colnames(corr_matrix)):grep("eng_shares_log", colnames(corr_matrix))], title=corr_title, pch.cex = 1)

###Caution on multicollinearity
####For the two Syuzhet values, this is expected as they are dummy variables with only two options. 

#_____________#

##Checking all numerical variables using histogram

par(mfrow = c(3,3))
for (col_name in names(jnjdata_d)) {
  # Check if the column is numeric
  if (is.numeric(jnjdata_d[[col_name]])) {
    # Create a histogram for the numeric column
    hist(jnjdata_d[[col_name]], 
         main = paste("Histogram of", col_name),
         xlab = col_name, 
         col = "skyblue", 
         border = "white")
  } else {
    message(paste(col_name, "is not numeric, skipping..."))
  }
}

###Similarly when we checked previously the non-dummy variables in histogram, we noticed the non-normality of values and skewed values.
###We can still see this non-normality across the histograms.

jnj_numvars_nondum = c("reaction_pos_per_logit","reaction_neg_per_logit","reaction_pos_per_logit","reaction_neg_per_logit","text_RMT_loc_per_logit",
                       "text_RMT_ass_per_logit","text_RMT_prom_per_logit","text_RMT_prev_per_logit","text_wordcount_log","text_NRC_anger_per_logit",
                       "text_NRC_anticipate_per_logit","text_NRC_disgust_per_logit","text_NRC_fear_per_logit","text_NRC_joy_per_logit","text_NRC_sad_per_logit",
                       "text_NRC_surprise_per_logit","text_NRC_trust_per_logit","text_NRC_neg_per_logit","text_NRC_pos_per_logit","text_syuzhet_value_log",
                       "text_VAD_valence_log","text_VAD_arousal","type_video_length_log",
                       "type_video_views_log","text_RMT_loc_predominance_logit","time_post_time_of_day_log")

## Checking non-dummy numerical variables using scatter plots
###We hope to check their relationship with our dependent variables visually.

### For Likes
jnj_numplot_like = list()
for (var_name in jnj_numvars_nondum) {
  plot = ggplot(data = jnjdata_d, aes(x = .data[[var_name]], y = eng_likes_log)) +
    geom_point() + geom_smooth(method=lm)
  jnj_numplot_like[[var_name]] = plot
}

jnj_crossnumplot_like = marrangeGrob(grobs=jnj_numplot_like, nrow=3, ncol=3)
jnj_crossnumplot_like

####For Likes, despite the data transformations, linear relationships appear not to be the relationship of like to the predictors.
####This seem to lead to the need for advanced models outside linear regression.

### For Comments
jnj_numplot_com = list()
for (var_name in jnj_numvars_nondum) {
  plot = ggplot(data = jnjdata_d, aes(x = .data[[var_name]], y = eng_comments_log)) +
    geom_point() + geom_smooth(method=lm)
  jnj_numplot_com[[var_name]] = plot
}

jnj_crossnumplot_com = marrangeGrob(grobs=jnj_numplot_com, nrow=3, ncol=3)
jnj_crossnumplot_com

####The same goes for Comment, as well as Shares below.

### For Shares
jnj_numplot_share = list()
for (var_name in jnj_numvars_nondum) {
  plot = ggplot(data = jnjdata_d, aes(x = .data[[var_name]], y = eng_shares_log)) +
    geom_point() + geom_smooth(method=lm)
  jnj_numplot_share[[var_name]] = plot
}

jnj_crossnumplot_share = marrangeGrob(grobs=jnj_numplot_share, nrow=3, ncol=3)
jnj_crossnumplot_share

#_____________#

jnj_facvars = c("time_post_year","time_post_month","time_post_weekday","time_post_weekend",
                "time_post_hour","time_post_period","type_photo","type_others","type_video")

## Checking distribution of factor variables using summarise by mean

for (var_name in jnj_facvars) {
  summ = jnjdata_d %>%
    group_by(.data[[var_name]]) %>% summarise_at(vars(eng_likes_log,eng_comments_log,eng_shares_log), list(mean = mean))
  print(summ)
}

## Checking factor variables using density plots
###We hope to check what factor variables would have different distributions for each category when graphed by the dependent variables. 
###The more different the distribution of categories are, the higher the chance they could be a good predictor.

### For Likes
jnj_facplot_like = list()
for (var_name in jnj_facvars) {
  plot = ggplot(data = jnjdata_d, aes(x = eng_likes_log, fill = as.factor(.data[[var_name]]))) +
    geom_density(alpha=0.5) + labs(title = var_name)
  jnj_facplot_like[[var_name]] = plot
}

jnj_crossfacplot_like = marrangeGrob(grobs=jnj_facplot_like, nrow=3, ncol=3)
jnj_crossfacplot_like

####Across the graphs, notable factors include month, hour, and content types, as well as the period (pandemic, etc) which will be explored later on.

### For Comments
jnj_facplot_comm = list()
for (var_name in jnj_facvars) {
  plot = ggplot(data = jnjdata_d, aes(x = eng_comments_log, fill = as.factor(.data[[var_name]]))) +
    geom_density(alpha=0.5) + labs(title = var_name)
  jnj_facplot_comm[[var_name]] = plot
}

jnj_crossfacplot_comm = marrangeGrob(grobs=jnj_facplot_comm, nrow=3, ncol=3)
jnj_crossfacplot_comm

####The same goes for the comments variable, with a notable distribution for the weekday.

### For Shares
jnj_facplot_share = list()
for (var_name in jnj_facvars) {
  plot = ggplot(data = jnjdata_d, aes(x = eng_shares_log, fill = as.factor(.data[[var_name]]))) +
    geom_density(alpha=0.5) + labs(title = var_name)
  jnj_facplot_share[[var_name]] = plot
}

jnj_crossfacplot_share = marrangeGrob(grobs=jnj_facplot_share, nrow=3, ncol=3)
jnj_crossfacplot_share

####Likewise, share has notable factors include month, hour, and content types. 
####All the three dependent variables have similar behavior of factor variables towards them.

#______________________________________________________________________________________________________________#

#Data Partitions for Modeling and Analysis

set.seed(130)

##For Like
jnj_trainpart_like = jnjdata_d$eng_likes_log %>% createDataPartition(p=0.70, list=FALSE)
jnj_train_like = jnjdata_d[jnj_trainpart_like,]
jnj_test_like = jnjdata_d[-jnj_trainpart_like,]

##For Comments
jnj_trainpart_comm = jnjdata_d$eng_comments_log %>% createDataPartition(p=0.70, list=FALSE)
jnj_train_comm = jnjdata_d[jnj_trainpart_comm,]
jnj_test_comm = jnjdata_d[-jnj_trainpart_comm,]

##For Shares
jnj_trainpart_share = jnjdata_d$eng_shares_log %>% createDataPartition(p=0.70, list=FALSE)
jnj_train_share = jnjdata_d[jnj_trainpart_share,]
jnj_test_share = jnjdata_d[-jnj_trainpart_share,]

###We attempted to remove outliers based on our residual plots; however, removing them made results inconsistent due to the data partition.
###As such, we have utilised other methods such as WLS and vcov to hope for our attempts to tackle the assumptions.

#______________________________________________________________________________________________________________#

#The Linear Regression M Linear Models 

###The models are based on our related studies and the logical models built, as well as incorporating correlation.
###We have tested numerous models incorporating the logical framework we built, below are the best ones.

####Likes Model
####According to related literature, likes are impacted by:
#####Time Effects (peak times, weekdays, months, time of day)
#####Text Language (word count, emotions, arousal, locomotion, assessment)
#####Content Type (we will use Type:Others as a reference group)

jnj_predictors_like.model_fit = eng_likes_log ~ 
  time_post_week_thu+time_post_month_jan+time_post_month_mar+time_post_month_apr+
  text_RMT_ass_per_logit+text_wordcount_log+
  text_NRC_anticipate_per_logit+text_NRC_disgust_per_logit+text_VAD_arousal+
  type_photo+type_video_views_log

jnj_like_model_fit  = lm(jnj_predictors_like.model_fit, data = jnj_train_like)
par(mfrow = c(2,2))
plot(jnj_like_model_fit)
summary(jnj_like_model_fit)
AIC(jnj_like_model_fit)

#####Assumption Tests

######Global test of linear model assumption
jnj_like_model_fit_gvlma = gvlma(jnj_like_model_fit) 
summary(jnj_like_model_fit_gvlma)
######All assumptions in the summary are violated, further robust tests and transformation are needed.

######Evaluating multi-collinearity
VIF(jnj_like_model_fit)
######All predictors are below 2 so multi-collinearity assumption is not violated.

######Assessing normality
par(mfrow = c(1,1))
qqPlot(jnj_like_model_fit, id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

######Assessing the independence of errors
durbinWatsonTest(jnj_like_model_fit)
######Independence of errors is violated.

######Assessing linearity
crPlots(jnj_like_model_fit)
######Despite the transformations, a lot of relationships seem to be non-linear.

######Assessing homoscedasticity
ncvTest(jnj_like_model_fit)
par(mfrow = c(1,1))
spreadLevelPlot(jnj_like_model_fit)
######Homoscedasticity of residual variance is violated. We will use WLS and adjust standard errors.

######User generated function for assessing normality
residplot = function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

#######Applying the function to test normality
par(mfrow = c(1,1))
residplot(jnj_like_model_fit)
########The plot for the like model seems to follow the normal curve, though a bit skewed to the right.

#_____________#

#####As many assumptions are still violated, we will attempt to try the Weighted Least Squares Regression

#define weights to use
jnj_like_model_fit_weights = 1 / lm(abs(jnj_like_model_fit$residuals) ~ jnj_like_model_fit$fitted.values)$fitted.values^2

#perform weighted least squares regression
jnj_like_model_fit_wls_model = lm(jnj_predictors_like.model_fit, data = jnj_train_like, weights=jnj_like_model_fit_weights)

#view summary of model
summary(jnj_like_model_fit_wls_model)

######As the model violates the homoscedasticity assumption, we will also see the model with adjust the standard errors.
jnj_like_model_fit_wls_model_cov = coeftest(jnj_like_model_fit_wls_model, vcov = vcovHC, save=TRUE) 

######Comparing the old model and the vcov model
huxreg("Like Model"   = jnj_like_model_fit, 
       "Like Model - WLS and Adjusted for Homoscedasticity" = jnj_like_model_fit_wls_model_cov,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

par(mfrow = c(2,2))
plot(jnj_like_model_fit_wls_model)

######Global test of linear model assumption
jnj_like_model_fit_wls_model_gvlma = gvlma(jnj_like_model_fit_wls_model) 

summary(jnj_like_model_fit_gvlma) #old model
summary(jnj_like_model_fit_wls_model_gvlma) #new model WLS
######Compared to the first model, the WLS is able to satisfy the homoskedasticity assumption.
ncvTest(jnj_like_model_fit_wls_model) #no more violation


#_____________#

####Validation tests for Like Model

#####Predictions Tests

jnj_like_model_fit_predictions = jnj_like_model_fit_wls_model %>% predict(jnj_test_like, vcov = vcovHC)
data.frame(R2 = R2(jnj_like_model_fit_predictions, jnj_test_like$eng_likes_log),
           RMSE = RMSE(jnj_like_model_fit_predictions, jnj_test_like$eng_likes_log),
           MAE = MAE(jnj_like_model_fit_predictions, jnj_test_like$eng_likes_log))

#####Repeated K-fold model cross-validation
######Define the training control
set.seed(113)
train.control = trainControl(method = "repeatedcv", 
                             number = 10, repeats = 3)
######Train the model
jnj_like_model_fit_trained = train(jnj_predictors_like.model_fit, data = jnj_train_like, method = "lm",
                                   trControl = train.control)
######Summarize the results
print(jnj_like_model_fit_trained)

#_____________#

####Comments Model
####According to related literature, comments are impacted by:
#####Time Effects (weekdays, months, time of day)
#####Reactions (Negative and Positive reactions. We used Positive as reference group.)
#####Text Language (emotions, arousal, locomotion, assessment)
#####Content Type (we will use Type:Others as a reference group)

jnj_predictors_comm.model_fit = eng_comments_log ~ 
  time_post_week_wed+time_post_month_feb+time_post_month_mar+time_post_month_aug+
  time_post_month_sep+time_post_month_oct+time_post_month_nov+
  reaction_neg_per_logit+text_NRC_anticipate_per_logit+text_NRC_disgust_per_logit+
  type_photo+type_video_views_log+time_post_time_of_day_log+
  text_RMT_loc_predominance_logit

jnj_comm_model_fit = lm(jnj_predictors_comm.model_fit, data = jnj_train_comm)
par(mfrow = c(2,2))
plot(jnj_comm_model_fit)
summary(jnj_comm_model_fit)
AIC(jnj_comm_model_fit)

#####We also tested models with the interaction terms:reaction_neg_per_logit:text_NRC_neg_per_logit+text_NRC_neg_per_logit:type_video+text_VAD_arousal:type_video
#####However, these interaction terms were not significant, the non-interaction term model had marginally lower R2 but better and lower AIC

#####Assumption Tests

######Global test of linear model assumption
jnj_comm_model_fit_gvlma = gvlma(jnj_comm_model_fit) 
summary(jnj_comm_model_fit_gvlma)
######All assumptions in the summary are violated, except homoskedasticity which is accepted.
######We will still validate this through ncvTest.

######Evaluating multi-collinearity
VIF(jnj_comm_model_fit)
######text_NRC_anger_per_logit seem to violate the multi-collinearity assumption.
######For the final model, we have dropped this.

######Assessing normality
par(mfrow = c(1,1))
qqPlot(jnj_comm_model_fit, id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

######Assessing the independence of errors
durbinWatsonTest(jnj_comm_model_fit) #violated

######Assessing linearity
par(mfrow = c(1,1))
crPlots(jnj_comm_model_fit)
######Similarly, a lot of relationships seem to be non-linear.

######Assessing homoscedasticity
ncvTest(jnj_comm_model_fit)
par(mfrow = c(1,1))
spreadLevelPlot(jnj_comm_model_fit)
######Though the gvlma accepted the homoskedasticity assumption, there is still violation on ncvTest.
######As such, we need to alter the standard errors and apply WLS.

#######Applying the function to test normality
par(mfrow = c(1,1))
residplot(jnj_comm_model_fit)

#_____________#

#####As many assumptions are still violated, we will attempt to try the Weighted Least Squares Regression

#define weights to use
jnj_comm_model_fit_weights = 1 / lm(abs(jnj_comm_model_fit$residuals) ~ jnj_comm_model_fit$fitted.values)$fitted.values^2

#perform weighted least squares regression
jnj_comm_model_fit_wls_model = lm(jnj_predictors_comm.model_fit, data = jnj_train_comm, weights=jnj_comm_model_fit_weights)

#view summary of model
summary(jnj_comm_model_fit_wls_model)

######As the model violates the homoscedasticity assumption, we will also see the model with adjust the standard errors.
jnj_comm_model_fit_wls_model_cov = coeftest(jnj_comm_model_fit_wls_model, vcov = vcovHC, save=TRUE) 

######Comparing the old model and the vcov model
huxreg("Comments Model"   = jnj_comm_model_fit, 
       "Comments Model - WLS and Adjusted for Homoscedasticity" = jnj_comm_model_fit_wls_model_cov,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

par(mfrow = c(2,2))
plot(jnj_comm_model_fit_wls_model)

######Global test of linear model assumption
jnj_comm_model_fit_wls_model_gvlma = gvlma(jnj_comm_model_fit_wls_model) 

summary(jnj_comm_model_fit_gvlma) #old model
summary(jnj_comm_model_fit_wls_model_gvlma) #new model WLS
######Compared to the first model, the WLS is able to satisfy the homoskedasticity assumption.

######Similarly to the like model, the WLS model is able to satisfy the homoskedasticity assumption.
ncvTest(jnj_comm_model_fit_wls_model) #no more violation

#_____________#

####Validation tests for Comm Model

#####Predictions Tests

jnj_comm_model_fit_predictions = jnj_comm_model_fit_wls_model %>% predict(jnj_test_comm, vcov = vcovHC)
data.frame(R2 = R2(jnj_comm_model_fit_predictions, jnj_test_comm$eng_comments_log),
           RMSE = RMSE(jnj_comm_model_fit_predictions, jnj_test_comm$eng_comments_log),
           MAE = MAE(jnj_comm_model_fit_predictions, jnj_test_comm$eng_comments_log))

#####Repeated K-fold model cross-validation
######Define the training control
set.seed(113)
train.control = trainControl(method = "repeatedcv", 
                             number = 10, repeats = 3)
######Train the model
jnj_comm_model_fit_trained = train(jnj_predictors_comm.model_fit, data = jnj_train_comm, method = "lm",
                                   trControl = train.control)
######Summarize the results
print(jnj_comm_model_fit_trained)

#_____________#

####Shares Model
####According to related literature, comments are impacted by:
#####Time Effects (peaktimes,time of day)
#####Reactions (we will use Positive as reference)
#####Text Language (emotions, arousal, locomotion, assessment)
#####Content Type (we will use Type:Others as a reference group)

jnj_predictors_share.model_fit_IT1 =  eng_shares_log ~ 
  time_post_hour_peak + text_NRC_anticipate_per_logit+text_NRC_disgust_per_logit+
  text_NRC_trust_per_logit+text_NRC_pos_per_logit+text_VAD_arousal+
  type_photo+text_VAD_arousal:type_video_views_log+type_video_views_log

jnj_share_model_fit = lm(jnj_predictors_share.model_fit_IT1, data = jnj_train_share)
par(mfrow = c(2,2))
plot(jnj_share_model_fit)
summary(jnj_share_model_fit)
AIC(jnj_share_model_fit)

#####Unlike the comments model, the interaction term text_VAD_arousal:type_video_views_log seem to provide a good addition of predictive power to the shares model.
#####The model interaction term had lower AIC and better r-squared. 

#####Assumption Tests

######Global test of linear model assumption
jnj_share_model_fit_gvlma = gvlma(jnj_share_model_fit) 
summary(jnj_share_model_fit_gvlma)
######Initially, the homoskedasticity assumption is accepted.   

######Evaluating multi-collinearity
VIF(jnj_share_model_fit)
######Similar to the comments mode, text_NRC_anger_per_logit seem to violate the multi-collinearity assumption. We removed it from the final mode.
######The interaction term is expected to have a high VIF.

######Assessing normality
par(mfrow = c(1,1))
qqPlot(jnj_share_model_fit, id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

######Assessing the independence of errors
durbinWatsonTest(jnj_share_model_fit)

######We attempted to use crPlots but it is not available for models with interactions.

######Assessing homoscedasticity
ncvTest(jnj_share_model_fit)
par(mfrow = c(1,1))
spreadLevelPlot(jnj_share_model_fit)
######As homoskedasticity assumption is also violated, we need to alter the standard errors and apply WLS.

#######Applying the function to test normality
par(mfrow = c(1,1))
residplot(jnj_share_model_fit)
########The plot for the share model seems to follow the normal curve but there is a small skewness evident from the curve.

#_____________#

#####As many assumptions are still violated, we will attempt to try the Weighted Least Squares Regression

#define weights to use
jnj_share_model_fit_weights = 1 / lm(abs(jnj_share_model_fit$residuals) ~ jnj_share_model_fit$fitted.values)$fitted.values^2

#perform weighted least squares regression
jnj_share_model_fit_wls_model = lm(jnj_predictors_share.model_fit_IT1, data = jnj_train_share, weights=jnj_share_model_fit_weights)

#view summary of model
summary(jnj_share_model_fit_wls_model)

######As the model violates the homoscedasticity assumption, we will also see the model with adjust the standard errors.
jnj_share_model_fit_wls_model_cov = coeftest(jnj_share_model_fit_wls_model, vcov = vcovHC, save=TRUE) 

######Comparing the old model and the vcov model
huxreg("Share Model"   = jnj_share_model_fit, 
       "Share Model - WLS and Adjusted for Homoscedasticity" = jnj_share_model_fit_wls_model_cov,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

par(mfrow = c(2,2))
plot(jnj_share_model_fit_wls_model)

######Global test of linear model assumption
jnj_share_model_fit_wls_model_gvlma = gvlma(jnj_share_model_fit_wls_model) 

summary(jnj_share_model_fit_gvlma) #old model
summary(jnj_share_model_fit_wls_model_gvlma) #new model WLS
######Likewise, the WLS model for Shares is able to satisfy the homoskedasticity assumption, as well as the Link assumption.
ncvTest(jnj_share_model_fit_wls_model) #no more violation

#_____________#

####Validation tests for Shares Model

#####Predictions Tests

jnj_share_model_fit_predictions = jnj_share_model_fit_wls_model %>% predict(jnj_test_share, vcov = vcovHC)
data.frame(R2 = R2(jnj_share_model_fit_predictions, jnj_test_share$eng_shares_log),
           RMSE = RMSE(jnj_share_model_fit_predictions, jnj_test_share$eng_shares_log),
           MAE = MAE(jnj_share_model_fit_predictions, jnj_test_share$eng_shares_log))

#####Repeated K-fold model cross-validation
######Define the training control
set.seed(113)
train.control = trainControl(method = "repeatedcv", 
                             number = 10, repeats = 3)
######Train the model
jnj_share_model_fit_trained = train(jnj_predictors_share.model_fit_IT1, data = jnj_train_share, method = "lm",
                                   trControl = train.control)
######Summarize the results
print(jnj_share_model_fit_trained)
#______________________________________________________________________________________________________________#

#Applying the Models for Deeper Insights

##Models for Different Time Periods
###By analysing through different time periods based on the pandemic, we can get an overview of how our audience has been affected by the pandemic.

###Partitioning overall data to apply the chosen models
jnjdata_prepandemic = subset(jnjdata_d, jnjdata_d$time_post_prepandemic==1)
jnjdata_pandemic = subset(jnjdata_d, jnjdata_d$time_post_pandemic==1)
jnjdata_postpandemic = subset(jnjdata_d, jnjdata_d$time_post_postpandemic==1)

###Likes across Periods
jnj_like_model_fit_prepandemic  = lm(jnj_predictors_like.model_fit, data = jnjdata_prepandemic)
jnj_like_model_fit_pandemic  = lm(jnj_predictors_like.model_fit, data = jnjdata_pandemic)
jnj_like_model_fit_postpandemic  = lm(jnj_predictors_like.model_fit, data = jnjdata_postpandemic)

huxreg("Like Model: Best Model" = jnj_like_model_fit_wls_model,
       "Like Model: PrePandemic"   = jnj_like_model_fit_prepandemic, 
       "Like Model: Pandemic" = jnj_like_model_fit_pandemic,
       "Like Model: PostPandemic" = jnj_like_model_fit_postpandemic,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

###Comments across Periods
jnj_comm_model_fit_prepandemic  = lm(jnj_predictors_comm.model_fit, data = jnjdata_prepandemic)
jnj_comm_model_fit_pandemic  = lm(jnj_predictors_comm.model_fit, data = jnjdata_pandemic)
jnj_comm_model_fit_postpandemic  = lm(jnj_predictors_comm.model_fit, data = jnjdata_postpandemic)

huxreg("Comment Model: Best Model" = jnj_comm_model_fit_wls_model,
       "Comment Model: PrePandemic"   = jnj_comm_model_fit_prepandemic, 
       "Comment Model: Pandemic" = jnj_comm_model_fit_pandemic,
       "Comment Model: PostPandemic" = jnj_comm_model_fit_postpandemic,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

###Shares across Periods
jnj_share_model_fit_prepandemic  = lm(jnj_predictors_share.model_fit_IT1, data = jnjdata_prepandemic)
jnj_share_model_fit_pandemic  = lm(jnj_predictors_share.model_fit_IT1, data = jnjdata_pandemic)
jnj_share_model_fit_postpandemic  = lm(jnj_predictors_share.model_fit_IT1, data = jnjdata_postpandemic)

huxreg("Share Model: Best Model" = jnj_share_model_fit_wls_model,
       "Share Model: PrePandemic"   = jnj_share_model_fit_prepandemic, 
       "Share Model: Pandemic" = jnj_share_model_fit_pandemic,
       "Share Model: PostPandemic" = jnj_share_model_fit_postpandemic,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01))

#______________________________________________________________________________________________________________#
########END########
#______________________________________________________________________________________________________________#
