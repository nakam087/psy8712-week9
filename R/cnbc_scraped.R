# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)

# Data Import and Cleaning
cnbc_links<- c("https://www.cnbc.com/business/", #setting up links
               "https://www.cnbc.com/investing/",
               "https://www.cnbc.com/technology/",
               "https://www.cnbc.com/politics/")
sources<-c("Business", "Investing", "Tech","Politics") #stating sources
cnbc_tbl <- tibble(headline=as.character(),#preliminary tibble so all 4 sections will appear in the for loop
                   length=as.numeric(),
                   source=as.character())
for (i in 1:length(sources)){ #created a for loop, which will run through all the links and do one source at a time
  html<-read_html(cnbc_links[i]) #reading links
  elements<-html_elements(html,".Card-title") #getting headlines per link
  text<-html_text(elements) #making headlines into text
  length<-str_count(text,'\\w+') #counting each word in headline
  source<-sources[i] #stating what source
  html_tbl<-tibble(headline=text, #tibble with new information
                   length=length,
                   source=source)
  cnbc_tbl <- rbind(cnbc_tbl,html_tbl) #add to pre-existing tibble
}

# Visualization
#created a boxplot of source and length with labels
ggplot(cnbc_tbl,aes(source,length))+
  geom_boxplot()+
  labs(title="Relationship between headline length and category on CNBC",
       x="Article Category",
       y="Headline length")

# Analysis
#used aov to run anova comparing length by source
anova<-aov(length~source, data=cnbc_tbl)
(summary_anova<-summary(anova))#printed summary

# Publication
#[1] "The results of an ANOVA comparing lengths across sources was F(3, 130) = 3.01, p = .03. This test was statistically significant."
#found relevant values in summary anova 
pval<-summary_anova[[1]]$`Pr(>F)`[1] 
dfn<-summary_anova[[1]]$Df[1]
dfd<-summary_anova[[1]]$Df[2]
f<-summary_anova[[1]]$`F value`[1]
#used ifelse function to dictate significance, with yes printing "was"
significant<-ifelse(pval<0.05,yes="was",no="was not")
#message
paste0("The results of an ANOVA comparing lengths across sources was F(",round(dfn,0),", ", round(dfd,0),") = ",str_remove(formatC(f, format="f",digits=2),"^0"),", p = ",str_remove(formatC(pval, format="f",digits=2),"^0"),". This test ",significant," statistically significant.")
     