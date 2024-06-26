# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(RedditExtractoR)

# Data Import and Cleaning
rstats_df<-find_thread_urls(subreddit="rstats", period='month') #getting posts from r/rstats for 1 month
content_df<-get_thread_content(rstats_df$url)#parsing urls from rstats_df to retrieve metadata and comments
#create tibble of titles, number of comments, and number of upvotes (stored within threads of content_df)
rstats_tbl <- tibble(post = rstats_df$title,
                     comments = rstats_df$comments,
                     upvotes = content_df$threads$upvotes
                     )

# Visualization
ggplot(rstats_tbl,aes(comments,upvotes))+ #both comments and upvotes are continuous so...
  geom_point()+ #visualized using a scatterplot
  geom_smooth(method = "lm",se=F)+ #added a line
  labs(x="Number of comments", #added a title and axis titles as well
       y="Number of upvotes",
       title="Relationship between upvotes and comments")

# Analysis
#selected cor.test because it is more "in-depth" than cor and contains all the information we need
cor<-cor.test(rstats_tbl$comments, 
         rstats_tbl$upvotes, 
         method="pearson")

# Publication
#[1] "The correlation between upvotes and comments was r(121) =.53, p = .00. This test was statistically significant."
# created an if else statement that will print 'was' if the result is less than alpha = 0.05, else print 'was not'
if_stat<- if (cor$p.value < 0.05){
  print("was")
} else {
  print("was not")
}
#rounded ppm coefficient and pvalue based on last week's strategy
ppm<- str_remove(formatC(cor$estimate, format="f",digits=2),"^0")
pval <- str_remove(formatC(cor$p.value, format="f",digits=2),"^0")
#printing statement with dynamic values
paste0("The correlation between upvotes and comments was r(",round(cor$parameter,0),") =", ppm,", p = ", pval,". This test ", if_stat ," statistically significant.")
