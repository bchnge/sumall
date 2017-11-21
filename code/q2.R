setwd('~/sumall_interview/')

df <- read.csv('q2_data.csv')

require(ggplot2)
require(plyr)
require(lubridate)

df_2 <- ddply(df, .variables = c('year','month','day', 'weekday'), summarise,
              numTweets = NROW(time))

require(RJSONIO)

get_top_news <- function(x){
  
  api_key <- 'add774017a2f233321c7045c6b1d74e5:14:68394663'
  baseUrl <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?"

  Q='syria'
  
  yr <- as.character(as.integer(x[1]))
  mo <- as.character(as.integer(x[2]))
  day <- as.character(as.integer(x[3]))
  if (nchar(mo) == 1){
    mo <- paste('0',mo,sep='')
  }
  if (nchar(day) == 1){
    day <- paste('0',day,sep='')
  }
  BEGIN_DATE=as.character(paste(yr,mo,day,sep=''))

  Url <- paste(baseUrl, 'q=',Q,'&fq=news_desk:("Foreign")&begin_date=',BEGIN_DATE,'&end_date=',BEGIN_DATE,'&api-key=',api_key, sep='')
  raw.data <- readLines(Url, warn=F)
  
  res  <- fromJSON(raw.data)
  results <- res$response$docs
  results[[1]]$headline[[1]]
}

# pull headlines for each date
df_2$headline <- apply(df_2, MARGIN = 1, FUN = get_top_news)



require(ggthemes)

df_2$date <- with(df_2, ydm(paste(year, '-',day,'-',month)))
df_2 <- df_2[with(df_2, order(date)),]
g <- ggplot(df_2, aes(x = date))

g + geom_point(aes(y = numTweets), size = 12, alpha = 0.6, color = 'steelblue') +
  geom_area(aes(y = numTweets), fill = 'steelblue', color = 'steelblue', alpha = 0.3) + 
  geom_text(hjust = 0, angle = 90, aes(y = numTweets + 15000, label =headline, alpha = numTweets),
            size = 4.5, color = 'steelblue', face = 'bold') + 
  geom_text(aes(y = numTweets, label = round(numTweets/1000))) + 
  scale_alpha_continuous(guide = FALSE) + scale_y_continuous(breaks = NULL, limits = c(0,600000)) + 
  ggtitle('Syrian Tweets (8-23-2013 - 10-15-2013) and Related News Using New York Times Article Search API') + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  scale_size(c(2,10)) + xlab('Date') + ylab('Number of Tweets (Thousands)') + 
  theme_few()
