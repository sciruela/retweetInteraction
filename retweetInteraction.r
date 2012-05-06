require(ggplot2)
require(twitteR)

searchTerm='@_iNat'
rdmTweets <- searchTwitter(searchTerm, n=1500)

tw.df=twListToDF(rdmTweets)

require(plyr)
tw.dfx=ddply(tw.df, .var = "screenName", .fun = function(x) {return(subset(x, created %in% min(created),select=c(screenName,created)))})
tw.dfxa=arrange(tw.dfx,-desc(created))
tw.df$screenName=factor(tw.df$screenName, levels = tw.dfxa$screenName)

library(stringr)
trim <- function (x) sub('@','',x)
tw.df$rt=sapply(tw.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
tw.df$rtt=sapply(tw.df$rt,function(rt) if (is.na(rt)) 'T' else 'RT')

tw.df$rtof=sapply(tw.df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))


require(gdata)
tw.df.rt=drop.levels(subset(tw.df,subset=(!is.na(rtof))))
tw.df.rta=arrange(ddply(tw.df.rt, .var = "screenName", .fun = function(x) {return(subset(x, created %in% min(created),select=c(screenName,created)))}),-desc(created))
tw.df.rt$screenName=factor(tw.df.rt$screenName, levels = tw.df.rta$screenName)


g = ggplot(subset(tw.df.rt,subset=(!is.na(rtof))))
g = g + geom_linerange(aes(x=created,ymin=screenName,ymax=rtof),colour='lightgrey')

g = g + geom_point(data=(tw.df),aes(x=created,y=screenName),colour='lightgrey')

g = g + geom_point(aes(x=created,y=screenName),colour='lightgrey') + geom_point(aes(x=created,y=rtof),colour='grey') + opts(axis.text.y=theme_text(size=5))

subdata.rtof=function(u) return(subset(tw.df.rt,subset=(!is.na(rtof) & rtof==u)))
subdata.user=function(u) return(subset(tw.df.rt,subset=(!is.na(rtof) & screenName==u)))


s1='AngeLa8aC'
ss1=subdata.rtof(s1)
ss1x=subdata.user(s1)
sc1='aquamarine3'

g = g + geom_linerange(data=ss1,aes(x=created,ymin=screenName,ymax=rtof),colour=sc1)

s2='_iNat'
ss2=subdata.rtof(s2)
ss2x=subdata.user(s2)
sc2='orange'

g = g + geom_linerange(data=ss2,aes(x=created,ymin=screenName,ymax=rtof),colour=sc2)

g = g + geom_point(data=ss1,aes(x=created,y=rtof),colour=sc1) + geom_point(data=ss1,aes(x=created,y=screenName),colour=sc1)
g = g + geom_point(data=ss2,aes(x=created,y=rtof),colour=sc2) + geom_point(data=ss2,aes(x=created,y=screenName),colour=sc2)

g = g + geom_point(data=(ss1x),aes(x=created,y=screenName),colour='black',size=1)
g = g + geom_point(data=(ss2x),aes(x=created,y=screenName),colour='black',size=1)

print(g)