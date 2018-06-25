library(mongolite)
library(stringr)
library(dplyr)


# get data user data ###

db <- mongo(collection = "users_backup_2018-05-09",
            db = "kapi", 
            url = "mongodb://dac.dinh%40kyanon.digital:dacdinh@45.122.223.198:27017/kapi")

data.users <- db$find(paste0('{"total_friend":{"$gte":0} , "total_follower":{"$gt":0} }'),
                      fields = ('{"updated_at":0,"created_at":0,"_id":0}'), limit=5000000 )


# renamme data 1st col
colnames(data.users)[1] <- "from_user"

# select the appropriate features
data.users.c <- data.users[,c("from_user","total_friend","total_follower","networks")]

# check the query
data.users2 = db$find(query = '{ "$or" : [ {"total_friend" : {"$gte" : 0} }, { "total_follower" : {"$gt" : 0} } ] }',
                      fields = '{"updated_at":0, "created_at":0, "_id":0}', 
                      limit = 5000000 )


data.users3 = db$find(query = '{ "$and" : [ {"total_friend" : {"$gte" : 0} }, { "total_follower" : {"$gt" : 0} } ] }',
                      fields = '{"updated_at":0, "created_at":0, "_id":0}', 
                      limit = 5000000 )



#########################################################################################
##                                                                                     ##  
##                                                                                     ##
#########################################################################################


# get post info ###
db <- mongo(collection = "posts",
            db = "kapi", 
            url = "mongodb://dac.dinh%40kyanon.digital:dacdinh@45.122.223.198:27017/kapi")

options(scipen=1000)
d <- as.integer(as.POSIXct(strptime("2017-08-18","%Y-%m-%d"))) * 1000
d1 <- as.integer(as.POSIXct(("2015-12-20"))) * 1000

data.posts <- db$find(query = paste0('{"created_date":{"$gt": { "$date" : { "$numberLong" : "',d,'" } } }, "mention_to":"samsung"}'),
                      fields = ('{"updated_at":0,"created_at":0,"_id":0,"description":0,"interactions_count":0}'),
                      limit = 200000)



# q= paste0('{"created_date" : { "$gt" : {"$date" : {"$numberLong" : "',d,'"} } } }')

# check the query
data.posts2 = db$find(query = paste0('{"$and" : [{"created_date":{"$gt": { "$date" : { "$numberLong" : "', d,'" } } } }, {"mention_to" : "samsung"}]}') ,
                      fields = '{"updated_at" : 0, "created_at" : 0, "_id" : 0 ,"description" : 0, "interactions_count" : 0}',
                      limit = 200000)

identical(data.posts, data.posts2)


# format data  and calculate the required features ###
data.posts.c <- data.posts[,c("from_user","parent_id","message","likes_count","shares_count","comments_count","sentiment")]

data.posts.c <- transform(data.posts.c, sentiment = as.numeric(sentiment), likes_count=as.numeric(likes_count),
                          shares_count=as.numeric(shares_count),comments_count=as.numeric(comments_count))

data.posts.c$word_count <- str_count(data.posts.c$message,'\\w+')



#
Posts <- data.posts.c[is.na(data.posts.c$parent_id),] %>%
  group_by(from_user) %>%
  summarise(posts=n(),
            Positive_Post = sum(sentiment[sentiment=="1"], na.rm = T),
            Negative_Post = abs(sum(sentiment[sentiment=="-1"],na.rm=T) ),
            word_count_post = sum(word_count),
            post_likes=sum(likes_count),
            post_shares=sum(shares_count),
            post_comments=sum(comments_count) ) 

Shares <- data.posts.c[!is.na(data.posts.c$parent_id),] %>% 
  group_by(from_user) %>%
  summarise(shares=n(),
            Positive_Share = sum(sentiment[sentiment=="1"], na.rm = T),
            Negative_Share = abs(sum(sentiment[sentiment=="-1"], na.rm = T) ),
            word_count_share = sum(word_count),
            share_likes=sum(likes_count),
            share_shares=sum(shares_count),
            share_comments=sum(comments_count))


#########################################################################################
##                                                                                     ##  
##                                                                                     ##
#########################################################################################


# Comments ####

db <- mongo(collection = "comments",
            db = "kapi", 
            url = "mongodb://dac.dinh%40kyanon.digital:dacdinh@45.122.223.198:27017/kapi")

data.comments <- db$find(paste0('{"created_date":{"$gt": { "$date" : { "$numberLong" : "', d,'" } } }, "mention_to":"samsung"}'),
                         fields = ('{"updated_at":0,"created_at":0,"_id":0,"raw_message":0,"replies_link":0,"likes_link":0}'),
                         limit=250000) 

data.comments$word_count = str_count(data.comments$message, '\\w+') 

data.comments <- transform(data.comments, sentiment = as.numeric(sentiment),
                           likes_count = as.numeric(likes_count),
                           replies_count = as.numeric(replies_count) )

Comments <- data.comments %>% 
  group_by(from_user) %>% 
  summarise( 
    comments = n(),
    Positive_Comment = sum(sentiment[sentiment=="1"], na.rm = T), 
    Negative_Comment = abs(sum(sentiment[sentiment=="-1"],na.rm=T)),
    word_count_comments = sum(word_count),
    comments_likes_count = sum(likes_count),
    comments_replies_count= sum(replies_count) )


# join list ####
detach(package : dplyr)
library(plyr)
data <- join_all(list(Posts, Shares, Comments, data.users.c), by = 'from_user')

# data[is.na(data)] <- 0
