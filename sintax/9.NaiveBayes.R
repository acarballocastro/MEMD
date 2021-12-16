packages <- c("e1071", "caret","naivebayes","C50")
sapply(packages, require, character.only = TRUE)

if(!exists('d.e')){
  path <-'../data'
  d.e <- read.csv2(paste0(path,'/',"hotel_bookings_proc.csv"), sep=",")
  d.e$adr<-as.numeric(d.e$adr)
  v<-list(
    categoric=c('hotel','is_canceled', 'arrival_date_month','arrival_date_year', 'meal','market_segment','distribution_channel',
                'is_repeated_guest','reserved_room_type','assigned_room_type', 'room_coherence', 
                'is_company', 'is_agent', 'customer_type','deposit_type', 'if_prev_cancel','if_wait'),
    integer=c('lead_time', 'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
              'booking_changes','required_car_parking_spaces','total_of_special_requests'),
    continua='adr')
  v$numeric <- c(v$integer,v$continua)
  library(ggplot2)
  for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
  for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
}
dd<- d.e 

set.seed(2018)
test<-sample(1:nrow(dd),size = nrow(dd)/3)
dataTrain<-dd[-test,]
dataTest<-dd[test,]
dd$is_canceled <- as.factor(dd$is_canceled)

mod <- naiveBayes(is_canceled ~ ., data = dataTrain)
mod

pred <- predict(mod, dataTest)
tab <- table(dataTest$is_canceled, pred)
confusionMatrix(tab)
