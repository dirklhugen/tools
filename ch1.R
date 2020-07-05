################
### Example 1.2
#con <- url("http://www2.math.su.se/~esbj/GLMbook/moppe.sas")
#data <- readLines(con, n = 200L, warn = FALSE, encoding = "unknown")
#close(con)
setwd("/Users/dirk/Desktop/Non Life Insurance Pricing")
data <- readLines("moppe.sas", n = 200L, warn = FALSE, encoding = "unknown")
## Find the data range
data.start <- grep("^cards;", data) + 1L
data.end   <- grep("^;", data[data.start:999L]) + data.start - 2L
table.1.2  <- read.table(text = data[data.start:data.end],
                         header = FALSE, sep = "", quote = "",
                         col.names = c("premiekl", "moptva", "zon", "dur",
                                       "medskad", "antskad", "riskpre", "helpre", "cell"),
                         na.strings = NULL,
                         colClasses = c(rep("factor", 3), "numeric",
                                        rep("integer", 4), "NULL"),
                         comment.char = "")
rm(con, data, data.start, data.end)     # Cleanup
comment(table.1.2) <-
  c("Title: Partial casco moped insurance from Wasa insurance, 1994--1999",
    "Source: http://www2.math.su.se/~esbj/GLMbook/moppe.sas",
    "Copyright: http://www2.math.su.se/~esbj/GLMbook/")
## See the SAS code for this derived field
table.1.2$skadfre = with(table.1.2, antskad / dur)
## English language column names as comments:
comment(table.1.2$premiekl) <-
  c("Name: Class",
    "Code: 1=Weight over 60kg and more than 2 gears",
    "Code: 2=Other")
comment(table.1.2$moptva)   <-
  c("Name: Age",
    "Code: 1=At most 1 year",
    "Code: 2=2 years or more")
comment(table.1.2$zon)      <-
  c("Name: Zone",
    "Code: 1=Central and semi-central parts of Sweden's three largest cities",
    "Code: 2=suburbs and middle-sized towns",
    "Code: 3=Lesser towns, except those in 5 or 7",
    "Code: 4=Small towns and countryside, except 5--7",
    "Code: 5=Northern towns",
    "Code: 6=Northern countryside",
    "Code: 7=Gotland (Sweden's largest island)")
comment(table.1.2$dur)      <-
  c("Name: Duration",
    "Unit: year")
comment(table.1.2$medskad)  <-
  c("Name: Claim severity",
    "Unit: SEK")
comment(table.1.2$antskad)  <- "Name: No. claims"
comment(table.1.2$riskpre)  <-
  c("Name: Pure premium",
    "Unit: SEK")
comment(table.1.2$helpre)   <-
  c("Name: Actual premium",
    "Note: The premium for one year according to the tariff in force 1999",
    "Unit: SEK")
comment(table.1.2$skadfre)  <-
  c("Name: Claim frequency",
    "Unit: /year")
## Save results for later
save(table.1.2, file = "table.1.2.RData")
## Print the table (not as pretty as the book)
print(table.1.2)
################

# Example 1.3 - replicate table 1.4
rating.factor <-
  with(table.1.2,
       c(rep("Vehicle class", nlevels(premiekl)),
         rep("Vehicle age", nlevels(moptva)),
         rep("Zone", nlevels(zon))))
rating.factor

## The Class column
class.num <- with(table.1.2, c(levels(premiekl), levels(moptva), levels(zon)))
class.num

## The Duration is the sum of durations within each class
duration.total <-
  c(with(table.1.2, tapply(dur, premiekl, sum)),
    with(table.1.2, tapply(dur, moptva, sum)),
    with(table.1.2, tapply(dur, zon, sum)))
duration.total
table.1.2<- mutate(table.1.2,class=premiekl,age=moptva,zone=zon)

group_by(table.1.2, class) %>% summarize(totDur=sum(dur)) %>% mutate(maxSum=max(totDur))
group_by(table.1.2, age) %>% summarize(totDur=sum(dur))  %>% mutate(maxSum=max(totDur))
group_by(table.1.2, zone) %>% summarize(totDur=sum(dur))  %>% mutate(maxSum=max(totDur))

fac<-c(rep("class",2),rep("age",2), rep("zone", 7))

max_df <- group_by(table.1.2,class)
## Calculate relativities in the tariff
## The denominator of the fraction is the class with the highest exposure
## (i.e. the maximum total duration): we make that explicit with the
## which.max() construct.  We also set the contrasts to use this as the base,
## which will be useful for the glm() model later.
class.base <- which.max(duration.total[1:2])
age.base   <- which.max(duration.total[3:4])
zone.base  <- which.max(duration.total[5:11])

class_df<-group_by(table.1.2, class) %>%
  summarize(totDur=sum(dur),totPrem=sum(helpre))
colnames(class_df)[1]<-'level'
age_df<-group_by(table.1.2, age) %>%
  summarize(totDur=sum(dur),totPrem=sum(helpre))
colnames(age_df)[1]<-'level'
zone_df<-group_by(table.1.2, zone) %>%
  summarize(totDur=sum(dur),totPrem=sum(helpre))
colnames(zone_df)[1]<-'level'

df <- cbind(fac,rbind(class_df,age_df,zone_df))
fac_df <- group_by(df,fac) %>% summarize(maxDur=max(totDur))
left_join(df,left_join(fac_df,df,by=c('maxDur'='totDur','fac'))[c(1,4)],by='fac') %>% mutate(rel=totPrem.x/totPrem.y)

rt.class <- with(table.1.2, tapply(helpre, premiekl, sum))
rt.class <- rt.class / rt.class[class.base]
rt.age   <- with(table.1.2, tapply(helpre, moptva, sum))
rt.age   <- rt.age / rt.age[age.base]
rt.zone  <- with(table.1.2, tapply(helpre, zon, sum))
rt.zone  <- rt.zone / rt.zone[zone.base]

contrasts(table.1.2$premiekl) <-
  contr.treatment(nlevels(table.1.2$premiekl))[rank(-duration.total[1:2],
                                                    ties.method = "first"), ]
contrasts(table.1.2$moptva) <-
  contr.treatment(nlevels(table.1.2$moptva))[rank(-duration.total[3:4],
                                                  ties.method = "first"), ]
contrasts(table.1.2$zon) <-
  contr.treatment(nlevels(table.1.2$zon))[rank(-duration.total[5:11],
                                               ties.method = "first"), ]

## Relativities of MMT; we use the glm approach here as per the book’s
## SAS code at http://www2.math.su.se/~esbj/GLMbook/moppe.sas
m <- glm(riskpre ~ premiekl + moptva + zon, data = table.1.2,
         family = poisson("log"), weights = dur)
summary(m)
## If the next line is a mystery then you need to
## (1) read up on contrasts or
## (2) remember that the link function is log() which is why we use exp here
rels <- exp( coef(m)[1] + coef(m)[-1] ) / exp(coef(m)[1])
rels
rm.class <- c(1, rels[1])               # See rm.zone below for the
rm.age   <- c(rels[2], 1)               # general approach
rm.zone  <- c(1, rels[3:8])[rank(-duration.total[5:11], ties.method = "first")]

## Create and save the data frame
table.1.4 <-
  data.frame(Rating.factor = rating.factor, Class = class.num,
             Duration = duration.total,
             Rel.tariff = c(rt.class, rt.age, rt.zone),
             Rel.MMT    = c(rm.class, rm.age, rm.zone))
save(table.1.4, file = "table.1.4.RData")
print(table.1.4, digits = 3)
rm(rating.factor, class.num, duration.total, class.base, age.base, zone.base,
   rt.class, rt.age, rt.zone, rm.class, rm.age, rm.zone, m, rels)
################