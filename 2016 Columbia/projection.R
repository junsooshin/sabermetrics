# by Jun Soo (Derek) Shin
# 2016 Columbia Diamond Dollars Case Competition
# Using Tom Tango's WAR projection formula to project WARs for the following
# years.
# Ethan Bein helped.

# the database we use is http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,58,3&season=2016&month=0&season1=2014&ind=1&team=0&rost=0&age=0&filter=&players=0
# it includes seasons from 2014-2016
# it includes Season, Name, Team, PA, WAR, and Age
db = read.csv("2014-2016.csv") 

# puts the seasons by the same player in the one row
db = reshape(db,
             timevar = "Season",
             idvar = c("Name", "playerid"),
             direction = "wide")

# replaces NA values with 0
db[is.na(db)] <- 0

# calculates WAR per PA (players with 0 PA get 0 WAR/PA)
db$WarPA2014 = ifelse(db$PA.2014==0, 0, db$WAR.2014/db$PA.2014)
db$WarPA2015 = ifelse(db$PA.2015==0, 0, db$WAR.2015/db$PA.2015)
db$WarPA2016 = ifelse(db$PA.2016==0, 0, db$WAR.2016/db$PA.2016)

# we don't actually use these
# db$War2014 = db$WarPA2014*600
# db$War2015 = db$WarPA2015*600
# db$War2016 = db$WarPA2016*600

# projection for the next year using the weighted past 3 season WARs 
db$WarPA2017Proj = ((5*db$WarPA2016*db$PA.2016 + 4*db$WarPA2015*db$PA.2015 + 3*db$WarPA2014*db$PA.2014)
                   /(5*db$PA.2016 + 4*db$PA.2015 + 3*db$PA.2014 + 400))

# gets coefficients for previous and previous-1 seasons
# and uses them to get db$PA2017Proj
reg = lm(PA.2016~PA.2015+PA.2014, data=db)
db$PA2017Proj = 0.8*db$PA.2016 + 0.1*db$PA.2015

# actual projections are (projected WAR per PA * projected PA)
db$WarActual2017ProjNoAging = db$WarPA2017Proj*db$PA2017Proj

# applies standard aging curve for the future
db$WarActual2017Proj = db$WarPA2017Proj*db$PA2017Proj+(0.1*(30-db$Age.2016))
db$WarActual2018Proj = db$WarActual2017Proj-0.4+(0.08*(29-db$Age.2016))
db$WarActual2019Proj = db$WarActual2018Proj-0.4+(0.03*(28-db$Age.2016))
db$WarActual2020Proj = db$WarActual2019Proj-0.4+(0.03*(27-db$Age.2016))
db$WarActual2021Proj = db$WarActual2020Proj-0.4+(0.03*(26-db$Age.2016))
db$WarActual2022Proj = db$WarActual2021Proj-0.4+(0.03*(25-db$Age.2016))

# gets rid of players who did not play in 2016
db = filter(db, Age.2016 != 0)

write.csv(db, "projection.csv")
