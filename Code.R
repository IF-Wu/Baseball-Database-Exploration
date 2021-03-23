library(dplyr)
library(RSQLite)
library(ggplot2)
baseball <- dbConnect(SQLite(), "lahman2013.sqlite") # Connect to the database
tblNames <- dbListTables(baseball) # Explore the tables in the database
fields <- lapply(tblNames, function(tbl) dbListFields(baseball, tbl)) # Explore the fields in each table

# Bring the entire collection of tables from the database
all.table <- lapply(tblNames, function(tbl) dbGetQuery(baseball, sprintf("SELECT * FROM %s", tbl)))
names(all.table) <- tblNames

# We can also import a single table into R, 24 in total
AllstarFull <- dbGetQuery(baseball, "SELECT * FROM AllstarFull")
Appearances <- dbGetQuery(baseball, "SELECT * FROM Appearances")
AwardsManagers <- dbGetQuery(baseball, "SELECT * FROM AwardsManagers")
AwardsPlayers <- dbGetQuery(baseball, "SELECT * FROM AwardsPlayers")
AwardsShareManagers <- dbGetQuery(baseball, "SELECT * FROM AwardsShareManagers")
AwardsSharePlayers <- dbGetQuery(baseball, "SELECT * FROM AwardsSharePlayers")
Batting <- dbGetQuery(baseball, "SELECT * FROM Batting")
BattingPost <- dbGetQuery(baseball, "SELECT * FROM BattingPost")
Fielding <- dbGetQuery(baseball, "SELECT * FROM Fielding")
FieldingOF <- dbGetQuery(baseball, "SELECT * FROM FieldingOF")
FieldingPost <- dbGetQuery(baseball, "SELECT * FROM FieldingPost")
HallOfFame <- dbGetQuery(baseball, "SELECT * FROM HallOfFame")
Managers <- dbGetQuery(baseball, "SELECT * FROM Managers")
ManagersHalf <- dbGetQuery(baseball, "SELECT * FROM ManagersHalf")
Master <- dbGetQuery(baseball, "SELECT * FROM Master")
Pitching <- dbGetQuery(baseball, "SELECT * FROM Pitching")
PitchingPost <- dbGetQuery(baseball, "SELECT * FROM PitchingPost")
Salaries <- dbGetQuery(baseball, "SELECT * FROM Salaries")
Schools <- dbGetQuery(baseball, "SELECT * FROM Schools")
SchoolsPlayers <- dbGetQuery(baseball, "SELECT * FROM SchoolsPlayers")
SeriesPost <- dbGetQuery(baseball, "SELECT * FROM SeriesPost")
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
TeamsFranchises <- dbGetQuery(baseball, "SELECT * FROM TeamsFranchises")
TeamsHalf <- dbGetQuery(baseball, "SELECT * FROM TeamsHalf")

# Question 1: What years does the data cover? are there data for each of these years?
# Since there are many table in the database, I will select Batting table as an example.
dbGetQuery(baseball, "SELECT COUNT(DISTINCT yearID)
                      FROM Batting")
dbGetQuery(baseball, "SELECT DISTINCT yearID
                      FROM Batting
                      ORDER BY yearID")
# We can see there are totally 143 years data in this table(from 1871 to 2013), and we have the batting data for each year.
# We will check whether other tables also have the data from 1871 to 2013, we use Managers table and Salaries table to verify.
dbGetQuery(baseball, "SELECT COUNT(DISTINCT yearID), MAX(DISTINCT yearID), MIN(DISTINCT yearID)
                      FROM Managers")
dbGetQuery(baseball, "SELECT COUNT(DISTINCT yearID), MAX(DISTINCT yearID), MIN(DISTINCT yearID)
                      FROM Salaries")
# We can see that the Managers table still have the data from 1871 to 2013, while the Salaries table only have 29 years data from 1985 to 2013.

# Using R to validate
# Batting table
Batting <- dbGetQuery(baseball, "SELECT * FROM Batting")
c(From = min(Batting$yearID), To = max(Batting$yearID), Duration = length(unique(Batting$yearID)))
# Managers table
Managers <- dbGetQuery(baseball, "SELECT * FROM Managers")
c(From = min(Managers$yearID), To = max(Managers$yearID), Duration = length(unique(Managers$yearID)))
# Salaries table
Salaries <- dbGetQuery(baseball, "SELECT * FROM Salaries")
c(From = min(Salaries$yearID), To = max(Salaries$yearID), Duration = length(unique(Salaries$yearID)))

# Question 2: How many (unique) people are included in the database? How many are players, managers, etc?
# Select all tables associate with playerID
total_unique <- dbGetQuery(baseball, "SELECT COUNT(DISTINCT [unique].playerID)
                                      FROM(
                                        SELECT PlayerID FROM Master
                                        UNION SELECT PlayerID FROM Batting
                                        UNION SELECT PlayerID FROM Pitching
                                        UNION SELECT PlayerID FROM Fielding
                                        UNION SELECT PlayerID FROM AllstarFull
                                        UNION SELECT PlayerID FROM HallOfFame
                                        UNION SELECT PlayerID FROM Managers
                                        UNION SELECT PlayerID FROM BattingPost
                                        UNION SELECT PlayerID FROM PitchingPost
                                        UNION SELECT PlayerID FROM FieldingOF
                                        UNION SELECT PlayerID FROM ManagersHalf
                                        UNION SELECT PlayerID FROM Salaries
                                        UNION SELECT PlayerID FROM AwardsManagers
                                        UNION SELECT PlayerID FROM AwardsPlayers
                                        UNION SELECT PlayerID FROM AwardsShareManagers
                                        UNION SELECT PlayerID FROM AwardsSharePlayers
                                        UNION SELECT PlayerID FROM FieldingPost
                                        UNION SELECT PlayerID FROM Appearances
                                        UNION SELECT PlayerID FROM SchoolsPlayers
                                      ) AS [unique]")
total_unique # There are 18359 unique people included in the database
# Select all tables associate with manager
manager_unique <- dbGetQuery(baseball, "SELECT COUNT(DISTINCT manager.playerID)
                                        FROM(
                                          SELECT playerID FROM Managers
                                          UNION SELECT playerID FROM ManagersHalf
                                          UNION SELECT playerID FROM AwardsManagers
                                          UNION SELECT PlayerID FROM AwardsShareManagers
                                        ) AS manager")
manager_unique # There are 682 managers in the database
# Select all tables associate with player
# Since the HallofFrame table contains people other than players, we just ignore this table.
player_unique <- dbGetQuery(baseball, "SELECT COUNT(DISTINCT player.playerID)
                                       FROM(
                                         SELECT PlayerID FROM Batting
                                         UNION SELECT PlayerID FROM Pitching
                                         UNION SELECT PlayerID FROM Fielding
                                         UNION SELECT PlayerID FROM AllstarFull
                                         UNION SELECT PlayerID FROM BattingPost
                                         UNION SELECT PlayerID FROM PitchingPost
                                         UNION SELECT PlayerID FROM FieldingOF
                                         UNION SELECT PlayerID FROM Salaries
                                         UNION SELECT PlayerID FROM AwardsPlayers
                                         UNION SELECT PlayerID FROM AwardsSharePlayers
                                         UNION SELECT PlayerID FROM FieldingPost
                                         UNION SELECT PlayerID FROM Appearances
                                         UNION SELECT PlayerID FROM SchoolsPlayers
                                       ) AS player")
player_unique # There are 18173 players in the database

# Using R to validate
all.ID <- c(Master$playerID, Batting$playerID, Pitching$playerID, Fielding$playerID, AllstarFull$playerID, HallOfFame$playerID, Managers$playerID, BattingPost$playerID, PitchingPost$playerID, FieldingOF$playerID, ManagersHalf$playerID, Salaries$playerID, AwardsManagers$playerID, AwardsPlayers$playerID, AwardsShareManagers$playerID, AwardsSharePlayers$playerID, FieldingPost$playerID, Appearances$playerID, SchoolsPlayers$playerID)
length(unique(all.ID)) # 18359 in total

manager.ID <- c(Managers$playerID, ManagersHalf$playerID, AwardsManagers$playerID, AwardsShareManagers$playerID)
length(unique(manager.ID)) # 682 in total

player.ID <- c(Batting$playerID, Pitching$playerID, Fielding$playerID, AllstarFull$playerID, BattingPost$playerID, PitchingPost$playerID, FieldingOF$playerID, Salaries$playerID, AwardsPlayers$playerID, AwardsSharePlayers$playerID, FieldingPost$playerID, Appearances$playerID, SchoolsPlayers$playerID)
length(unique(player.ID)) # 18173 in total

# Question 3: How many players became managers?
# Count the number of Y in the plyrMgr field
# We need to get the unique manager name
# First get the number of player-manager
dbGetQuery(baseball, "SELECT COUNT(DISTINCT playerID) AS Count, plyrMgr AS Type
                      FROM Managers 
                      GROUP BY plyrMgr")
# Then get the number of manager who is/was player
dbGetQuery(baseball, "SELECT COUNT(DISTINCT playerID) AS count
                      FROM(
                        SELECT *
                        FROM Managers
                        LEFT JOIN Master ON Managers.playerID = Master.playerID
                      )
                      WHERE debut != 0")

# Using R to validate
Managers <- dbGetQuery(baseball, "SELECT * FROM Managers")
length(unique(Managers$playerID)) # we get 682 here, but we cannot use this number as total
plyrMgr_yes <- Managers[which(Managers$plyrMgr == 'Y'), ]
length(unique(plyrMgr_yes$playerID)) 
# since some of players were managers in some years, while not in other years. So we regard the player as a manager if there exist "Y" in the plyrMgr field
plyrMgr_no <- Managers[which(Managers$plyrMgr == 'N'), ]
length(unique(plyrMgr_no$playerID))
#Then get the number of manager who is/was player
total_number_manager <- merge(Managers, Master, by.x = "playerID", by.y = "playerID", all.x = TRUE)
total_number_manager <- total_number_manager[which(total_number_manager$debut != 0), ]
length(unique(total_number_manager$playerID))

# Question 4: What team won the World Series in 2010? Include the name of the team, the league and division.
dbGetQuery(baseball, "SELECT yearID AS Year, teamID AS Team, name AS Name, lgID AS League, divID AS Division
                      FROM Teams
                      WHERE yearID = '2010' AND WSWin = 'Y'")

# Using R to validate
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
WS2010_win <- Teams[which(Teams$yearID == 2010 & Teams$WSWin == "Y"), ]
WS2010_win[1, c("yearID", "teamID", "name", "lgID", "divID")]

# Question 5: Compute the table of World Series winners for all years, again with the name of the team, league and division.
# From 1969, the leagues were split into two divisions of East and West
dbGetQuery(baseball, "SELECT yearID AS Year, teamID AS TeamWin, name AS NameWin, lgIDwinner AS LeagueWin, divID AS DivisionWin
                      FROM(
                        SELECT * FROM Teams
                        LEFT JOIN SeriesPost
                        ON Teams.yearID = SeriesPost.yearID
                        WHERE round = 'WS'
                      )
                      WHERE WSWin = 'Y' OR teamID = teamIDwinner OR franchID = teamIDwinner OR teamIDBR = teamIDwinner OR teamIDlahman45 = teamIDwinner OR teamIDretro = teamIDwinner
                      GROUP BY yearID")

# Using R to validate
SeriesPost <- dbGetQuery(baseball, "SELECT * FROM SeriesPost")
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
series_ws <- SeriesPost[which(SeriesPost$round == "WS"), ]
merge_team_series <- merge(Teams, series_ws, by.x = "yearID", by.y = "yearID", all.X = TRUE)
WS_win <- merge_team_series[which(merge_team_series$WSWin == "Y" | 
                                    merge_team_series$teamID == merge_team_series$teamIDwinner | 
                                    merge_team_series$franchID == merge_team_series$teamIDwinner | 
                                    merge_team_series$teamIDBR == merge_team_series$teamIDwinner |
                                    merge_team_series$teamIDlahman45 == merge_team_series$teamIDwinner | 
                                    merge_team_series$teamIDretro == merge_team_series$teamIDwinner), ]
WS_win[, c("yearID", "teamID", "name", "lgID", "divID")]

# Question 6: What team lost the World Series each year? Again, include the name of the team, league and division.
dbGetQuery(baseball, "SELECT yearID AS Year, teamIDloser AS TeamLose, name AS NameLose, lgIDloser AS LeagueLose, divID AS DivisionLose 
                      FROM(
                        SELECT * FROM Teams
                        LEFT JOIN SeriesPost
                        ON Teams.yearID = SeriesPost.yearID
                        WHERE round = 'WS'
                      )
                      WHERE teamID = teamIDloser OR franchID = teamIDloser OR teamIDBR = teamIDloser OR teamIDlahman45 = teamIDloser OR teamIDretro = teamIDloser
                      GROUP BY yearID")

# Using R to validate
SeriesPost <- dbGetQuery(baseball, "SELECT * FROM SeriesPost")
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
series_ws <- SeriesPost[which(SeriesPost$round == "WS"), ]
merge_team_series <- merge(Teams, series_ws, by.x = "yearID", by.y = "yearID", all.X = TRUE)
WS_lose <- merge_team_series[which(merge_team_series$teamID == merge_team_series$teamIDloser | 
                                     merge_team_series$franchID == merge_team_series$teamIDloser | 
                                     merge_team_series$teamIDBR == merge_team_series$teamIDloser |
                                     merge_team_series$teamIDlahman45 == merge_team_series$teamIDloser | 
                                     merge_team_series$teamIDretro == merge_team_series$teamIDloser), ]
WS_lose[, c("yearID", "teamIDloser", "name", "lgID", "divID")]

# Question 7: Do you see a relationship between the number of games won in a season and winning the World Series?
relationship <- dbGetQuery(baseball, "SELECT W AS numberWin
                                      FROM(
                                        SELECT * FROM Teams
                                        LEFT JOIN SeriesPost
                                        ON Teams.yearID = SeriesPost.yearID
                                        WHERE round = 'WS'
                                      )
                                      WHERE WSWin = 'Y' OR teamID = teamIDwinner OR franchID = teamIDwinner OR teamIDBR = teamIDwinner OR teamIDlahman45 = teamIDwinner OR teamIDretro = teamIDwinner
                                      GROUP BY yearID")
hist(relationship$numberWin, xlab = "Number of Win", main = "Number of Win in a season for the Champion of the World Series")

# Using R to validate
SeriesPost <- dbGetQuery(baseball, "SELECT * FROM SeriesPost")
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
series_ws <- SeriesPost[which(SeriesPost$round == "WS"), ]
merge_team_series <- merge(Teams, series_ws, by.x = "yearID", by.y = "yearID", all.X = TRUE)
WS_win <- merge_team_series[which(merge_team_series$WSWin == "Y" | 
                                    merge_team_series$teamID == merge_team_series$teamIDwinner | 
                                    merge_team_series$franchID == merge_team_series$teamIDwinner |
                                    merge_team_series$teamIDBR == merge_team_series$teamIDwinner |
                                    merge_team_series$teamIDlahman45 == merge_team_series$teamIDwinner | 
                                    merge_team_series$teamIDretro == merge_team_series$teamIDwinner), ]
hist(WS_win$W, xlab = "Number of Win", main = "Number of Win in a season for the Champion of the World Series")

# Question 8: In 2003, what were the three highest salaries? (We refer here to unique salaries, i.e., there may be several players getting the exact same amount.) Find the players who got any of these 3 salaries with all of their details?
# First find out the three highest salaries
dbGetQuery(baseball, "SELECT DISTINCT salary
                      FROM Salaries
                      WHERE yearID = '2003'
                      ORDER BY salary DESC LIMIT 3")
# Then I will find the players who got those salaries with some useful information
dbGetQuery(baseball, "SELECT Salaries.yearID, Salaries.playerID, Master.nameFirst, Master.nameLast, Master.birthYear, Master.birthCountry, Master.weight, Master.height, Salaries.teamID, Teams.name, Salaries.lgID, Salaries.salary, Appearances.G_all, Teams.W, Teams.WSWin, Teams.LgWin
                      FROM Salaries
                      LEFT JOIN Master ON Salaries.playerID = Master.playerID
                      LEFT JOIN Appearances ON Master.playerID = Appearances.playerID AND Salaries.yearID = Appearances.yearID
                      LEFT JOIN Teams ON Teams.teamID = Appearances.teamID AND Salaries.yearID = Teams.yearID
                      WHERE Salaries.yearId = '2003' AND (Salaries.salary = '22000000' OR Salaries.salary = '20000000' OR Salaries.salary = '18700000')
                      ORDER BY Salaries.salary DESC")

# Using R to validate
Salaries <- dbGetQuery(baseball, "SELECT * FROM Salaries")
Master <- dbGetQuery(baseball, "SELECT * FROM Master")
Appearances <- dbGetQuery(baseball, "SELECT * FROM Appearances")
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
salary_2003 <- Salaries[which(Salaries$yearID == '2003'), ]
unique_salary_2003 <- dplyr::distinct(salary_2003, salary)
unique_salary_2003[order(unique_salary_2003, decreasing = TRUE)[1:3], ]
master_sub <- Master[, c("playerID", "birthYear", "birthCountry", "nameFirst", "nameLast", "weight", "height")]
merge1 <- merge(salary_2003, master_sub, by.x = "playerID", by.y = "playerID", all.x = TRUE)
appearances_sub <- Appearances[which(Appearances$yearID == "2003"), c("playerID", "G_all")]
merge2 <- merge(merge1, appearances_sub, by.x = "playerID", by.y = "playerID", all.x = TRUE)
team_sub <- Teams[which(Teams$yearID == "2003"), c("teamID", "name", "W", "WSWin", "LgWin")]
player_information <- merge(merge2, team_sub, by.x = "teamID", by.y = "teamID", all.x = TRUE)
player_information[order(player_information$salary, decreasing = TRUE)[1: 3], ]

# Question 9: For 2010, compute the total payroll of each of the different teams. Next compute the team payrolls for all years in the database for which we have salary information. Display these in a plot.
# First compute the total payroll of each teams in 2010
payroll_2010_sql <- dbGetQuery(baseball, "SELECT teamID AS team, SUM(salary) AS total_payroll
                                          FROM Salaries
                                          WHERE yearID = '2010'
                                          GROUP BY teamID")
ggplot(payroll_2010_sql, aes(x = team, y = total_payroll)) + 
  geom_point() + 
  coord_flip()
# Then calculate the team payroll for all years and make plot
payroll_all_sql <- dbGetQuery(baseball, "SELECT yearID AS year, teamID AS team, SUM(salary) AS total_payroll
                                         FROM Salaries
                                         GROUP BY teamID, yearID")
ggplot(payroll_all_sql, aes(x = year, y = total_payroll, color = team)) +
  geom_point() + 
  guides(col = guide_legend(nrow = 12))

# Using R to validate
Salaries <- dbGetQuery(baseball, "SELECT * FROM Salaries")
payroll_2010_r <- Salaries[which(Salaries$yearID == "2010"), ]
payroll_2010_r <- payroll_2010_r %>% 
  group_by(teamID) %>% 
  summarise(salary = sum(salary))
ggplot(payroll_2010_r, aes(x = teamID, y = salary)) + 
  geom_point() + 
  coord_flip()
payroll_all_r <- Salaries %>% 
  group_by(teamID, yearID) %>% 
  summarise(salary = sum(salary))
ggplot(payroll_all_r, aes(x = yearID, y = salary, color = teamID)) +
  geom_point() + 
  guides(col = guide_legend(nrow = 12))

# Question 10: Which player has hit the most home runs? Show the number per year.
# It appears to be a "greatest N per group" problem. First get the maximum HR per year and using them as a derived table, join them back to the source to get the rows matching the maximums.
dbGetQuery(baseball, "SELECT B.playerID, Master.nameFirst, Master.nameLast, B.yearID, Y.max_HR
                      FROM Batting AS B
                      INNER JOIN(
                        SELECT playerID, yearID, MAX(HR) AS max_HR
                        FROM Batting
                        GROUP BY yearID
                      ) AS Y ON B.yearID = Y.yearID AND B.HR = Y.max_HR
                      LEFT JOIN Master ON B.playerID = Master.playerID
                      ORDER BY B.yearID")

# Using R to validate
Batting <- dbGetQuery(baseball, "SELECT * FROM Batting")
Master <- dbGetQuery(baseball, "SELECT * FROM Master")
batting_with_name <- merge(Batting, Master[, c("playerID", "nameFirst", "nameLast")], by.x = "playerID", by.y = "playerID", all.x = TRUE)
batting_with_name %>% 
  replace(is.na(.), 0) %>% 
  select(playerID, nameFirst, nameLast, yearID, HR) %>% 
  group_by(yearID) %>% 
  filter(HR == max(HR)) %>%
  arrange(yearID)

# Question 11: Has the distribution of home runs for players increased over the years?
# We calculate the total home runs in different years
HR_year_sql <- dbGetQuery(baseball, "SELECT yearID as year, SUM(HR) AS total_HR
                                     FROM Batting
                                     GROUP BY yearID")
ggplot(HR_year_sql, aes(x = year, y = total_HR)) + 
  geom_point() + 
  geom_smooth()

# Using R to validate
Batting <- dbGetQuery(baseball, "SELECT * FROM Batting")
HR_year_r <- Batting %>%
  replace(is.na(.), 0) %>%
  group_by(yearID) %>%
  summarise(HR = sum(HR))
ggplot(HR_year_r, aes(x = yearID, y = HR)) + 
  geom_point() + 
  geom_smooth()

# Question 12: Are certain baseball parks better for hitting home runs?
# We calculate the total HRs in different baseball parks over the years, and see the top 10 parks
dbGetQuery(baseball, "SELECT park, SUM(HR) AS total_HR
                      FROM Teams
                      GROUP BY park
                      ORDER BY SUM(HR) DESC LIMIT 10")
# Then we calculate top 5 parks in each year
top_five_parks_sql <- dbGetQuery(baseball, "WITH Top_Five AS(
                                              SELECT yearID AS year, park, HR, ROW_NUMBER()
                                              OVER(
                                                PARTITION BY yearID
                                                ORDER BY HR DESC
                                              ) AS Row_Number
                                              FROM Teams
                                            )
                                            SELECT year, park, HR
                                            FROM Top_Five
                                            WHERE Row_Number <= 5")
table(top_five_parks_sql$park)[order(table(top_five_parks_sql$park), decreasing = TRUE)[1: 5]]

# Using R to validate
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
# First calculate the total HRs of top 10 parks over the years
Teams %>%
  group_by(park) %>%
  summarise(HR = sum(HR)) %>%
  arrange(desc(HR)) %>%
  top_n(10)
# Then we calculate top 5 parks in each year, and find the top 5 frequent parks
top_five_parks_r <- Teams %>% 
  select(yearID, park, HR) %>%
  arrange(desc(HR)) %>%
  group_by(yearID) %>%
  slice(1: 5)
table(top_five_parks_r$park)[order(table(top_five_parks_r$park), decreasing = TRUE)[1: 5]]

# Question 13: How many games do pitchers start in a season? Plot this against games finished in a season.
pitcher_sf_sql <- dbGetQuery(baseball, "SELECT yearID as Season, SUM(GS) AS Game_Started, SUM(GF) AS Game_Finished
                                        FROM Pitching
                                        GROUP BY yearID")
# Since there are NAs in the first 5 years of the game finished field, we regard those as 0
pitcher_sf_sql[is.na(pitcher_sf_sql)] = 0
ggplot(pitcher_sf_sql, aes(x = Game_Started, y = Game_Finished, color = Season)) + 
  geom_point()

# Using R to validate
Pitching <- dbGetQuery(baseball, "SELECT * FROM Pitching")
pitcher_sf_r <- Pitching %>%
  select(yearID, GS, GF) %>%
  replace(is.na(.), 0) %>% 
  group_by(yearID) %>%
  summarise(GS = sum(GS), GF = sum(GF))
ggplot(pitcher_sf_r, aes(x = GS, y = GF, color = yearID)) + 
  geom_point()

# Question 14 : How many games do pitchers win in a season?
# I also use Pitching table to calculate, not include the postseason game
pitcher_win_sql <- dbGetQuery(baseball, "SELECT yearID AS Season, SUM(W) AS Number_Win
                                         FROM Pitching
                                         GROUP BY yearID")
ggplot(pitcher_win_sql, aes(x = Season, y = Number_Win)) + 
  geom_point()

# Using R to validate
Pitching <- dbGetQuery(baseball, "SELECT * FROM Pitching")
pitcher_win_r <- Pitching %>%
  select(yearID, W) %>%
  replace(is.na(.), 0) %>%
  group_by(yearID) %>%
  summarise(W = sum(W))
ggplot(pitcher_win_r, aes(x = yearID, y = W)) + 
  geom_point()

# Question 15: What are the top ten collegiate producers of major league baseball players? How many colleges are represented in the database?
# First compute the number of colleges that present in the database
dbGetQuery(baseball, "SELECT COUNT(DISTINCT schoolID)
                      FROM Schools")
# Then compute the top 10 colleges that produce mlb players
dbGetQuery(baseball, "SELECT schoolName, COUNT(schoolName) AS Number_Players
                      FROM SchoolsPlayers
                      LEFT JOIN Schools ON SchoolsPlayers.schoolID = Schools.schoolID
                      GROUP BY SchoolsPlayers.schoolID
                      ORDER BY Number_Players DESC
                      LIMIT 10")

# Using R to validate
Schools <- dbGetQuery(baseball, "SELECT * FROM Schools")
SchoolsPlayers <- dbGetQuery(baseball, "SELECT * FROM SchoolsPlayers")
length(unique(Schools$schoolID))
merge_school <- merge(SchoolsPlayers, Schools, by.x = "schoolID", by.y = "schoolID", all.x = TRUE)
merge_school %>%
  group_by(schoolName) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)

# Question 16: What players have pitched in the post season and also hit a home run in their career?
dbGetQuery(baseball, "SELECT DISTINCT PitchingPost.playerID, Master.nameFirst, Master.nameLast
                      FROM PitchingPost
                      LEFT JOIN Master ON PitchingPost.playerID = Master.playerID
                      LEFT JOIN Batting ON PitchingPost.playerID = Batting.playerID AND PitchingPost.yearID = Batting.yearID AND PitchingPost.teamID = Batting.teamID
                      LEFT JOIN BattingPost ON PitchingPost.playerID = BattingPost.playerID AND PitchingPost.yearID = BattingPost.yearID AND PitchingPost.teamID = BattingPost.teamID AND PitchingPost.round = BattingPost.round
                      WHERE Batting.HR > 0 OR BattingPost.HR > 0")

# Using R to validate
PitchingPost <- dbGetQuery(baseball, "SELECT * FROM PitchingPost")
Master <- dbGetQuery(baseball, "SELECT * FROM Master")
Batting <- dbGetQuery(baseball, "SELECT * FROM Batting")
BattingPost <- dbGetQuery(baseball, "SELECT * FROM BattingPost")
Master_sub <- Master[, c("playerID", "nameFirst", "nameLast")]
merge_1 <- merge(PitchingPost, Master_sub, by.x = "playerID", by.y = "playerID", all.x = TRUE)
Batting_sub <- Batting[, c("playerID", "yearID", "teamID", "HR")]
merge_2 <- merge(merge_1, Batting_sub, by.x = c("playerID", "yearID", "teamID"), by.y = c("playerID", "yearID", "teamID"), all.x = TRUE)
BattingPost_sub <- BattingPost[, c("playerID", "yearID", "teamID", "round", "HR")]
merge_3 <- merge(merge_2, BattingPost_sub, by.x = c("playerID", "yearID", "teamID", "round"), by.y = c("playerID", "yearID", "teamID", "round"), all.x = TRUE)
pitch_HR <- merge_3[which(merge_3$HR > 0 | merge_3$HR.y > 0), c("playerID", "nameFirst", "nameLast")]
pitch_HR[!duplicated(pitch_HR$playerID), ]

# Question 17 : How many players are there in each year, from 2000 to 2013? Do all teams have the same number of players?
# We use Appearances table since it contains all the players in each team from 1871 to 2013
# First calculate the number of players in each year from 2000 to 2013
dbGetQuery(baseball, "SELECT yearID AS year, COUNT(DISTINCT playerID) AS count
                      FROM Appearances
                      WHERE yearID >= '2000'
                      GROUP BY yearID")
# Then check whether all teams have the same number of players
check_number_sql <- dbGetQuery(baseball, "SELECT yearID AS year, teamID AS team, COUNT(DISTINCT playerID) AS count
                                          FROM Appearances
                                          WHERE yearID >= '2000'
                                          GROUP BY teamID, yearID")
ggplot(check_number_sql, aes(x = team, y = count, color = as.factor(year))) + 
  geom_point()+
  coord_flip()

# Using R to validate
Appearances <- dbGetQuery(baseball, "SELECT * FROM Appearances")
player_00_13 <- Appearances[which(Appearances$yearID >= "2000"), ]
player_00_13 %>%
  select(yearID, playerID) %>%
  group_by(yearID) %>%
  distinct(playerID, .keep_all = TRUE) %>%
  count()

check_number_r <- player_00_13 %>%
  select(yearID, playerID, teamID) %>%
  group_by(yearID, teamID) %>%
  distinct(playerID, .keep_all = TRUE) %>%
  count()
ggplot(check_number_r, aes(x = teamID, y = n, color = as.factor(yearID))) + 
  geom_point()+
  coord_flip()

# Question 18: Do players who hit more home runs receive higher salaries?
# Since we cannot directly compare salaries in different year since the value of dollar is different over the years. So we compare the salaries in the same year.
# We choose 1985-1987, 1998-2000, 2011-2013 as example to see the result
salary_8587_sql <- dbGetQuery(baseball, "SELECT Salaries.yearID, Salaries.salary, Batting.HR
                                         FROM Salaries
                                         LEFT JOIN Batting ON Salaries.yearID = Batting.yearID AND Salaries.playerID = Batting.playerID AND Salaries.teamID = Batting.teamID
                                         WHERE Salaries.yearID >= '1985' AND Salaries.yearID <= '1987'")
ggplot(salary_8587_sql, aes(x = HR, y = salary)) + 
  geom_point() + 
  geom_smooth()

salary_9800_sql <- dbGetQuery(baseball, "SELECT Salaries.yearID, Salaries.salary, Batting.HR
                                         FROM Salaries
                                         LEFT JOIN Batting ON Salaries.yearID = Batting.yearID AND Salaries.playerID = Batting.playerID AND Salaries.teamID = Batting.teamID
                                         WHERE Salaries.yearID >= '1998' AND Salaries.yearID <= '2000'")
ggplot(salary_9800_sql, aes(x = HR, y = salary)) + 
  geom_point() + 
  geom_smooth()

salary_1113_sql <- dbGetQuery(baseball, "SELECT Salaries.yearID, Salaries.salary, Batting.HR
                                         FROM Salaries
                                         LEFT JOIN Batting ON Salaries.yearID = Batting.yearID AND Salaries.playerID = Batting.playerID AND Salaries.teamID = Batting.teamID
                                         WHERE Salaries.yearID >= '2011' AND Salaries.yearID <= '2013'")
ggplot(salary_1113_sql, aes(x = HR, y = salary)) + 
  geom_point() + 
  geom_smooth()

# Using R to validate
Batting <- dbGetQuery(baseball, "SELECT * FROM Batting")
Salaries <- dbGetQuery(baseball, "SELECT * FROM Salaries")
Batting_sub1 <- Batting[, c("playerID", "yearID", "teamID", "HR")]
salary_8587_r <- Salaries[which(Salaries$yearID >= "1985" & Salaries$yearID <= "1987"), ]
Merge1 <- merge(salary_8587_r, Batting_sub1, by.x = c("playerID", "yearID", "teamID"), by.y = c("playerID", "yearID", "teamID"), all.x = TRUE)
ggplot(Merge1, aes(x = HR, y = salary)) + 
  geom_point() + 
  geom_smooth()

salary_9800_r <- Salaries[which(Salaries$yearID >= "1998" & Salaries$yearID <= "2000"), ]
Merge2 <- merge(salary_9800_r, Batting_sub1, by.x = c("playerID", "yearID", "teamID"), by.y = c("playerID", "yearID", "teamID"), all.x = TRUE)
ggplot(Merge2, aes(x = HR, y = salary)) + 
  geom_point() + 
  geom_smooth()

salary_1113_r <- Salaries[which(Salaries$yearID >= "2011" & Salaries$yearID <= "2013"), ]
Merge3 <- merge(salary_1113_r, Batting_sub1, by.x = c("playerID", "yearID", "teamID"), by.y = c("playerID", "yearID", "teamID"), all.x = TRUE)
ggplot(Merge3, aes(x = HR, y = salary)) + 
  geom_point() + 
  geom_smooth()

# Question 19: Explore the change in salary over time. Use a plot. Identify the teams that won the world series or league on the plot. How does salary relate to winning the league and/or world series.
# We compute the total salaries in a team over the years
salary_win_sql <- dbGetQuery(baseball, "SELECT Salaries.yearID, Salaries.teamID, Teams.LgWin, Teams.WSWin, SUM(Salaries.salary) AS salary
                                        FROM Salaries
                                        LEFT JOIN Teams ON Salaries.yearID = Teams.yearID AND Salaries.teamID = Teams.teamID AND Salaries.lgID = Teams.lgID
                                        GROUP BY Salaries.yearID, Salaries.teamID")
# The 1994 MLB World Series and the League Championship Series were canceled due to the strike by the MLB Players Association
salary_win_sql[is.na(salary_win_sql)] <- "N"
ggplot(salary_win_sql, aes(x = yearID, y = salary, color = teamID, shape = LgWin, size = WSWin)) + 
  geom_point() + 
  theme(legend.direction = "horizontal", legend.box = "vertical") +
  guides(col = guide_legend(nrow = 12))

# Using R to validate
Salaries <- dbGetQuery(baseball, "SELECT * FROM Salaries")
Teams <- dbGetQuery(baseball, "SELECT * FROM Teams")
Teams_sub <- Teams[, c("yearID", "teamID", "lgID", "LgWin", "WSWin")]
salary_win_r <- merge(Salaries, Teams_sub, by.x = c("yearID", "teamID", "lgID"), by.y = c("yearID", "teamID", "lgID"), all.x = TRUE)
salary_win_r <- salary_win_r %>%
  select(yearID, teamID, LgWin, WSWin, salary) %>%
  group_by(yearID, teamID, LgWin, WSWin) %>%
  summarise(salary = sum(salary))
salary_win_r[is.na(salary_win_r)] <- "N"  
ggplot(salary_win_r, aes(x = yearID, y = salary, color = teamID, shape = LgWin, size = WSWin)) + 
  geom_point() + 
  theme(legend.direction = "horizontal", legend.box = "vertical") +
  guides(col = guide_legend(nrow = 12))

# Question 20: How many people are there in each categories? For inducted players who were voted by BBWAA (Baseball Writers' Association of America), how many percentage of vote did they receive?
# For this question I want to explore the voting in the Hall of Fame. I want to know the type of HoF, and the percentage of vote a player receive to be inducted in the HoF.
# First compute the number of people in each categories
dbGetQuery(baseball, "SELECT category, COUNT(category) AS count
                      FROM HallofFame
                      GROUP BY category")
# Then compute the percentage of voted received
HoF_percentage_sql <- dbGetQuery(baseball, "SELECT H.playerID AS player, Master.nameFirst AS first_name, Master.nameLast AS last_name, H.yearid AS year, 100.0 * H.votes / H.ballots AS percentage_of_votes_received, 100.0* H.needed / H.ballots AS percentage_of_votes_needed, 100.0 * (H.votes-H.needed) / H.ballots AS percentage_exceeded
                                            FROM HallofFame AS H
                                            LEFT JOIN Master ON H.playerID = Master.playerID
                                            WHERE H.votedBy = 'BBWAA' AND H.category = 'Player' AND H.inducted = 'Y'")

ggplot()+ 
  geom_point(data = HoF_percentage_sql, aes(x = year, y = percentage_of_votes_received, colour = "Received"))+ 
  geom_point(data = HoF_percentage_sql, aes(x = year, y = percentage_of_votes_needed, colour = "Needed"))+ 
  scale_color_manual(name = "Type of Percentage", values = c(Received = "red", Needed = "blue"))+ 
  ylab("Percentage of Vote")

# Using R to validate
HallOfFame <- dbGetQuery(baseball, "SELECT * FROM HallOfFame")
Master <- dbGetQuery(baseball, "SELECT * FROM Master")
table(HallOfFame$category)

sub_master <- Master[, c("playerID", "nameFirst", "nameLast")]
HoF_percentage_r <- merge(HallOfFame, sub_master, by.x = "playerID", by.y = "playerID", all.x = TRUE)
HoF_percentage_r <- HoF_percentage_r[which(HoF_percentage_r$votedBy == "BBWAA" & HoF_percentage_r$category == "Player" & HoF_percentage_r$inducted == "Y"), ]
HoF_percentage_r$percentage_of_votes_received <- 100.0 * HoF_percentage_r$votes / HoF_percentage_r$ballots
HoF_percentage_r$percentage_of_votes_needed <- 100.0 * HoF_percentage_r$needed / HoF_percentage_r$ballots
HoF_percentage_r$percentage_exceeded <- HoF_percentage_r$percentage_of_votes_received - HoF_percentage_r$percentage_of_votes_needed
ggplot()+ 
  geom_point(data = HoF_percentage_r, aes(x = yearid, y = percentage_of_votes_received, colour = "Received"))+ 
  geom_point(data = HoF_percentage_r, aes(x = yearid, y = percentage_of_votes_needed, colour = "Needed"))+ 
  scale_color_manual(name = "Type of Percentage", values = c(Received = "red", Needed = "blue"))+
  xlab("year")+
  ylab("Percentage of Vote")