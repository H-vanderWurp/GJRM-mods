
dat <- read.csv2("bundesliga.csv", sep=";", header=T)

dat$PreTabelleHome <- integer(dim(dat)[1])
dat$PreTabelleGuest <- integer(dim(dat)[1])
dat$AufsteigerHome <- logical(dim(dat)[1])
dat$AufsteigerGuest <- logical(dim(dat)[1])

######### ADD Aufsteiger und vorherige Platzierung #########

## Laufende Saison 2010/2011
dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2010] <- 1
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2010] <- 1
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2010] <- 2
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2010] <- 2
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2010] <- 3
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2010] <- 3
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2010] <- 4
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2010] <- 4
dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2010] <- 5
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2010] <- 5
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2010] <- 6
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2010] <- 6
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2010] <- 7
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2010] <- 7
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2010] <- 8
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2010] <- 8
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2010] <- 9
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2010] <- 9
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2010] <- 10
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2010] <- 10
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2010] <- 11
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2010] <- 11
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2010] <- 12
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2010] <- 12
dat$PreTabelleHome[dat$Home == "Koeln" & dat$SeasonFrom==2010] <- 13
dat$PreTabelleGuest[dat$Guest == "Koeln" & dat$SeasonFrom==2010] <- 13
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2010] <- 14
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2010] <- 14
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2010] <- 15
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2010] <- 15
dat$PreTabelleHome[dat$Home == "Nuernberg" & dat$SeasonFrom==2010] <- 16
dat$PreTabelleGuest[dat$Guest == "Nuernberg" & dat$SeasonFrom==2010] <- 16
dat$PreTabelleHome[dat$Home == "Klautern" & dat$SeasonFrom==2010] <- 17
dat$PreTabelleGuest[dat$Guest == "Klautern" & dat$SeasonFrom==2010] <- 17
dat$PreTabelleHome[dat$Home == "St. Pauli" & dat$SeasonFrom==2010] <- 18
dat$PreTabelleGuest[dat$Guest == "St. Pauli" & dat$SeasonFrom==2010] <- 18

dat$AufsteigerHome[dat$Home == "Klautern" & dat$SeasonFrom==2010] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Klautern" & dat$SeasonFrom==2010] <- TRUE
dat$AufsteigerHome[dat$Home == "St. Pauli" & dat$SeasonFrom==2010] <- TRUE
dat$AufsteigerGuest[dat$Guest == "St. Pauli" & dat$SeasonFrom==2010] <- TRUE

## Laufende Saison 2011/2012

dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2011] <- 1
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2011] <- 1
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2011] <- 2
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2011] <- 2
dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2011] <- 3
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2011] <- 3
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2011] <- 4
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2011] <- 4
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2011] <- 5
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2011] <- 5
dat$PreTabelleHome[dat$Home == "Nuernberg" & dat$SeasonFrom==2011] <- 6
dat$PreTabelleGuest[dat$Guest == "Nuernberg" & dat$SeasonFrom==2011] <- 6
dat$PreTabelleHome[dat$Home == "Klautern" & dat$SeasonFrom==2011] <- 7
dat$PreTabelleGuest[dat$Guest == "Klautern" & dat$SeasonFrom==2011] <- 7
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2011] <- 8
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2011] <- 8
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2011] <- 9
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2011] <- 9
dat$PreTabelleHome[dat$Home == "Koeln" & dat$SeasonFrom==2011] <- 10
dat$PreTabelleGuest[dat$Guest == "Koeln" & dat$SeasonFrom==2011] <- 10
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2011] <- 11
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2011] <- 11
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2011] <- 12
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2011] <- 12
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2011] <- 13
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2011] <- 13
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2011] <- 14
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2011] <- 14
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2011] <- 15
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2011] <- 15
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2011] <- 16
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2011] <- 16
dat$PreTabelleHome[dat$Home == "Hertha" & dat$SeasonFrom==2011] <- 17
dat$PreTabelleGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2011] <- 17
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2011] <- 18
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2011] <- 18

dat$AufsteigerHome[dat$Home == "Hertha" & dat$SeasonFrom==2011] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2011] <- TRUE
dat$AufsteigerHome[dat$Home == "Augsburg" & dat$SeasonFrom==2011] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2011] <- TRUE

## Laufende Saison 2012/2013

dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2012] <- 1
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2012] <- 1
dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2012] <- 2
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2012] <- 2
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2012] <- 3
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2012] <- 3
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2012] <- 4
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2012] <- 4
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2012] <- 5
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2012] <- 5
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2012] <- 6
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2012] <- 6
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2012] <- 7
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2012] <- 7
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2012] <- 8
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2012] <- 8
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2012] <- 9
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2012] <- 9
dat$PreTabelleHome[dat$Home == "Nuernberg" & dat$SeasonFrom==2012] <- 10
dat$PreTabelleGuest[dat$Guest == "Nuernberg" & dat$SeasonFrom==2012] <- 10
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2012] <- 11
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2012] <- 11
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2012] <- 12
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2012] <- 12
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2012] <- 13
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2012] <- 13
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2012] <- 14
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2012] <- 14
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2012] <- 15
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2012] <- 15
dat$PreTabelleHome[dat$Home == "Fuerth" & dat$SeasonFrom==2012] <- 16
dat$PreTabelleGuest[dat$Guest == "Fuerth" & dat$SeasonFrom==2012] <- 16
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2012] <- 17
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2012] <- 17
dat$PreTabelleHome[dat$Home == "Duesseldorf" & dat$SeasonFrom==2012] <- 18
dat$PreTabelleGuest[dat$Guest == "Duesseldorf" & dat$SeasonFrom==2012] <- 18

dat$AufsteigerHome[dat$Home == "Fuerth" & dat$SeasonFrom==2012] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Fuerth" & dat$SeasonFrom==2012] <- TRUE
dat$AufsteigerHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2012] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2012] <- TRUE
dat$AufsteigerHome[dat$Home == "Duesseldorf" & dat$SeasonFrom==2012] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Duesseldorf" & dat$SeasonFrom==2012] <- TRUE

## Laufende Saison 2013/2014

dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2013] <- 1
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2013] <- 1
dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2013] <- 2
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2013] <- 2
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2013] <- 3
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2013] <- 3
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2013] <- 4
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2013] <- 4
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2013] <- 5
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2013] <- 5
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2013] <- 6
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2013] <- 6
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2013] <- 7
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2013] <- 7
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2013] <- 8
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2013] <- 8
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2013] <- 9
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2013] <- 9
dat$PreTabelleHome[dat$Home == "Nuernberg" & dat$SeasonFrom==2013] <- 10
dat$PreTabelleGuest[dat$Guest == "Nuernberg" & dat$SeasonFrom==2013] <- 10
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2013] <- 11
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2013] <- 11
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2013] <- 12
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2013] <- 12
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2013] <- 13
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2013] <- 13
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2013] <- 14
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2013] <- 14
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2013] <- 15
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2013] <- 15
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2013] <- 16
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2013] <- 16
dat$PreTabelleHome[dat$Home == "Hertha" & dat$SeasonFrom==2013] <- 17
dat$PreTabelleGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2013] <- 17
dat$PreTabelleHome[dat$Home == "Braunschweig" & dat$SeasonFrom==2013] <- 18
dat$PreTabelleGuest[dat$Guest == "Braunschweig" & dat$SeasonFrom==2013] <- 18

dat$AufsteigerHome[dat$Home == "Hertha" & dat$SeasonFrom==2013] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2013] <- TRUE
dat$AufsteigerHome[dat$Home == "Braunschweig" & dat$SeasonFrom==2013] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Braunschweig" & dat$SeasonFrom==2013] <- TRUE

## Laufende Saison 2014/2015

dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2014] <- 1
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2014] <- 1
dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2014] <- 2
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2014] <- 2
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2014] <- 3
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2014] <- 3
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2014] <- 4
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2014] <- 4
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2014] <- 5
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2014] <- 5
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2014] <- 6
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2014] <- 6
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2014] <- 7
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2014] <- 7
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2014] <- 8
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2014] <- 8
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2014] <- 9
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2014] <- 9
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2014] <- 10
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2014] <- 10
dat$PreTabelleHome[dat$Home == "Hertha" & dat$SeasonFrom==2014] <- 11
dat$PreTabelleGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2014] <- 11
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2014] <- 12
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2014] <- 12
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2014] <- 13
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2014] <- 13
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2014] <- 14
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2014] <- 14
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2014] <- 15
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2014] <- 15
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2014] <- 16
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2014] <- 16
dat$PreTabelleHome[dat$Home == "Koeln" & dat$SeasonFrom==2014] <- 17
dat$PreTabelleGuest[dat$Guest == "Koeln" & dat$SeasonFrom==2014] <- 17
dat$PreTabelleHome[dat$Home == "Paderborn" & dat$SeasonFrom==2014] <- 18
dat$PreTabelleGuest[dat$Guest == "Paderborn" & dat$SeasonFrom==2014] <- 18

dat$AufsteigerHome[dat$Home == "Koeln" & dat$SeasonFrom==2014] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Koeln" & dat$SeasonFrom==2014] <- TRUE
dat$AufsteigerHome[dat$Home == "Paderborn" & dat$SeasonFrom==2014] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Paderborn" & dat$SeasonFrom==2014] <- TRUE

## Laufende Saison 2015/2016

dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2015] <- 1
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2015] <- 1
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2015] <- 2
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2015] <- 2
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2015] <- 3
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2015] <- 3
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2015] <- 4
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2015] <- 4
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2015] <- 5
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2015] <- 5
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2015] <- 6
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2015] <- 6
dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2015] <- 7
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2015] <- 7
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2015] <- 8
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2015] <- 8
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2015] <- 9
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2015] <- 9
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2015] <- 10
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2015] <- 10
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2015] <- 11
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2015] <- 11
dat$PreTabelleHome[dat$Home == "Koeln" & dat$SeasonFrom==2015] <- 12
dat$PreTabelleGuest[dat$Guest == "Koeln" & dat$SeasonFrom==2015] <- 12
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2015] <- 13
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2015] <- 13
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2015] <- 14
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2015] <- 14
dat$PreTabelleHome[dat$Home == "Hertha" & dat$SeasonFrom==2015] <- 15
dat$PreTabelleGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2015] <- 15
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2015] <- 16
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2015] <- 16
dat$PreTabelleHome[dat$Home == "Ingolstadt" & dat$SeasonFrom==2015] <- 17
dat$PreTabelleGuest[dat$Guest == "Ingolstadt" & dat$SeasonFrom==2015] <- 17
dat$PreTabelleHome[dat$Home == "Darmstadt" & dat$SeasonFrom==2015] <- 18
dat$PreTabelleGuest[dat$Guest == "Darmstadt" & dat$SeasonFrom==2015] <- 18

dat$AufsteigerHome[dat$Home == "Ingolstadt" & dat$SeasonFrom==2015] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Ingolstadt" & dat$SeasonFrom==2015] <- TRUE
dat$AufsteigerHome[dat$Home == "Darmstadt" & dat$SeasonFrom==2015] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Darmstadt" & dat$SeasonFrom==2015] <- TRUE

## Laufende Saison 2016/2017

dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2016] <- 1
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2016] <- 1
dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2016] <- 2
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2016] <- 2
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2016] <- 3
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2016] <- 3
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2016] <- 4
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2016] <- 4
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2016] <- 5
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2016] <- 5
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2016] <- 6
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2016] <- 6
dat$PreTabelleHome[dat$Home == "Hertha" & dat$SeasonFrom==2016] <- 7
dat$PreTabelleGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2016] <- 7
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2016] <- 8
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2016] <- 8
dat$PreTabelleHome[dat$Home == "Koeln" & dat$SeasonFrom==2016] <- 9
dat$PreTabelleGuest[dat$Guest == "Koeln" & dat$SeasonFrom==2016] <- 9
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2016] <- 10
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2016] <- 10
dat$PreTabelleHome[dat$Home == "Ingolstadt" & dat$SeasonFrom==2016] <- 11
dat$PreTabelleGuest[dat$Guest == "Ingolstadt" & dat$SeasonFrom==2016] <- 11
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2016] <- 12
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2016] <- 12
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2016] <- 13
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2016] <- 13
dat$PreTabelleHome[dat$Home == "Darmstadt" & dat$SeasonFrom==2016] <- 14
dat$PreTabelleGuest[dat$Guest == "Darmstadt" & dat$SeasonFrom==2016] <- 14
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2016] <- 15
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2016] <- 15
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2016] <- 16
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2016] <- 16
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2016] <- 17
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2016] <- 17
dat$PreTabelleHome[dat$Home == "Leipzig" & dat$SeasonFrom==2016] <- 18
dat$PreTabelleGuest[dat$Guest == "Leipzig" & dat$SeasonFrom==2016] <- 18

dat$AufsteigerHome[dat$Home == "Freiburg" & dat$SeasonFrom==2016] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2016] <- TRUE
dat$AufsteigerHome[dat$Home == "Leipzig" & dat$SeasonFrom==2016] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Leipzig" & dat$SeasonFrom==2016] <- TRUE

## Laufende Saison 2017/2018

dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2017] <- 1
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2017] <- 1
dat$PreTabelleHome[dat$Home == "Leipzig" & dat$SeasonFrom==2017] <- 2
dat$PreTabelleGuest[dat$Guest == "Leipzig" & dat$SeasonFrom==2017] <- 2
dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2017] <- 3
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2017] <- 3
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2017] <- 4
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2017] <- 4
dat$PreTabelleHome[dat$Home == "Koeln" & dat$SeasonFrom==2017] <- 5
dat$PreTabelleGuest[dat$Guest == "Koeln" & dat$SeasonFrom==2017] <- 5
dat$PreTabelleHome[dat$Home == "Hertha" & dat$SeasonFrom==2017] <- 6
dat$PreTabelleGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2017] <- 6
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2017] <- 7
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2017] <- 7
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2017] <- 8
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2017] <- 8
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2017] <- 9
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2017] <- 9
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2017] <- 10
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2017] <- 10
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2017] <- 11
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2017] <- 11
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2017] <- 12
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2017] <- 12
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2017] <- 13
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2017] <- 13
dat$PreTabelleHome[dat$Home == "HSV" & dat$SeasonFrom==2017] <- 14
dat$PreTabelleGuest[dat$Guest == "HSV" & dat$SeasonFrom==2017] <- 14
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2017] <- 15
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2017] <- 15
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2017] <- 16
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2017] <- 16
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2017] <- 17
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2017] <- 17
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2017] <- 18
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2017] <- 18

dat$AufsteigerHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2017] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2017] <- TRUE
dat$AufsteigerHome[dat$Home == "Hannover" & dat$SeasonFrom==2017] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2017] <- TRUE

## Laufende Saison 2018/2019

dat$PreTabelleHome[dat$Home == "Bayern" & dat$SeasonFrom==2018] <- 1
dat$PreTabelleGuest[dat$Guest == "Bayern" & dat$SeasonFrom==2018] <- 1
dat$PreTabelleHome[dat$Home == "Schalke" & dat$SeasonFrom==2018] <- 2
dat$PreTabelleGuest[dat$Guest == "Schalke" & dat$SeasonFrom==2018] <- 2
dat$PreTabelleHome[dat$Home == "Hoffenheim" & dat$SeasonFrom==2018] <- 3
dat$PreTabelleGuest[dat$Guest == "Hoffenheim" & dat$SeasonFrom==2018] <- 3
dat$PreTabelleHome[dat$Home == "Dortmund" & dat$SeasonFrom==2018] <- 4
dat$PreTabelleGuest[dat$Guest == "Dortmund" & dat$SeasonFrom==2018] <- 4
dat$PreTabelleHome[dat$Home == "Leverkusen" & dat$SeasonFrom==2018] <- 5
dat$PreTabelleGuest[dat$Guest == "Leverkusen" & dat$SeasonFrom==2018] <- 5
dat$PreTabelleHome[dat$Home == "Leipzig" & dat$SeasonFrom==2018] <- 6
dat$PreTabelleGuest[dat$Guest == "Leipzig" & dat$SeasonFrom==2018] <- 6
dat$PreTabelleHome[dat$Home == "Stuttgart" & dat$SeasonFrom==2018] <- 7
dat$PreTabelleGuest[dat$Guest == "Stuttgart" & dat$SeasonFrom==2018] <- 7
dat$PreTabelleHome[dat$Home == "Frankfurt" & dat$SeasonFrom==2018] <- 8
dat$PreTabelleGuest[dat$Guest == "Frankfurt" & dat$SeasonFrom==2018] <- 8
dat$PreTabelleHome[dat$Home == "Gladbach" & dat$SeasonFrom==2018] <- 9
dat$PreTabelleGuest[dat$Guest == "Gladbach" & dat$SeasonFrom==2018] <- 9
dat$PreTabelleHome[dat$Home == "Hertha" & dat$SeasonFrom==2018] <- 10
dat$PreTabelleGuest[dat$Guest == "Hertha" & dat$SeasonFrom==2018] <- 10
dat$PreTabelleHome[dat$Home == "Bremen" & dat$SeasonFrom==2018] <- 11
dat$PreTabelleGuest[dat$Guest == "Bremen" & dat$SeasonFrom==2018] <- 11
dat$PreTabelleHome[dat$Home == "Augsburg" & dat$SeasonFrom==2018] <- 12
dat$PreTabelleGuest[dat$Guest == "Augsburg" & dat$SeasonFrom==2018] <- 12
dat$PreTabelleHome[dat$Home == "Hannover" & dat$SeasonFrom==2018] <- 13
dat$PreTabelleGuest[dat$Guest == "Hannover" & dat$SeasonFrom==2018] <- 13
dat$PreTabelleHome[dat$Home == "Mainz" & dat$SeasonFrom==2018] <- 14
dat$PreTabelleGuest[dat$Guest == "Mainz" & dat$SeasonFrom==2018] <- 14
dat$PreTabelleHome[dat$Home == "Freiburg" & dat$SeasonFrom==2018] <- 15
dat$PreTabelleGuest[dat$Guest == "Freiburg" & dat$SeasonFrom==2018] <- 15
dat$PreTabelleHome[dat$Home == "Wolfsburg" & dat$SeasonFrom==2018] <- 16
dat$PreTabelleGuest[dat$Guest == "Wolfsburg" & dat$SeasonFrom==2018] <- 16
dat$PreTabelleHome[dat$Home == "Duesseldorf" & dat$SeasonFrom==2018] <- 17
dat$PreTabelleGuest[dat$Guest == "Duesseldorf" & dat$SeasonFrom==2018] <- 17
dat$PreTabelleHome[dat$Home == "Nuernberg" & dat$SeasonFrom==2018] <- 18
dat$PreTabelleGuest[dat$Guest == "Nuernberg" & dat$SeasonFrom==2018] <- 18

dat$AufsteigerHome[dat$Home == "Duesseldorf" & dat$SeasonFrom==2018] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Duesseldorf" & dat$SeasonFrom==2018] <- TRUE
dat$AufsteigerHome[dat$Home == "Nuernberg" & dat$SeasonFrom==2018] <- TRUE
dat$AufsteigerGuest[dat$Guest == "Nuernberg" & dat$SeasonFrom==2018] <- TRUE

####### Marktwerte Home  #########

## Marktwerte mit Stichtag 15.09. und 15.02.
# (Ausnahme 2010: 15.09. nicht verfuegbar, daher 01.11. )
dat$MarktwertHome <- numeric(dim(dat)[1])

# Saison 10/11 1. Halbjahr, Stichtag 01.11.2010

dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 295.85
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 129.4
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 125.25
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 58.4
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 40.6
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 60.9
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 47.88
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 82.15
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 134.8
dat$MarktwertHome[dat$Home == "Klautern" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 37.8
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 55.7
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 141.35
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 53.6
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 39.7
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 144.6
dat$MarktwertHome[dat$Home == "St. Pauli" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 30.65
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 100.1
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 156.4

## Saison 10/11 2. Halbjahr, Stichtag 15.02.2011

dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 294
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 116.4
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 144.4
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 56.28
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 52.85
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 51.95
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 51.73
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 82.25
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 129.85
dat$MarktwertHome[dat$Home == "Klautern" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 46.95
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 58.4
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 142.5
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 50.5
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 52.1
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 134.6
dat$MarktwertHome[dat$Home == "St. Pauli" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 33
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 97.65
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 121.7

## Saison 11/12 1. Halbjahr, Stichtag 15.09.2011

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 29.05
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 334.85
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 102.4
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 175.7
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 54.23
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 61.85
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 69.15
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 45.78
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 83.45
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 91.45
dat$MarktwertHome[dat$Home == "Klautern" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 40.78
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 66.58
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 140.3
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 51.95
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 42.9
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 125.55
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 92.9
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 98.03

## Saison 11/12 2. Halbjahr, Stichtag 15.02.2012

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 36.45
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 359.95
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 94.45
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 179.75
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 36.45
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 73.6
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 76.3
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 55.65
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 71.83
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 87.13
dat$MarktwertHome[dat$Home == "Klautern" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 39.15
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 68.75
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 137
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 51
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 51.05
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 140.45
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 97.9
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 99.3

## Saison 12/13 1. Halbjahr, Stichtag 15.09.2012:

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 40.95
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 416.45
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 82.35
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 210.75
dat$MarktwertHome[dat$Home == "Duesseldorf" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 25.2
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 44.6
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 46.15
dat$MarktwertHome[dat$Home == "Fuerth" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 29.9
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 89.2
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 77.45
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 77.95
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 109.4
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 129.75
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 45.8
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 41.05
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 152
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 93.65
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 115.7

## Saison 12/13 2. Halbjahr, Stichtag 15.02.2013

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 34.3
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 435.45
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 84.9
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 249.95
dat$MarktwertHome[dat$Home == "Duesseldorf" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 30.9
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 54.2
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 50.75
dat$MarktwertHome[dat$Home == "Fuerth" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 30.85
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 83.8
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 86
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 72.95
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 101.05
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 143.8
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 48.1
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 40.95
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 171.6
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 93.95
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 97.4

## Saison 13/14 1. Halbjahr, Stichtag 15.09.2013

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 37.1
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 483.65
dat$MarktwertHome[dat$Home == "Braunschweig" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 25.45
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 58.3
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 288.05
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 82.65
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 68.88
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 94.74
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 73.58
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 40.6
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 67.5
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 90.3
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 133.1
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 39.85
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 50.05
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 181.8
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 89.4
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 108.4

## Saison 13/14 2. Halbjahr, Stichtag 15.02.2014

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 43.58
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 525.55
dat$MarktwertHome[dat$Home == "Braunschweig" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 26.6
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 56.35
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 323.35
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 83.85
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 70.25
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 101.83
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 78.05
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 53.8
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 82
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 89.45
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 166.15
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 56.6
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 53.2
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 186
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 88.4
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 123.35

## Saison 14/15 1. Halbjahr, Stichtag 15.09.2014 nicht verfuegbar, 
## daher 23.10.2014

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 42.85
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 564.35
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 55
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 344.45
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 76.15
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 52.6
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 119.4
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 69
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 74.6
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 122.15
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 80.1
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 41.25
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 176.85
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 79.05
dat$MarktwertHome[dat$Home == "Paderborn" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 22.35
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 208.25
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 98.8
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 174.45

## Saison 14/15 2. Halbjahr, Stichtag 15.02.2015

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 48.55
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 551.25
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 52.6
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 317.8
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 72.05
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 54.45
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 130.45
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 74.5
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 63.7
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 118.2
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 71.1
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 42.15
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 177.1
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 75.2
dat$MarktwertHome[dat$Home == "Paderborn" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 28.5
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 208.7
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 89.05
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 206.35

## Saison 15/16 1. Halbjahr, Stichtag 15.09.2015

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 52.55
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 559.25
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 50.35
dat$MarktwertHome[dat$Home == "Darmstadt" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 19.1
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 287.45
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 66.8
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 144.4
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 59.95
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 61.88
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 97.65
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 53.1
dat$MarktwertHome[dat$Home == "Ingolstadt" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 27.65
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 53.25
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 189.15
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 56.68
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 177.1
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 65.35
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 205.65

## Saison 15/16 2. Halbjahr, Stichtag 15.02.2016

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 58.35
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 617.1
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 56.23
dat$MarktwertHome[dat$Home == "Darmstadt" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 20.7
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 296.15
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 81.75
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 151.65
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 59.75
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 62.05
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 99.75
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 59.6
dat$MarktwertHome[dat$Home == "Ingolstadt" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 28.8
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 57.8
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 202.5
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 57.58
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 183.4
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 75
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 191

## Saison 16/17 1. Halbjahr, Stichtag 15.09.2016

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 61.2
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 582.23
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 67.75
dat$MarktwertHome[dat$Home == "Darmstadt" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 26.96
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 339.5
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 61.35
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 43.65
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 165.6
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 78.58
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 86.23
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 80.15
dat$MarktwertHome[dat$Home == "Ingolstadt" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 37
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 73.85
dat$MarktwertHome[dat$Home == "Leipzig" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 70.88
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 253.8
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 79.95
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 212.18
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 189.6

## Saison 16/17 2. Halbjahr, Stichtag 15.02.2017

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 62.28
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 556.15
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 68.8
dat$MarktwertHome[dat$Home == "Darmstadt" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 22.25
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 378.35
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 67.75
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 52.28
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 161.55
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 84.55
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 99.88
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 76.75
dat$MarktwertHome[dat$Home == "Ingolstadt" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 35.3
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 93.8
dat$MarktwertHome[dat$Home == "Leipzig" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 111.03
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 272.45
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 77.23
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 210.13
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 158

## Saison 17/18 1. Halbjahr, Stichtag 15.09.2017

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 57.13
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 581.4
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 71.55
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 392.03
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 84
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 70.38
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 150.58
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 48.75
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 90.45
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 119.63
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 69.95
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 106.45
dat$MarktwertHome[dat$Home == "Leipzig" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 189.08
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 200.9
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 71.45
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 154.9
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 64.35
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 143.1

## Saison 17/18 2. Halbjahr, Stichtag 15.02.2018


dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 68.8
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 642.5
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 81.98
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 364.45
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 89.4
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 76.8
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 177.1
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 64.08
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 96.13
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 142.8
dat$MarktwertHome[dat$Home == "HSV" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 78.8
dat$MarktwertHome[dat$Home == "Koeln" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 118.03
dat$MarktwertHome[dat$Home == "Leipzig" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 305.55
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 262.9
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 69.7
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 215
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 81.15
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 148.8

## Saison 18/19 1. Halbjahr, Stichtag 15.09.2018

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 100.8
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 827.8
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 118.4
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 412.3
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 147.9
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 85.65
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 209.65
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 91.5
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 135.53
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 214.85
dat$MarktwertHome[dat$Home == "Duesseldorf" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 38.95
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 41.6
dat$MarktwertHome[dat$Home == "Leipzig" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 316
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 377.2
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 101.95
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 256.4
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 165.1
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 148.25

## Saison 18/19 2. Halbjahr, Stichtag 15.02.2019

dat$MarktwertHome[dat$Home == "Augsburg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 117.05
dat$MarktwertHome[dat$Home == "Bayern" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 745.7
dat$MarktwertHome[dat$Home == "Bremen" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 129.5
dat$MarktwertHome[dat$Home == "Dortmund" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 556.8
dat$MarktwertHome[dat$Home == "Frankfurt" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 223.05
dat$MarktwertHome[dat$Home == "Freiburg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 101.9
dat$MarktwertHome[dat$Home == "Gladbach" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 275.1
dat$MarktwertHome[dat$Home == "Hannover" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 127.7
dat$MarktwertHome[dat$Home == "Hertha" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 187.5
dat$MarktwertHome[dat$Home == "Hoffenheim" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 249.55
dat$MarktwertHome[dat$Home == "Duesseldorf" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 54.2
dat$MarktwertHome[dat$Home == "Nuernberg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 54.4
dat$MarktwertHome[dat$Home == "Leipzig" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 384.3
dat$MarktwertHome[dat$Home == "Leverkusen" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 377.05
dat$MarktwertHome[dat$Home == "Mainz" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 126.8
dat$MarktwertHome[dat$Home == "Schalke" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 246.7
dat$MarktwertHome[dat$Home == "Stuttgart" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 168.7
dat$MarktwertHome[dat$Home == "Wolfsburg" & 
                    dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 161.65



####### Marktwerte Guest ######

dat$MarktwertGuest <- numeric(dim(dat)[1])

dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 295.85
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 129.4
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 125.25
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 58.4
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 40.6
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 60.9
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 47.88
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 82.15
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 134.8
dat$MarktwertGuest[dat$Guest == "Klautern" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 37.8
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 55.7
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 141.35
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 53.6
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 39.7
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 144.6
dat$MarktwertGuest[dat$Guest == "St. Pauli" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 30.65
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 100.1
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2010 & dat$Matchday <= 17] <- 156.4

## Saison 10/11 2. Halbjahr, Stichtag 15.02.2011

dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 294
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 116.4
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 144.4
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 56.28
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 52.85
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 51.95
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 51.73
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 82.25
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 129.85
dat$MarktwertGuest[dat$Guest == "Klautern" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 46.95
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 58.4
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 142.5
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 50.5
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 52.1
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 134.6
dat$MarktwertGuest[dat$Guest == "St. Pauli" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 33
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 97.65
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2010 & dat$Matchday > 17] <- 121.7

## Saison 11/12 1. Halbjahr, Stichtag 15.09.2011

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 29.05
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 334.85
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 102.4
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 175.7
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 54.23
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 61.85
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 69.15
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 45.78
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 83.45
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 91.45
dat$MarktwertGuest[dat$Guest == "Klautern" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 40.78
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 66.58
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 140.3
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 51.95
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 42.9
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 125.55
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 92.9
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday <= 17] <- 98.03

## Saison 11/12 2. Halbjahr, Stichtag 15.02.2012

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 36.45
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 359.95
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 94.45
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 179.75
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 36.45
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 73.6
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 76.3
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 55.65
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 71.83
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 87.13
dat$MarktwertGuest[dat$Guest == "Klautern" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 39.15
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 68.75
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 137
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 51
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 51.05
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 140.45
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 97.9
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2011 & dat$Matchday > 17] <- 99.3

## Saison 12/13 1. Halbjahr, Stichtag 15.09.2012:

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 40.95
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 416.45
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 82.35
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 210.75
dat$MarktwertGuest[dat$Guest == "Duesseldorf" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 25.2
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 44.6
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 46.15
dat$MarktwertGuest[dat$Guest == "Fuerth" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 29.9
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 89.2
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 77.45
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 77.95
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 109.4
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 129.75
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 45.8
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 41.05
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 152
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 93.65
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday <= 17] <- 115.7

## Saison 12/13 2. Halbjahr, Stichtag 15.02.2013

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 34.3
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 435.45
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 84.9
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 249.95
dat$MarktwertGuest[dat$Guest == "Duesseldorf" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 30.9
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 54.2
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 50.75
dat$MarktwertGuest[dat$Guest == "Fuerth" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 30.85
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 83.8
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 86
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 72.95
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 101.05
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 143.8
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 48.1
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 40.95
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 171.6
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 93.95
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2012 & dat$Matchday > 17] <- 97.4

## Saison 13/14 1. Halbjahr, Stichtag 15.09.2013

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 37.1
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 483.65
dat$MarktwertGuest[dat$Guest == "Braunschweig" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 25.45
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 58.3
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 288.05
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 82.65
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 68.88
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 94.74
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 73.58
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 40.6
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 67.5
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 90.3
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 133.1
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 39.85
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 50.05
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 181.8
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 89.4
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday <= 17] <- 108.4

## Saison 13/14 2. Halbjahr, Stichtag 15.02.2014

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 43.58
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 525.55
dat$MarktwertGuest[dat$Guest == "Braunschweig" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 26.6
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 56.35
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 323.35
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 83.85
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 70.25
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 101.83
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 78.05
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 53.8
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 82
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 89.45
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 166.15
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 56.6
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 53.2
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 186
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 88.4
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2013 & dat$Matchday > 17] <- 123.35

## Saison 14/15 1. Halbjahr, Stichtag 15.09.2014 nicht verfuegbar, 
## daher 23.10.2014

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 42.85
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 564.35
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 55
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 344.45
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 76.15
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 52.6
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 119.4
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 69
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 74.6
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 122.15
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 80.1
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 41.25
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 176.85
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 79.05
dat$MarktwertGuest[dat$Guest == "Paderborn" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 22.35
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 208.25
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 98.8
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2014 & dat$Matchday <= 17] <- 174.45

## Saison 14/15 2. Halbjahr, Stichtag 15.02.2015

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 48.55
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 551.25
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 52.6
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 317.8
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 72.05
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 54.45
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 130.45
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 74.5
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 63.7
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 118.2
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 71.1
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 42.15
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 177.1
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 75.2
dat$MarktwertGuest[dat$Guest == "Paderborn" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 28.5
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 208.7
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 89.05
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2014 & dat$Matchday > 17] <- 206.35

## Saison 15/16 1. Halbjahr, Stichtag 15.09.2015

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 52.55
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 559.25
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 50.35
dat$MarktwertGuest[dat$Guest == "Darmstadt" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 19.1
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 287.45
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 66.8
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 144.4
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 59.95
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 61.88
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 97.65
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 53.1
dat$MarktwertGuest[dat$Guest == "Ingolstadt" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 27.65
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 53.25
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 189.15
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 56.68
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 177.1
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 65.35
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2015 & dat$Matchday <= 17] <- 205.65

## Saison 15/16 2. Halbjahr, Stichtag 15.02.2016

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 58.35
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 617.1
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 56.23
dat$MarktwertGuest[dat$Guest == "Darmstadt" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 20.7
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 296.15
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 81.75
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 151.65
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 59.75
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 62.05
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 99.75
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 59.6
dat$MarktwertGuest[dat$Guest == "Ingolstadt" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 28.8
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 57.8
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 202.5
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 57.58
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 183.4
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 75
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2015 & dat$Matchday > 17] <- 191

## Saison 16/17 1. Halbjahr, Stichtag 15.09.2016

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 61.2
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 582.23
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 67.75
dat$MarktwertGuest[dat$Guest == "Darmstadt" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 26.96
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 339.5
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 61.35
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 43.65
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 165.6
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 78.58
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 86.23
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 80.15
dat$MarktwertGuest[dat$Guest == "Ingolstadt" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 37
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 73.85
dat$MarktwertGuest[dat$Guest == "Leipzig" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 70.88
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 253.8
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 79.95
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 212.18
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2016 & dat$Matchday <= 17] <- 189.6

## Saison 16/17 2. Halbjahr, Stichtag 15.02.2017

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 62.28
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 556.15
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 68.8
dat$MarktwertGuest[dat$Guest == "Darmstadt" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 22.25
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 378.35
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 67.75
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 52.28
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 161.55
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 84.55
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 99.88
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 76.75
dat$MarktwertGuest[dat$Guest == "Ingolstadt" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 35.3
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 93.8
dat$MarktwertGuest[dat$Guest == "Leipzig" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 111.03
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 272.45
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 77.23
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 210.13
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2016 & dat$Matchday > 17] <- 158

## Saison 17/18 1. Halbjahr, Stichtag 15.09.2017

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 57.13
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 581.4
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 71.55
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 392.03
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 84
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 70.38
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 150.58
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 48.75
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 90.45
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 119.63
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 69.95
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 106.45
dat$MarktwertGuest[dat$Guest == "Leipzig" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 189.08
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 200.9
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 71.45
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 154.9
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 64.35
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2017 & dat$Matchday <= 17] <- 143.1

## Saison 17/18 2. Halbjahr, Stichtag 15.02.2018


dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 68.8
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 642.5
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 81.98
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 364.45
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 89.4
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 76.8
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 177.1
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 64.08
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 96.13
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 142.8
dat$MarktwertGuest[dat$Guest == "HSV" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 78.8
dat$MarktwertGuest[dat$Guest == "Koeln" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 118.03
dat$MarktwertGuest[dat$Guest == "Leipzig" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 305.55
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 262.9
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 69.7
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 215
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 81.15
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2017 & dat$Matchday > 17] <- 148.8

## Saison 18/19 1. Halbjahr, Stichtag 15.09.2018

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 100.8
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 827.8
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 118.4
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 412.3
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 147.9
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 85.65
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 209.65
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 91.5
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 135.53
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 214.85
dat$MarktwertGuest[dat$Guest == "Duesseldorf" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 38.95
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 41.6
dat$MarktwertGuest[dat$Guest == "Leipzig" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 316
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 377.2
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 101.95
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 256.4
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 165.1
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday <= 17] <- 148.25

## Saison 18/19 2. Halbjahr, Stichtag 15.02.2019

dat$MarktwertGuest[dat$Guest == "Augsburg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 117.05
dat$MarktwertGuest[dat$Guest == "Bayern" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 745.7
dat$MarktwertGuest[dat$Guest == "Bremen" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 129.5
dat$MarktwertGuest[dat$Guest == "Dortmund" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 556.8
dat$MarktwertGuest[dat$Guest == "Frankfurt" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 223.05
dat$MarktwertGuest[dat$Guest == "Freiburg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 101.9
dat$MarktwertGuest[dat$Guest == "Gladbach" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 275.1
dat$MarktwertGuest[dat$Guest == "Hannover" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 127.7
dat$MarktwertGuest[dat$Guest == "Hertha" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 187.5
dat$MarktwertGuest[dat$Guest == "Hoffenheim" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 249.55
dat$MarktwertGuest[dat$Guest == "Duesseldorf" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 54.2
dat$MarktwertGuest[dat$Guest == "Nuernberg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 54.4
dat$MarktwertGuest[dat$Guest == "Leipzig" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 384.3
dat$MarktwertGuest[dat$Guest == "Leverkusen" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 377.05
dat$MarktwertGuest[dat$Guest == "Mainz" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 126.8
dat$MarktwertGuest[dat$Guest == "Schalke" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 246.7
dat$MarktwertGuest[dat$Guest == "Stuttgart" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 168.7
dat$MarktwertGuest[dat$Guest == "Wolfsburg" & 
                     dat$SeasonFrom == 2018 & dat$Matchday > 17] <- 161.65
dat$MWHome <- numeric(dim(dat)[1])
dat$MWGuest <- numeric(dim(dat)[1])
####### Marktwerte anteilig bestimmen:
for (i in 2010:2018) {
  dat$MWHome[dat$SeasonFrom== i & dat$Matchday <= 17] <- 
    dat$MarktwertHome[dat$SeasonFrom== i & dat$Matchday <= 17] / 
    (mean(c(dat$MarktwertHome[dat$SeasonFrom== i & dat$Matchday <= 17], 
            dat$MarktwertGuest[dat$SeasonFrom== i & dat$Matchday <= 17])) * 18)
  dat$MWGuest[dat$SeasonFrom== i & dat$Matchday <= 17] <- 
    dat$MarktwertGuest[dat$SeasonFrom== i & dat$Matchday <= 17] / 
    (mean(c(dat$MarktwertHome[dat$SeasonFrom== i & dat$Matchday <= 17], 
            dat$MarktwertGuest[dat$SeasonFrom== i & dat$Matchday <= 17])) * 18)
  dat$MWHome[dat$SeasonFrom== i & dat$Matchday > 17] <- 
    dat$MarktwertHome[dat$SeasonFrom== i & dat$Matchday > 17] / 
    (mean(c(dat$MarktwertHome[dat$SeasonFrom== i & dat$Matchday > 17], 
            dat$MarktwertGuest[dat$SeasonFrom== i & dat$Matchday > 17])) * 18)
  dat$MWGuest[dat$SeasonFrom== i & dat$Matchday > 17] <- 
    dat$MarktwertGuest[dat$SeasonFrom== i & dat$Matchday > 17] / 
    (mean(c(dat$MarktwertHome[dat$SeasonFrom== i & dat$Matchday > 17], 
            dat$MarktwertGuest[dat$SeasonFrom== i & dat$Matchday > 17])) * 18)
}

####### Form (letzte 3 Spiele) auch ueber Saisons hinweg: #######

dat$FormHome <- numeric(dim(dat)[1])
dat$FormGuest <- numeric(dim(dat)[1])

for (Verein in unique(dat$Home)){
  ## Alle Spiele eines Vereins
  indH <- which(dat$Home == Verein)
  indG <- which(dat$Guest == Verein)
  Spiele <- sort(c(indH, indG))
  
  ergH <- dat$Score90Home[indH] - dat$Score90Guest[indH]
  ergG <- dat$Score90Guest[indG] - dat$Score90Home[indG]
  erg <- c(ergH, ergG)[order(c(indH, indG))]
  
  ## Punkteausbeute:
  erg2 <- numeric(length(erg)) 
  erg2[erg == 0] <- 1
  erg2[erg > 0] <- 3
  
  erg[3] <- sum(erg2[1:2])
  erg[2] <- erg2[1]
  erg[1] <- 0
  for (i in 4:length(erg2)){
    erg[i] <- sum(erg2[(i-3):(i-1)])
  }
  dat$FormHome[indH] <- erg[Spiele %in% indH]
  dat$FormGuest[indG] <- erg[Spiele %in% indG]
}









