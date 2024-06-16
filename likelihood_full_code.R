library(Lahman)
library(tidyverse)
library(shiny)
library(ggpattern)

LahmanData = LahmanData
People$name = paste(People$nameFirst, People$nameLast)

names = People %>%
  select(name, playerID)

hof = HallOfFame %>%
  filter(category == 'Player' & inducted == 'Y' & votedBy == 'BBWAA') %>%
  left_join(names) %>%
  select(1, 10, 2:8) %>%
  replace_na(list(ballots = 0, votes = 0, needed = 0)) %>%
  group_by(playerID) %>%
  mutate(percentage = votes / ballots, percentage = round(percentage, 2)) %>%
  select(2, 8)

hof_2025 = data.frame(name = c('Billy Wagner', 'Andruw Jones', 'Carlos Beltran',
                               'Alex Rodriguez', 'Manny Ramirez', 'Chase Utley',
                               'Omar Vizquel', 'Bobby Abreu', 'Jimmy Rollins', 'Andy Pettitte',
                               'Mark Buehrle', 'Francisco Rodriguez', 'Torii Hunter',
                               'David Wright', 'Ichiro Suzuki', 'CC Sabathia',
                               'Dustin Pedroia', 'Fernando Rodney', 'Brian McCann',
                               'Ian Kinsler', 'Felix Hernandez', 'Hanley Ramirez',
                               'Curtis Granderson', 'Russell Martin', 'Troy Tulowitzki'),
                      YoB = c(10, 8, 3, 4, 9, 2, 8, 6, 4, 7, 5, 3, 5, 2, 1, 1, 1, 1,
                              1, 1, 1, 1, 1, 1, 1),
                      Last_Ballot = c(73.8, 61.6, 57.1, 34.8, 32.5, 28.8, 17.7, 14.8, 
                                      14.8, 13.5, 8.3, 7.8, 7.3, 6.2, NA, NA, NA, 
                                      NA, NA, NA, NA, NA, NA, NA, NA), 
                      Position = c('Pitcher','Position','Position','Position','Position',
                                   'Position','Position','Position','Position','Pitcher',
                                   'Pitcher','Pitcher','Position','Position','Position',
                                   'Pitcher','Position','Pitcher','Position','Position',
                                   'Pitcher','Position','Position','Position','Position'))

batting = Batting %>%
  replace_na(list(SF = 0, HBP = 0))

pitching = Pitching %>%
  replace_na(list(BAOpp = 0, IBB = 0, HBP = 0, SH = 0, SF = 0, GIDP = 0, ERA = 0))

hof_standard_1 = Pitching %>%
  left_join(names) %>%
  filter(name == 'Billy Wagner' | name == 'Andy Pettitte' | name == 'Mark Buehrle' |
           name == 'Francisco Rodriguez' | name == 'CC Sabathia'| 
           name == 'Fernando Rodney'| name == 'Felix Hernandez') %>%
  group_by(name) %>%
  mutate(G = sum(G), W = sum(W), L = sum(L), ER = sum(ER), IP = sum(IPouts) / 3, 
         SO = sum(SO), BB = sum(BB), H = sum(H), SV = sum(SV), 
         Win_pct = sum(W) / (sum(sum(W) + sum(L))), ERA = (9*sum(ER)) / sum(IP),
         BBp9 = (9*sum(BB)) / sum(IP), Hp9 = (9*sum(H)) / sum(IP), CG = sum(CG), 
         SHO = sum(SHO)) %>%
  mutate(Win_pct = round(Win_pct, 3), ERA = round(ERA, 2), BBp9 = round(BBp9, 2),
         Hp9 = round(Hp9, 2)) %>%
  select(1, 31, 6:8, 10:12, 14:15, 17:18, 20, 32:35) %>%
  unique() %>%
  mutate(W_pts = floor((W - 100) / 10), G_pts = floor((G - 500) / 20), 
         Win_pct_pts = floor((Win_pct - 0.500) / 0.013),
         ERA_pts = floor((4.00 - ERA ) / 0.2), SO_pts = floor((SO - 1000) / 200),
         BBp9_pts = floor((4.00 - BBp9) / 0.3), Hp9_pts = floor((10.00 - Hp9) / 0.3),
         IP_pts = floor((IP - 1000) / 1000), CG_pts = floor((CG - 200) / 100),
         SHO_pts = floor(SHO / 30)) %>%
  select(1, 2, 18:27) 
hof_standard_1 = hof_standard_1[-c(8), ]
hof_standard_1[2, 'W_pts'] = 0
hof_standard_1[5, 'W_pts'] = 0
hof_standard_1[6, 'W_pts'] = 0
hof_standard_1[2, 'G_pts'] = 10
hof_standard_1[5, 'G_pts'] = 10
hof_standard_1[6, 'G_pts'] = 10
hof_standard_1[7, 'G_pts'] = 0
hof_standard_1[5, 'Win_pct_pts'] = 0
hof_standard_1[6, 'Win_pct_pts'] = 0
hof_standard_1[5, 'SO_pts'] = 0
hof_standard_1[5, 'BBp9_pts'] = 0
hof_standard_1[2, 'Hp9_pts'] = 10
hof_standard_1[2, 'IP_pts'] = 0
hof_standard_1[5, 'IP_pts'] = 0
hof_standard_1[1:7, 'CG_pts'] = 0
hof_standard_1 = hof_standard_1 %>%
  mutate(HOF_pts = sum(W_pts + G_pts + Win_pct_pts + ERA_pts + SO_pts + BBp9_pts + Hp9_pts +
                         IP_pts + CG_pts + SHO_pts), Avg_HOF = 50) %>%
  select(1, 2, 13, 14)


hof_standard_2 = Batting %>%
  left_join(names) %>%
  filter(name == 'Brian McCann' | name == 'Russell Martin') %>%
  group_by(name) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = sum(H) / sum(AB), RpG = sum(R) / sum(G), RBIpG = sum(RBI) / sum(G),
         SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR)) + (2*sum(X2B)) + (3*sum(X3B)) + (4*sum(HR)))
         / (sum(AB)), OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(SF) 
                                                             + sum(HBP))) %>%
  mutate(BA = round(BA, 3), RpG = round(RpG, 3), RBIpG = round(RBIpG, 3), OBP = round(OBP, 3),
         SLG = round(SLG, 3)) %>%
  select(1, 23, 6:22, 24:28) %>%
  unique() %>%
  mutate(H_pts = floor((H - 1500) / 150), BA_pts = floor((BA - 0.275) / 0.005), BA_300 = 0,
         R_pts = floor((R - 900) / 100), RpG_500 = 0, RpG_644 = 0, 
         RBI_pts = floor((RBI - 800) / 100), RBIpG_500 = 1,
         RBIpG_600 = 0, SLG_pts = floor((SLG - 0.300) / 0.025), 
         OBP_pts = floor((OBP - 0.300) / 0.010), HR_pts = floor(HR / 200), 
         HR_10pct = 1, HR_20pct = 0, XB_pts = floor((sum(X2B+X3B+HR) - 300) / 200),
         BB_pts = floor((BB - 300) / 200), SB_pts = floor((SB / 100)), Def_pts = 20) %>%
  select(1, 2, 25:42) 
hof_standard_2[1, 'BA_pts'] = 0
hof_standard_2[1, 'R_pts'] = 0
hof_standard_2[2, 'H_pts'] = 0
hof_standard_2[2, 'BA_pts'] = 0
hof_standard_2[2, 'R_pts'] = 0
hof_standard_2[2, 'RBI_pts'] = 0
hof_standard_2[2, 'RBIpG_500'] = 0
hof_standard_2[2, 'HR_pts'] = 0
hof_standard_2 = hof_standard_2 %>%
  mutate(HOF_pts = sum(H_pts + BA_pts + BA_300 + R_pts + RpG_500 + RpG_644 + RBI_pts +
                         RBIpG_500 + RBIpG_600 + SLG_pts + OBP_pts + HR_pts + HR_10pct +
                         HR_20pct + XB_pts + BB_pts + SB_pts + Def_pts), Avg_HOF = 50) %>%
  select(1, 2, 21, 22)

hof_standard_4 = Batting %>%
  left_join(names) %>%
  filter(name == 'Chase Utley' | name == 'Dustin Pedroia' | name == 'Ian Kinsler') %>%
  group_by(name) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = sum(H) / sum(AB), RpG = sum(R) / sum(G), RBIpG = sum(RBI) / sum(G),
         SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR)) + (2*sum(X2B)) + (3*sum(X3B)) + (4*sum(HR)))
         / (sum(AB)), OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(SF) 
                                                             + sum(HBP))) %>%
  mutate(BA = round(BA, 3), RpG = round(RpG, 3), RBIpG = round(RBIpG, 3), OBP = round(OBP, 3),
         SLG = round(SLG, 3)) %>%
  select(1, 23, 6:22, 24:28) %>%
  unique() %>%
  mutate(H_pts = floor((H - 1500) / 150), BA_pts = floor((BA - 0.275) / 0.005), BA_300 = 0,
         R_pts = floor((R - 900) / 100), RpG_500 = 1, RpG_644 = 1, 
         RBI_pts = floor((RBI - 800) / 100), RBIpG_500 = 1,
         RBIpG_600 = 0, SLG_pts = floor((SLG - 0.300) / 0.025), 
         OBP_pts = floor((OBP - 0.300) / 0.010), HR_pts = floor(HR / 200), HR_10pct = 1, 
         HR_20pct = 0, XB_pts = floor((sum(X2B+X3B+HR) - 300) / 200),
         BB_pts = floor((BB - 300) / 200), SB_pts = floor((SB / 100)), Def_pts = 14) %>%
  select(1, 2, 25:42)
hof_standard_4[2, 'BA_pts'] = 0
hof_standard_4[1, 'RpG_644'] = 0
hof_standard_4[3, 'RpG_644'] = 0
hof_standard_4[2, 'RBIpG_500'] = 0
hof_standard_4[3, 'RBIpG_500'] = 0
hof_standard_4[3, 'HR_10pct'] = 0
hof_standard_4[3, 'RBI_pts'] = 0
hof_standard_4 = hof_standard_4 %>%
  mutate(HOF_pts = sum(H_pts + BA_pts + BA_300 + R_pts + RpG_500 + RpG_644 + RBI_pts +
                         RBIpG_500 + RBIpG_600 + SLG_pts + OBP_pts + HR_pts + HR_10pct +
                         HR_20pct + XB_pts + BB_pts + SB_pts + Def_pts), Avg_HOF = 50) %>%
  select(1, 2, 21, 22)

hof_standard_5 = Batting %>%
  left_join(names) %>%
  filter(name == 'David Wright') %>%
  group_by(name) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = sum(H) / sum(AB), RpG = sum(R) / sum(G), RBIpG = sum(RBI) / sum(G),
         SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR)) + (2*sum(X2B)) + (3*sum(X3B)) + (4*sum(HR)))
         / (sum(AB)), OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(SF) 
                                                             + sum(HBP))) %>%
  mutate(BA = round(BA, 3), RpG = round(RpG, 3), RBIpG = round(RBIpG, 3), OBP = round(OBP, 3),
         SLG = round(SLG, 3)) %>%
  select(1, 23, 6:22, 24:28) %>%
  unique() %>%
  mutate(H_pts = floor((H - 1500) / 150), BA_pts = floor((BA - 0.275) / 0.005), BA_300 = 0,
         R_pts = floor((R - 900) / 100), RpG_500 = 1, RpG_644 = 0, 
         RBI_pts = floor((RBI - 800) / 100), RBIpG_500 = 1,
         RBIpG_600 = 1, SLG_pts = floor((SLG - 0.300) / 0.025), 
         OBP_pts = floor((OBP - 0.300) / 0.010), HR_pts = floor(HR / 200),
         HR_10pct = 1, HR_20pct = 0, 
         XB_pts = floor((sum(X2B+X3B+HR) - 300) / 200),
         BB_pts = floor((BB - 300) / 200), SB_pts = floor((SB / 100)), Def_pts = 13) %>%
  select(1, 2, 25:42) %>%
  mutate(HOF_pts = sum(H_pts + BA_pts + BA_300 + R_pts + RpG_500 + RpG_644 + RBI_pts +
                         RBIpG_500 + RBIpG_600 + SLG_pts + OBP_pts + HR_pts + HR_10pct +
                         HR_20pct + XB_pts + BB_pts + SB_pts + Def_pts), Avg_HOF = 50) %>%
  select(1, 2, 21, 22)

hof_standard_6 = Batting %>%
  left_join(names) %>%
  filter(name == 'Alex Rodriguez' | name == 'Omar Vizquel' | name == 'Jimmy Rollins' |
           name == 'Hanley Ramirez' | name == 'Troy Tulowitzki') %>%
  group_by(name) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = sum(H) / sum(AB), RpG = sum(R) / sum(G), RBIpG = sum(RBI) / sum(G),
         SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR)) + (2*sum(X2B)) + (3*sum(X3B)) + (4*sum(HR)))
         / (sum(AB)), OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(SF) 
                                                             + sum(HBP))) %>%
  mutate(BA = round(BA, 3), RpG = round(RpG, 3), RBIpG = round(RBIpG, 3), OBP = round(OBP, 3),
         SLG = round(SLG, 3)) %>%
  select(1, 23, 6:22, 24:28) %>%
  unique() %>%
  mutate(H_pts = floor((H - 1500) / 150), BA_pts = floor((BA - 0.275) / 0.005), BA_300 = 0,
         R_pts = floor((R - 900) / 100), RpG_500 = 1, RpG_644 = 0, 
         RBI_pts = floor((RBI - 800) / 100), RBIpG_500 = 1,
         RBIpG_600 = 0, SLG_pts = floor((SLG - 0.300) / 0.025), 
         OBP_pts = floor((OBP - 0.300) / 0.010), 
         HR_pts = floor(HR / 200), HR_10pct = 1, HR_20pct = 0, 
         XB_pts = floor((sum(X2B+X3B+HR) - 300) / 200),
         BB_pts = floor((BB - 300) / 200), SB_pts = floor((SB / 100)), Def_pts = 16) %>%
  select(1, 2, 25:42)
hof_standard_6[1, 'BA_pts'] = 0
hof_standard_6[2, 'R_pts'] = 8
hof_standard_6[3, 'BA_pts'] = 0
hof_standard_6[5, 'H_pts'] = 0
hof_standard_6[5, 'R_pts'] = 0
hof_standard_6[1, 'RpG_500'] = 0
hof_standard_6[2, 'RpG_644'] = 1
hof_standard_6[1, 'RBIpG_500'] = 0
hof_standard_6[3, 'RBIpG_500'] = 0
hof_standard_6[2, 'RBIpG_600'] = 1
hof_standard_6[5, 'RBIpG_600'] = 1
hof_standard_6[1, 'HR_10pct'] = 0
hof_standard_6[3, 'HR_10pct'] = 0
hof_standard_6[2, 'HR_20pct'] = 1
hof_standard_6[2, 'RBI_pts'] = 8
hof_standard_6[5, 'RBI_pts'] = 0
hof_standard_6 = hof_standard_6 %>%
  mutate(HOF_pts = sum(H_pts + BA_pts + BA_300 + R_pts + RpG_500 + RpG_644 + RBI_pts +
                         RBIpG_500 + RBIpG_600 + SLG_pts + OBP_pts + HR_pts + HR_10pct +
                         HR_20pct + XB_pts + BB_pts + SB_pts + Def_pts), Avg_HOF = 50) %>%
  select(1, 2, 21, 22)

hof_standard_7 = Batting %>%
  left_join(names) %>%
  filter(name == 'Manny Ramirez') %>%
  group_by(name) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = sum(H) / sum(AB), RpG = sum(R) / sum(G), RBIpG = sum(RBI) / sum(G),
         SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR)) + (2*sum(X2B)) + (3*sum(X3B)) + (4*sum(HR)))
         / (sum(AB)), OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(SF) 
                                                             + sum(HBP))) %>%
  mutate(BA = round(BA, 3), RpG = round(RpG, 3), RBIpG = round(RBIpG, 3), OBP = round(OBP, 3),
         SLG = round(SLG, 3)) %>%
  select(1, 23, 6:22, 24:28) %>%
  unique() %>%
  mutate(H_pts = floor((H - 1500) / 150), BA_pts = floor((BA - 0.275) / 0.005), BA_300 = 1,
         R_pts = floor((R - 900) / 100), RpG_500 = 1, RpG_644 = 1, 
         RBI_pts = floor((RBI - 800) / 100), RBIpG_500 = 1,
         RBIpG_600 = 1, SLG_pts = floor((SLG - 0.300) / 0.025), 
         OBP_pts = floor((OBP - 0.300) / 0.010), 
         HR_pts = floor(HR / 200), HR_10pct = 1, HR_20pct = 1, 
         XB_pts = floor((sum(X2B+X3B+HR) - 300) / 200),
         BB_pts = floor((BB - 300) / 200), SB_pts = floor((SB / 100)), Def_pts = 3) %>%
  select(1, 2, 25:42) 
hof_standard_7[1, 'SLG_pts'] = 10
hof_standard_7[1, 'OBP_pts'] = 10
hof_standard_7[1, 'RBI_pts'] = 8
hof_standard_7 = hof_standard_7 %>%
  mutate(HOF_pts = sum(H_pts + BA_pts + BA_300 + R_pts + RpG_500 + RpG_644 + RBI_pts +
                         RBIpG_500 + RBIpG_600 + SLG_pts + OBP_pts + HR_pts + HR_10pct +
                         HR_20pct + XB_pts + BB_pts + SB_pts + Def_pts), Avg_HOF = 50) %>%
  select(1, 2, 21, 22)

hof_standard_8 = Batting %>%
  left_join(names) %>%
  filter(name == 'Andruw Jones' | name == 'Torii Hunter' | name == 'Carlos Beltran' |
           name == 'Curtis Granderson') %>%
  group_by(name) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = sum(H) / sum(AB), RpG = sum(R) / sum(G), RBIpG = sum(RBI) / sum(G),
         SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR)) + (2*sum(X2B)) + (3*sum(X3B)) + (4*sum(HR)))
         / (sum(AB)), OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(SF) 
                                                             + sum(HBP))) %>%
  mutate(BA = round(BA, 3), RpG = round(RpG, 3), RBIpG = round(RBIpG, 3), OBP = round(OBP, 3),
         SLG = round(SLG, 3)) %>%
  select(1, 23, 6:22, 24:28) %>%
  unique() %>%
  mutate(H_pts = floor((H - 1500) / 150), BA_pts = floor((BA - 0.275) / 0.005), BA_300 = 0,
         R_pts = floor((R - 900) / 100), RpG_500 = 1, RpG_644 = 0,
         RBI_pts = floor((RBI - 800) / 100), RBIpG_500 = 1,
         RBIpG_600 = 0, SLG_pts = floor((SLG - 0.300) / 0.025), 
         OBP_pts = floor((OBP - 0.300) / 0.010), 
         HR_pts = floor(HR / 200), HR_10pct = 1, HR_20pct = 0, 
         XB_pts = floor((sum(X2B+X3B+HR) - 300) / 200),
         BB_pts = floor((BB - 300) / 200), SB_pts = floor((SB / 100)), Def_pts = 12) %>%
  select(1, 2, 25:42)
hof_standard_8[1, 'BA_pts'] = 0
hof_standard_8[4, 'BA_pts'] = 0
hof_standard_8[4, 'RBIpG_500'] = 0
hof_standard_8[3, 'RBIpG_600'] = 1
hof_standard_8[1, 'HR_20pct'] = 1
hof_standard_8 = hof_standard_8 %>%
  mutate(HOF_pts = sum(H_pts + BA_pts + BA_300 + R_pts + RpG_500 + RpG_644 + RBI_pts +
                         RBIpG_500 + RBIpG_600 + SLG_pts + OBP_pts + HR_pts + HR_10pct +
                         HR_20pct + XB_pts + BB_pts + SB_pts + Def_pts), Avg_HOF = 50) %>%
  select(1, 2, 21, 22)

hof_standard_9 = Batting %>%
  left_join(names) %>%
  filter(name == 'Ichiro Suzuki' | name == 'Bobby Abreu') %>%
  group_by(name) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = sum(H) / sum(AB), RpG = sum(R) / sum(G), RBIpG = sum(RBI) / sum(G),
         SLG = ((sum(H)-sum(X2B)-sum(X3B)-sum(HR)) + (2*sum(X2B)) + (3*sum(X3B)) + (4*sum(HR)))
         / (sum(AB)), OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(SF) 
                                                             + sum(HBP))) %>%
  mutate(BA = round(BA, 3), RpG = round(RpG, 3), RBIpG = round(RBIpG, 3), OBP = round(OBP, 3),
         SLG = round(SLG, 3)) %>%
  select(1, 23, 6:22, 24:28) %>%
  unique() %>%
  mutate(H_pts = floor((H - 1500) / 150), BA_pts = floor((BA - 0.275) / 0.005), BA_300 = 1,
         R_pts = floor((R - 900) / 100), RpG_500 = 1, RpG_644 = 0,
         RBI_pts = floor((RBI - 800) / 100), RBIpG_500 = 0,
         RBIpG_600 = 0, SLG_pts = floor((SLG - 0.300) / 0.025), 
         OBP_pts = floor((OBP - 0.300) / 0.010), 
         HR_pts = floor(HR / 200), HR_10pct = 0, HR_20pct = 0, 
         XB_pts = floor((sum(X2B+X3B+HR) - 300) / 200),
         BB_pts = floor((BB - 300) / 200), SB_pts = floor((SB / 100)), Def_pts = 6) %>%
  select(1, 2, 25:42) 
hof_standard_9[1, 'BA_300'] = 0
hof_standard_9[1, 'RBIpG_500'] = 1
hof_standard_9[1, 'HR_10pct'] = 1
hof_standard_9[2, 'RBI_pts'] = 0
hof_standard_9 = hof_standard_9 %>%
  mutate(HOF_pts = sum(H_pts + BA_pts + BA_300 + R_pts + RpG_500 + RpG_644 + RBI_pts +
                         RBIpG_500 + RBIpG_600 + SLG_pts + OBP_pts + HR_pts + HR_10pct +
                         HR_20pct + XB_pts + BB_pts + SB_pts + Def_pts), Avg_HOF = 50) %>%
  select(1, 2, 21, 22)

standards = rbind(hof_standard_1, hof_standard_2, hof_standard_4, hof_standard_5, hof_standard_6, 
                  hof_standard_7, hof_standard_8, hof_standard_9) %>%
  group_by(name) %>%
  mutate(comp_to_avg_st = HOF_pts / Avg_HOF)

hof_2025 = hof_2025 %>%
  left_join(standards) %>%
  arrange(desc(YoB), desc(Last_Ballot)) %>%
  select(5, 1, 4, 2:3, 6:8)

ink_hr_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(HR = sum(HR)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, HR) %>%
  arrange(yearID, desc(HR)) 

ink_hr_al = ink_hr_al[order(ink_hr_al$HR,
                            decreasing = TRUE), ]

ink_hr_al = Reduce(rbind,                                
                   by(ink_hr_al,
                      ink_hr_al["yearID"],
                      head,
                      n = 1))

ink_hr_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(HR = sum(HR)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, HR) %>%
  arrange(yearID, desc(HR))

ink_hr_nl = ink_hr_nl[order(ink_hr_nl$HR,
                            decreasing = TRUE), ]

ink_hr_nl = Reduce(rbind,                                
                   by(ink_hr_nl,
                      ink_hr_nl["yearID"],
                      head,
                      n = 1))
hr_bi = rbind(ink_hr_al, ink_hr_nl)

hr_pts = hof_2025 %>%
  inner_join(hr_bi) %>%
  group_by(name) %>%
  mutate(hr_pts = n()*4) %>%
  unique()

ink_rbi_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(RBI = sum(RBI)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, RBI) %>%
  arrange(yearID, desc(RBI)) 

ink_rbi_al = ink_rbi_al[order(ink_rbi_al$RBI,
                              decreasing = TRUE), ]

ink_rbi_al = Reduce(rbind,                                
                    by(ink_rbi_al,
                       ink_rbi_al["yearID"],
                       head,
                       n = 1))

ink_rbi_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(RBI = sum(RBI)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, RBI) %>%
  arrange(yearID, desc(RBI))

ink_rbi_nl = ink_rbi_nl[order(ink_rbi_nl$RBI,
                              decreasing = TRUE), ]

ink_rbi_nl = Reduce(rbind,                                
                    by(ink_rbi_nl,
                       ink_rbi_nl["yearID"],
                       head,
                       n = 1))
rbi_bi = rbind(ink_rbi_al, ink_rbi_nl)

rbi_pts = hof_2025 %>%
  inner_join(rbi_bi) %>%
  group_by(name) %>%
  mutate(rbi_pts = n()*4) %>%
  unique()

ink_ba_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(PA = sum(AB + sum(BB) + sum(SF) + sum(SH))) %>%
  filter(PA >= 502) %>%
  mutate(BA = round(sum(H) / sum(AB), 3)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, BA) %>%
  arrange(yearID, desc(BA)) 

ink_ba_al = ink_ba_al[order(ink_ba_al$BA,
                            decreasing = TRUE), ]

ink_ba_al = Reduce(rbind,                                
                   by(ink_ba_al,
                      ink_ba_al["yearID"],
                      head,
                      n = 1))

ink_ba_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(PA = sum(AB + sum(BB) + sum(SF) + sum(SH))) %>%
  filter(PA >= 502) %>%
  mutate(BA = round(sum(H) / sum(AB), 3)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, BA) %>%
  arrange(yearID, desc(BA)) 

ink_ba_nl = ink_ba_nl[order(ink_ba_nl$BA,
                            decreasing = TRUE), ]

ink_ba_nl = Reduce(rbind,                                
                   by(ink_ba_nl,
                      ink_ba_nl["yearID"],
                      head,
                      n = 1))
ba_bi = rbind(ink_ba_al, ink_ba_nl)

ba_pts = hof_2025 %>%
  inner_join(ba_bi) %>%
  group_by(name) %>%
  mutate(ba_pts = n()*4) %>%
  unique()

ink_r_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(R = sum(R)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, R) %>%
  arrange(yearID, desc(R)) 

ink_r_al = ink_r_al[order(ink_r_al$R,
                          decreasing = TRUE), ]

ink_r_al = Reduce(rbind,                                
                  by(ink_r_al,
                     ink_r_al["yearID"],
                     head,
                     n = 1))

ink_r_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(R = sum(R)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, R) %>%
  arrange(yearID, desc(R))

ink_r_nl = ink_r_nl[order(ink_r_nl$R,
                          decreasing = TRUE), ]

ink_r_nl = Reduce(rbind,                                
                  by(ink_r_nl,
                     ink_r_nl["yearID"],
                     head,
                     n = 1))
r_bi = rbind(ink_r_al, ink_r_nl)

r_pts = hof_2025 %>%
  inner_join(r_bi) %>%
  group_by(name) %>%
  mutate(r_pts = n()*3) %>%
  unique()

ink_h_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(H = sum(H)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, H) %>%
  arrange(yearID, desc(H)) 

ink_h_al = ink_h_al[order(ink_h_al$H,
                          decreasing = TRUE), ]

ink_h_al = Reduce(rbind,                                
                  by(ink_h_al,
                     ink_h_al["yearID"],
                     head,
                     n = 1))

ink_h_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(H = sum(H)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, H) %>%
  arrange(yearID, desc(H))

ink_h_nl = ink_h_nl[order(ink_h_nl$H,
                          decreasing = TRUE), ]

ink_h_nl = Reduce(rbind,                                
                  by(ink_h_nl,
                     ink_h_nl["yearID"],
                     head,
                     n = 1))
h_bi = rbind(ink_h_al, ink_h_nl)

h_pts = hof_2025 %>%
  inner_join(h_bi) %>%
  group_by(name) %>%
  mutate(h_pts = n()*3) %>%
  unique()

ink_slg_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(PA = sum(AB + sum(BB) + sum(SF) + sum(SH))) %>%
  filter(PA >= 502) %>%
  mutate(SLG = round(((sum(H) - sum(X2B) -sum(X3B) - sum(HR)) + 2*sum(X2B) + 3*sum(X3B)
                      + 4*sum(HR))/ sum(AB), 3)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SLG) %>%
  arrange(yearID, desc(SLG)) 

ink_slg_al = ink_slg_al[order(ink_slg_al$SLG,
                              decreasing = TRUE), ]

ink_slg_al = Reduce(rbind,                                
                    by(ink_slg_al,
                       ink_slg_al["yearID"],
                       head,
                       n = 1))

ink_slg_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(PA = sum(AB + sum(BB) + sum(SF) + sum(SH))) %>%
  filter(PA >= 502) %>%
  mutate(SLG = round(((sum(H) - sum(X2B) -sum(X3B) - sum(HR)) + 2*sum(X2B) + 3*sum(X3B)
                      + 4*sum(HR))/ sum(AB), 3)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SLG) %>%
  arrange(yearID, desc(SLG))

ink_slg_nl = ink_slg_nl[order(ink_slg_nl$SLG,
                              decreasing = TRUE), ]

ink_slg_nl = Reduce(rbind,                                
                    by(ink_slg_nl,
                       ink_slg_nl["yearID"],
                       head,
                       n = 1))
slg_bi = rbind(ink_slg_al, ink_slg_nl)

slg_pts = hof_2025 %>%
  inner_join(slg_bi) %>%
  group_by(name) %>%
  mutate(slg_pts = n()*3) %>%
  unique()

ink_x2b_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(X2B = sum(X2B)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, X2B) %>%
  arrange(yearID, desc(X2B)) 

ink_x2b_al = ink_x2b_al[order(ink_x2b_al$X2B,
                              decreasing = TRUE), ]

ink_x2b_al = Reduce(rbind,                                
                    by(ink_x2b_al,
                       ink_x2b_al["yearID"],
                       head,
                       n = 1))

ink_x2b_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(X2B = sum(X2B)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, X2B) %>%
  arrange(yearID, desc(X2B))

ink_x2b_nl = ink_x2b_nl[order(ink_x2b_nl$X2B,
                              decreasing = TRUE), ]

ink_x2b_nl = Reduce(rbind,                                
                    by(ink_x2b_nl,
                       ink_x2b_nl["yearID"],
                       head,
                       n = 1))
x2b_bi = rbind(ink_x2b_al, ink_x2b_nl)

x2b_pts = hof_2025 %>%
  inner_join(x2b_bi) %>%
  group_by(name) %>%
  mutate(x2b_pts = n()*2) %>%
  unique()

ink_bb_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(BB = sum(BB)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, BB) %>%
  arrange(yearID, desc(BB)) 

ink_bb_al = ink_bb_al[order(ink_bb_al$BB,
                            decreasing = TRUE), ]

ink_bb_al = Reduce(rbind,                                
                   by(ink_bb_al,
                      ink_bb_al["yearID"],
                      head,
                      n = 1))

ink_bb_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(BB = sum(BB)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, BB) %>%
  arrange(yearID, desc(BB))

ink_bb_nl = ink_bb_nl[order(ink_bb_nl$BB,
                            decreasing = TRUE), ]

ink_bb_nl = Reduce(rbind,                                
                   by(ink_bb_nl,
                      ink_bb_nl["yearID"],
                      head,
                      n = 1))
bb_bi = rbind(ink_bb_al, ink_bb_nl)

bb_pts = hof_2025 %>%
  inner_join(bb_bi) %>%
  group_by(name) %>%
  mutate(bb_pts = n()*2) %>%
  unique()

ink_sb_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(SB = sum(SB)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SB) %>%
  arrange(yearID, desc(SB)) 

ink_sb_al = ink_sb_al[order(ink_sb_al$SB,
                            decreasing = TRUE), ]

ink_sb_al = Reduce(rbind,                                
                   by(ink_sb_al,
                      ink_sb_al["yearID"],
                      head,
                      n = 1))

ink_sb_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(SB = sum(SB)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SB) %>%
  arrange(yearID, desc(SB))

ink_sb_nl = ink_sb_nl[order(ink_sb_nl$SB,
                            decreasing = TRUE), ]

ink_sb_nl = Reduce(rbind,                                
                   by(ink_sb_nl,
                      ink_sb_nl["yearID"],
                      head,
                      n = 1))
sb_bi = rbind(ink_sb_al, ink_sb_nl)

sb_pts = hof_2025 %>%
  inner_join(sb_bi) %>%
  group_by(name) %>%
  mutate(sb_pts = n()*2) %>%
  unique()

ink_g_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(G = sum(G)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, G) %>%
  arrange(yearID, desc(G)) 

ink_g_al = ink_g_al[order(ink_g_al$G,
                          decreasing = TRUE), ]

ink_g_al = Reduce(rbind,                                
                  by(ink_g_al,
                     ink_g_al["yearID"],
                     head,
                     n = 1))

ink_g_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(G = sum(G)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, G) %>%
  arrange(yearID, desc(G))

ink_g_nl = ink_g_nl[order(ink_g_nl$G,
                          decreasing = TRUE), ]

ink_g_nl = Reduce(rbind,                                
                  by(ink_g_nl,
                     ink_g_nl["yearID"],
                     head,
                     n = 1))
g_bi = rbind(ink_g_al, ink_g_nl)

g_pts = hof_2025 %>%
  inner_join(g_bi) %>%
  group_by(name) %>%
  mutate(g_pts = n()*2) %>%
  unique()

ink_ab_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(AB = sum(AB)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, AB) %>%
  arrange(yearID, desc(AB)) 

ink_ab_al = ink_ab_al[order(ink_ab_al$AB,
                            decreasing = TRUE), ]

ink_ab_al = Reduce(rbind,                                
                   by(ink_ab_al,
                      ink_ab_al["yearID"],
                      head,
                      n = 1))

ink_ab_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(AB = sum(AB)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, AB) %>%
  arrange(yearID, desc(AB))

ink_ab_nl = ink_ab_nl[order(ink_ab_nl$AB,
                            decreasing = TRUE), ]

ink_ab_nl = Reduce(rbind,                                
                   by(ink_ab_nl,
                      ink_ab_nl["yearID"],
                      head,
                      n = 1))
ab_bi = rbind(ink_ab_al, ink_ab_nl)

ab_pts = hof_2025 %>%
  inner_join(ab_bi) %>%
  group_by(name) %>%
  mutate(ab_pts = n()) %>%
  unique()

ink_w_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(W = sum(W)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, W) %>%
  arrange(yearID, desc(W)) 

ink_w_al = ink_w_al[order(ink_w_al$W,
                          decreasing = TRUE), ]

ink_w_al = Reduce(rbind,                                
                  by(ink_w_al,
                     ink_w_al["yearID"],
                     head,
                     n = 1))

ink_w_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(W = sum(W)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, W) %>%
  arrange(yearID, desc(W))

ink_w_nl = ink_w_nl[order(ink_w_nl$W,
                          decreasing = TRUE), ]

ink_w_nl = Reduce(rbind,                                
                  by(ink_w_nl,
                     ink_w_nl["yearID"],
                     head,
                     n = 1))
w_bi = rbind(ink_w_al, ink_w_nl)

w_pts = hof_2025 %>%
  inner_join(w_bi) %>%
  group_by(name) %>%
  mutate(w_pts = n()*4) %>%
  unique()

ink_era_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(ERA = (9*sum(ER)) / (sum(IPouts) / 3), IP = sum(IPouts) / 3) %>%
  filter(IP >= 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, ERA) %>%
  arrange(yearID, ERA) 

ink_era_al = ink_era_al[order(ink_era_al$ERA,
                              decreasing = FALSE), ]

ink_era_al = Reduce(rbind,                                
                    by(ink_era_al,
                       ink_era_al["yearID"],
                       head,
                       n = 1))

ink_era_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(ERA = (9*sum(ER)) / (sum(IPouts) / 3), IP = sum(IPouts) / 3) %>%
  filter(IP >= 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, ERA) %>%
  arrange(yearID, ERA) 

ink_era_nl = ink_era_nl[order(ink_era_nl$ERA,
                              decreasing = FALSE), ]

ink_era_nl = Reduce(rbind,                                
                    by(ink_era_nl,
                       ink_era_nl["yearID"],
                       head,
                       n = 1))
era_bi = rbind(ink_era_al, ink_era_nl)

era_pts = hof_2025 %>%
  inner_join(era_bi) %>%
  group_by(name) %>%
  mutate(era_pts = n()*4) %>%
  unique()

ink_so_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(SO = sum(SO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SO) %>%
  arrange(yearID, desc(SO)) 

ink_so_al = ink_so_al[order(ink_so_al$SO,
                            decreasing = TRUE), ]

ink_so_al = Reduce(rbind,                                
                   by(ink_so_al,
                      ink_so_al["yearID"],
                      head,
                      n = 1))

ink_so_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(SO = sum(SO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SO) %>%
  arrange(yearID, desc(SO))

ink_so_nl = ink_so_nl[order(ink_so_nl$SO,
                            decreasing = TRUE), ]

ink_so_nl = Reduce(rbind,                                
                   by(ink_so_nl,
                      ink_so_nl["yearID"],
                      head,
                      n = 1))
so_bi = rbind(ink_so_al, ink_so_nl)

so_pts = hof_2025 %>%
  inner_join(so_bi) %>%
  group_by(name) %>%
  mutate(so_pts = n()*4) %>%
  unique()

ink_ip_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(IP = sum(IPouts) / 3) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, IP) %>%
  arrange(yearID, desc(IP)) 

ink_ip_al = ink_ip_al[order(ink_ip_al$IP,
                            decreasing = TRUE), ]

ink_ip_al = Reduce(rbind,                                
                   by(ink_ip_al,
                      ink_ip_al["yearID"],
                      head,
                      n = 1))

ink_ip_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(IP = sum(IPouts) / 3) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, IP) %>%
  arrange(yearID, desc(IP))

ink_ip_nl = ink_ip_nl[order(ink_ip_nl$IP,
                            decreasing = TRUE), ]

ink_ip_nl = Reduce(rbind,                                
                   by(ink_ip_nl,
                      ink_ip_nl["yearID"],
                      head,
                      n = 1))
ip_bi = rbind(ink_ip_al, ink_ip_nl)

ip_pts = hof_2025 %>%
  inner_join(ip_bi) %>%
  group_by(name) %>%
  mutate(ip_pts = n()*3) %>%
  unique()

ink_win_pct_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(Win_pct = sum(W) / (sum(W) + sum(L))) %>%
  filter((sum(W) + sum(L)) >= 16) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, Win_pct) %>%
  arrange(yearID, desc(Win_pct)) 

ink_win_pct_al = ink_win_pct_al[order(ink_win_pct_al$Win_pct,
                                      decreasing = TRUE), ]

ink_win_pct_al = Reduce(rbind,                                
                        by(ink_win_pct_al,
                           ink_win_pct_al["yearID"],
                           head,
                           n = 1))

ink_win_pct_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(Win_pct = sum(W) / (sum(W) + sum(L))) %>%
  filter((sum(W) + sum(L)) >= 16) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, Win_pct) %>%
  arrange(yearID, desc(Win_pct))

ink_win_pct_nl = ink_win_pct_nl[order(ink_win_pct_nl$Win_pct,
                                      decreasing = TRUE), ]

ink_win_pct_nl = Reduce(rbind,                                
                        by(ink_win_pct_nl,
                           ink_win_pct_nl["yearID"],
                           head,
                           n = 1))
win_pct_bi = rbind(ink_win_pct_al, ink_win_pct_nl)

win_pct_pts = hof_2025 %>%
  inner_join(win_pct_bi) %>%
  group_by(name) %>%
  mutate(win_pct_pts = n()*3) %>%
  unique()

ink_sv_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(sv = sum(SV)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SV) %>%
  arrange(yearID, desc(SV)) 

ink_sv_al = ink_sv_al[order(ink_sv_al$SV,
                            decreasing = TRUE), ]

ink_sv_al = Reduce(rbind,                                
                   by(ink_sv_al,
                      ink_sv_al["yearID"],
                      head,
                      n = 1))

ink_sv_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(sv = sum(SV)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SV) %>%
  arrange(yearID, desc(SV))

ink_sv_nl = ink_sv_nl[order(ink_sv_nl$SV,
                            decreasing = TRUE), ]

ink_sv_nl = Reduce(rbind,                                
                   by(ink_sv_nl,
                      ink_sv_nl["yearID"],
                      head,
                      n = 1))
sv_bi = rbind(ink_sv_al, ink_sv_nl)

sv_pts = hof_2025 %>%
  inner_join(sv_bi) %>%
  group_by(name) %>%
  mutate(sv_pts = n()*3) %>%
  unique()

ink_cg_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(CG = sum(CG)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, CG) %>%
  arrange(yearID, desc(CG)) 

ink_cg_al = ink_cg_al[order(ink_cg_al$CG,
                            decreasing = TRUE), ]

ink_cg_al = Reduce(rbind,                                
                   by(ink_cg_al,
                      ink_cg_al["yearID"],
                      head,
                      n = 1))

ink_cg_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(CG = sum(CG)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, CG) %>%
  arrange(yearID, desc(CG))

ink_cg_nl = ink_cg_nl[order(ink_cg_nl$CG,
                            decreasing = TRUE), ]

ink_cg_nl = Reduce(rbind,                                
                   by(ink_cg_nl,
                      ink_cg_nl["yearID"],
                      head,
                      n = 1))
cg_bi = rbind(ink_cg_al, ink_cg_nl)

cg_pts = hof_2025 %>%
  inner_join(cg_bi) %>%
  group_by(name) %>%
  mutate(cg_pts = n()*2) %>%
  unique()

ink_bbp9_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(BBp9 = (9*sum(BB)) / (sum(IPouts) / 3), IP = sum(IPouts) / 3) %>%
  filter(IP >= 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, BBp9) %>%
  arrange(yearID, BBp9) 

ink_bbp9_al = ink_bbp9_al[order(ink_bbp9_al$BBp9,
                                decreasing = FALSE), ]

ink_bbp9_al = Reduce(rbind,                                
                     by(ink_bbp9_al,
                        ink_bbp9_al["yearID"],
                        head,
                        n = 1))

ink_bbp9_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(BBp9 = (9*sum(BB)) / (sum(IPouts) / 3), IP = sum(IPouts) / 3) %>%
  filter(IP >= 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, BBp9) %>%
  arrange(yearID, BBp9) 

ink_bbp9_nl = ink_bbp9_nl[order(ink_bbp9_nl$BBp9,
                                decreasing = FALSE), ]

ink_bbp9_nl = Reduce(rbind,                                
                     by(ink_bbp9_nl,
                        ink_bbp9_nl["yearID"],
                        head,
                        n = 1))
bbp9_bi = rbind(ink_bbp9_al, ink_bbp9_nl)

bbp9_pts = hof_2025 %>%
  inner_join(bbp9_bi) %>%
  group_by(name) %>%
  mutate(bbp9_pts = n()*2) %>%
  unique()

ink_hp9_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(Hp9 = (9*sum(H)) / (sum(IPouts) / 3), IP = sum(IPouts) / 3) %>%
  filter(IP >= 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, Hp9) %>%
  arrange(yearID, Hp9) 

ink_hp9_al = ink_hp9_al[order(ink_hp9_al$Hp9,
                              decreasing = FALSE), ]

ink_hp9_al = Reduce(rbind,                                
                    by(ink_hp9_al,
                       ink_hp9_al["yearID"],
                       head,
                       n = 1))

ink_hp9_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(Hp9 = (9*sum(H)) / (sum(IPouts) / 3), IP = sum(IPouts) / 3) %>%
  filter(IP >= 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, Hp9) %>%
  arrange(yearID, Hp9) 

ink_hp9_nl = ink_hp9_nl[order(ink_hp9_nl$Hp9,
                              decreasing = FALSE), ]

ink_hp9_nl = Reduce(rbind,                                
                    by(ink_hp9_nl,
                       ink_hp9_nl["yearID"],
                       head,
                       n = 1))
hp9_bi = rbind(ink_hp9_al, ink_hp9_nl)

hp9_pts = hof_2025 %>%
  inner_join(hp9_bi) %>%
  group_by(name) %>%
  mutate(hp9_pts = n()*2) %>%
  unique()

ink_g_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(G = sum(G)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, G) %>%
  arrange(yearID, desc(G)) 

ink_g_al = ink_g_al[order(ink_g_al$G,
                          decreasing = TRUE), ]

ink_g_al = Reduce(rbind,                                
                  by(ink_g_al,
                     ink_g_al["yearID"],
                     head,
                     n = 1))

ink_g_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(G = sum(G)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, G) %>%
  arrange(yearID, desc(G))

ink_g_nl = ink_g_nl[order(ink_g_nl$G,
                          decreasing = TRUE), ]

ink_g_nl = Reduce(rbind,                                
                  by(ink_g_nl,
                     ink_g_nl["yearID"],
                     head,
                     n = 1))
g_bi = rbind(ink_g_al, ink_g_nl)

g_pts = hof_2025 %>%
  inner_join(g_bi) %>%
  group_by(name) %>%
  mutate(g_pts = n()) %>%
  unique()

ink_gs_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(GS = sum(GS)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, GS) %>%
  arrange(yearID, desc(GS)) 

ink_gs_al = ink_gs_al[order(ink_gs_al$GS,
                            decreasing = TRUE), ]

ink_gs_al = Reduce(rbind,                                
                   by(ink_gs_al,
                      ink_gs_al["yearID"],
                      head,
                      n = 1))

ink_gs_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(GS = sum(GS)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, GS) %>%
  arrange(yearID, desc(GS))

ink_gs_nl = ink_gs_nl[order(ink_gs_nl$GS,
                            decreasing = TRUE), ]

ink_gs_nl = Reduce(rbind,                                
                   by(ink_gs_nl,
                      ink_gs_nl["yearID"],
                      head,
                      n = 1))
gs_bi = rbind(ink_gs_al, ink_gs_nl)

gs_pts = hof_2025 %>%
  inner_join(gs_bi) %>%
  group_by(name) %>%
  mutate(gs_pts = n()) %>%
  unique()

ink_sho_al = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(SHO = sum(SHO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SHO) %>%
  arrange(yearID, desc(SHO)) 

ink_sho_al = ink_sho_al[order(ink_sho_al$SHO,
                              decreasing = TRUE), ]

ink_sho_al = Reduce(rbind,                                
                    by(ink_sho_al,
                       ink_sho_al["yearID"],
                       head,
                       n = 1))

ink_sho_nl = Pitching %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(SHO = sum(SHO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SHO) %>%
  arrange(yearID, desc(SHO))

ink_sho_nl = ink_sho_nl[order(ink_sho_nl$SHO,
                              decreasing = TRUE), ]

ink_sho_nl = Reduce(rbind,                                
                    by(ink_sho_nl,
                       ink_sho_nl["yearID"],
                       head,
                       n = 1))
sho_bi = rbind(ink_sho_al, ink_sho_nl)

sho_pts = hof_2025 %>%
  inner_join(sho_bi) %>%
  group_by(name) %>%
  mutate(sho_pts = n()) %>%
  unique()

hof_2025 = hof_2025 
hof_2025[2, 'Black_Ink'] = 4 + 4 + 4 + 9 # From hr_pts + rbi_pts + ba_pts + slg_pts
hof_2025[3, 'Black_Ink'] = 4 + 4 + 1 # From hr_pts + rbi_pts + ab_pts
hof_2025[6, 'Black_Ink'] = 2 + 2 # From x2b_pts + g_pts
hof_2025[9, 'Black_Ink'] = 20 + 8 + 4 + 15 + 3 + 12 + 2 + 1 # From hr_pts + 
# rbi_pts + ba_pts + r_pts + h_pts + slg_pts + x2b_pts + ab_pts
hof_2025[10, 'Black_Ink'] = 3 + 4 # From r_pts + ab_pts
hof_2025[11, 'Black_Ink'] = 1 # From g_pts
hof_2025[13, 'Black_Ink'] = 3 # From r_pts
hof_2025[15, 'Black_Ink'] = 8 + 18 + 2 + 1 + 8 # From ba_pts + h_pts + sb_pts + g_pts + ab_pts
hof_2025[17, 'Black_Ink'] = 6 + 3 + 2 # From r_pts + h_pts + x2b_pts
hof_2025[20, 'Black_Ink'] = 1 # From ab_pts
hof_2025[22, 'Black_Ink'] = 4 + 3 # From ba_pts + r_pts
hof_2025[23, 'Black_Ink'] = 4 + 3 # From rbi_pts + r_pts
hof_2025[5, 'Black_Ink'] = 4   # From w_pts
hof_2025[16, 'Black_Ink'] = 4 + 3  + 4 + 2 + 1 # From w_pts + ip_pts + cg_pts + hp9_pts + sho_pts
hof_2025[21, 'Black_Ink'] = 4 + 8 + 3 + 3 + 6 + 1 + 1 # From w_pts + era_pts + ip_pts + win_pct_pts + hp9_pts + gs_pts + sho_pts
hof_2025[7, 'Black_Ink'] = 6 + 2  + 2 + 2 # From ip_pts + cg_pts + bbp9_pts
hof_2025[18, 'Black_Ink'] = 3   # From sv_pts
hof_2025[12, 'Black_Ink'] = 9   # From sv_pts

hof_2025 = hof_2025 %>%
  mutate(Avg_Black_Ink = 27, comp_to_avg_bi = round(Black_Ink / Avg_Black_Ink, 2)) %>%
  replace_na(list(Black_Ink = 0, comp_to_avg_bi = 0))
hof_2025[1, 'Avg_Black_Ink'] = 40
hof_2025[5, 'Avg_Black_Ink'] = 40
hof_2025[7, 'Avg_Black_Ink'] = 40
hof_2025[12, 'Avg_Black_Ink'] = 40
hof_2025[16, 'Avg_Black_Ink'] = 40
hof_2025[18, 'Avg_Black_Ink'] = 40
hof_2025[21, 'Avg_Black_Ink'] = 40

hof_2025 = hof_2025[order(hof_2025$HOF_pts), ]

season_pts_bat = Batting %>%
  left_join(names) %>%
  filter(name == 'Alex Rodriguez' | name == 'Andruw Jones' | name == 'Bobby Abreu' |
           name == 'Brian McCann' | name == 'Carlos Beltran' | name == 'Chase Utley' |
           name == 'Curtis Granderson' | name == 'David Wright' | name == 'Dustin Pedroia' |
           name == 'Hanley Ramirez' | name == 'Ian Kinsler' | name == 'Ichiro Suzuki' |
           name == 'Jimmy Rollins' | name  == 'Manny Ramirez' | name == 'Omar Vizquel' |
           name == 'Russell Martin' | name == 'Torii Hunter' | name == 'Troy Tulowitzki') %>%
  group_by(playerID, yearID) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = round(H / AB, 3)) %>%
  select(playerID, name, 6:22, 24)
season_pts_bat = season_pts_bat[, -c(1)]

ba_300 = season_pts_bat %>%
  filter(BA > 0.300 & BA <= 0.350 & G >= 100) %>%
  group_by(name) %>%
  mutate(BA300_pts = n()*2.5) %>%
  select(playerID, name, BA300_pts) %>%
  unique()

ba_350 = season_pts_bat %>%
  filter(BA > 0.350 & BA <= 0.400 & G >= 100) %>%
  group_by(name) %>%
  mutate(BA350_pts = n()*5) %>%
  select(playerID, name, BA350_pts) %>%
  unique()

ba_400 = season_pts_bat %>%
  filter(BA > 0.400 & G >= 100) %>%
  group_by(name) %>%
  mutate(BA400_pts = n()*15) %>%
  select(playerID, name, BA400_pts) %>%
  unique()

rbi_pts = season_pts_bat %>%
  filter(RBI >= 100) %>%
  group_by(name) %>%
  mutate(RBI_pts = n()*3) %>%
  select(playerID, name, RBI_pts) %>%
  unique()

hit_pts = season_pts_bat %>%
  filter(H >= 200) %>%
  group_by(name) %>%
  mutate(Hit_pts = n()*5) %>%
  select(playerID, name, Hit_pts) %>%
  unique()

r_pts = season_pts_bat %>%
  filter(R >= 100) %>%
  group_by(name) %>%
  mutate(R_pts = n()*3) %>%
  select(playerID, name, R_pts) %>%
  unique()

hr_30 = season_pts_bat %>%
  filter(HR >= 30 & HR < 40) %>%
  group_by(name) %>%
  mutate(HR_30pts = n()*2) %>%
  select(playerID, name, HR_30pts) %>%
  unique()

hr_40 = season_pts_bat %>%
  filter(HR >= 40 & HR < 50) %>%
  group_by(name) %>%
  mutate(HR_40pts = n()*4) %>%
  select(playerID, name, HR_40pts) %>%
  unique()

hr_50 = season_pts_bat %>%
  filter(HR >= 50) %>%
  group_by(name) %>%
  mutate(HR_50pts = n()*10) %>%
  select(playerID, name, HR_50pts) %>%
  unique()

x2b_35 = season_pts_bat %>%
  filter(X2B >= 35) %>%
  group_by(name) %>%
  mutate(X2B_35pts = n()) %>%
  select(playerID, name, X2B_35pts) %>%
  unique()

x2b_45 = season_pts_bat %>%
  filter(X2B >= 45) %>%
  group_by(name) %>%
  mutate(X2B_45pts = n()*2) %>%
  select(playerID, name, X2B_45pts) %>%
  unique()

bat_pts = hof_2025 %>%
  left_join(ba_300) %>%
  left_join(ba_350) %>%
  left_join(hit_pts) %>%
  left_join(rbi_pts) %>%
  left_join(r_pts) %>%
  left_join(hr_30) %>%
  left_join(hr_40) %>%
  left_join(hr_50) %>%
  left_join(x2b_35) %>%
  left_join(x2b_45) 
  
mvp = AwardsPlayers %>%
  left_join(names) %>%
  filter(name == 'Alex Rodriguez' | name == 'Andruw Jones' | name == 'Bobby Abreu' |
           name == 'Brian McCann' | name == 'Carlos Beltran' | name == 'Chase Utley' |
           name == 'Curtis Granderson' | name == 'David Wright' | name == 'Dustin Pedroia' |
           name == 'Hanley Ramirez' | name == 'Ian Kinsler' | name == 'Ichiro Suzuki' |
           name == 'Jimmy Rollins' | name  == 'Manny Ramirez' | name == 'Omar Vizquel' |
           name == 'Russell Martin' | name == 'Torii Hunter' | name == 'Troy Tulowitzki') %>%
  filter(awardID == 'Most Valuable Player') %>%
  group_by(name) %>%
  mutate(MVP_Pts = 8*n()) %>%
  select(playerID, name, MVP_Pts) %>%
  unique()

as = AllstarFull %>%
  left_join(names) %>%
  filter(name == 'Alex Rodriguez' | name == 'Andruw Jones' | name == 'Bobby Abreu' |
           name == 'Brian McCann' | name == 'Carlos Beltran' | name == 'Chase Utley' |
           name == 'Curtis Granderson' | name == 'David Wright' | name == 'Dustin Pedroia' |
           name == 'Hanley Ramirez' | name == 'Ian Kinsler' | name == 'Ichiro Suzuki' |
           name == 'Jimmy Rollins' | name  == 'Manny Ramirez' | name == 'Omar Vizquel' |
           name == 'Russell Martin' | name == 'Torii Hunter' | name == 'Troy Tulowitzki') %>%
  group_by(name) %>%
  mutate(AS_Pts = 3*n()) %>%
  select(playerID, name, AS_Pts) %>%
  unique()

roy = AwardsPlayers %>%
  left_join(names) %>%
  filter(name == 'Alex Rodriguez' | name == 'Andruw Jones' | name == 'Bobby Abreu' |
           name == 'Brian McCann' | name == 'Carlos Beltran' | name == 'Chase Utley' |
           name == 'Curtis Granderson' | name == 'David Wright' | name == 'Dustin Pedroia' |
           name == 'Hanley Ramirez' | name == 'Ian Kinsler' | name == 'Ichiro Suzuki' |
           name == 'Jimmy Rollins' | name  == 'Manny Ramirez' | name == 'Omar Vizquel' |
           name == 'Russell Martin' | name == 'Torii Hunter' | name == 'Troy Tulowitzki') %>%
  filter(awardID == 'Rookie of the Year') %>%
  group_by(name) %>%
  mutate(ROY_Pts = n()) %>%
  select(playerID, name, ROY_Pts) %>%
  unique()

award_pts = rbind(mvp, as, roy) %>%
  replace_na(list(MVP_Pts = 0, AS_Pts = 0, ROY_Pts = 0)) %>%
  group_by(name) %>%
  mutate(MVP_Pts = sum(MVP_Pts), AS_Pts = sum(AS_Pts), ROY_Pts = sum(ROY_Pts)) %>%
  group_by(name) %>%
  mutate(Award_Pts = MVP_Pts + AS_Pts + ROY_Pts) %>%
  select(name, Award_Pts) %>%
  unique()

gg_pts = AwardsPlayers %>%
  left_join(names) %>%
  filter(name == 'Alex Rodriguez' | name == 'Andruw Jones' | name == 'Bobby Abreu' |
           name == 'Brian McCann' | name == 'Carlos Beltran' | name == 'Chase Utley' |
           name == 'Curtis Granderson' | name == 'David Wright' | name == 'Dustin Pedroia' |
           name == 'Hanley Ramirez' | name == 'Ian Kinsler' | name == 'Ichiro Suzuki' |
           name == 'Jimmy Rollins' | name  == 'Manny Ramirez' | name == 'Omar Vizquel' |
           name == 'Russell Martin' | name == 'Torii Hunter' | name == 'Troy Tulowitzki') %>%
  filter(awardID == 'Gold Glove') %>%
  group_by(name) %>%
  mutate(GG_Pts = n()) %>%
  select(playerID, name, GG_Pts) %>%
  unique()

ws_pts = SeriesPost %>%
  filter(round == 'WS' & yearID >= 1989) %>%
  rename(teamID = teamIDwinner) %>%
  select(1:3) %>%
  inner_join(Batting) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(ws_pts = n()) %>%
  select(name, ws_pts) %>%
  unique()
ws_pts = ws_pts[-c(1,2,4,9,14), ]
ws_pts[1, 'ws_pts'] = 2*3
ws_pts[2, 'ws_pts'] = 3*5
ws_pts[3, 'ws_pts'] = 1*6
ws_pts[4, 'ws_pts'] = 1*5
ws_pts[5, 'ws_pts'] = 1*3
ws_pts[6, 'ws_pts'] = 1*1
ws_pts[7, 'ws_pts'] = 1*6
ws_pts[8, 'ws_pts'] = 1*5
ws_pts[9, 'ws_pts'] = 1*6

lcs_pts = SeriesPost %>%
  filter(round == 'WS' & yearID >= 1989) %>%
  rename(teamID = teamIDloser) %>%
  select(1, 2, 5) %>%
  inner_join(Batting) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(lcs_pts = n()) %>%
  select(name, lcs_pts) %>%
  unique()
lcs_pts = lcs_pts[-c(4,6), ]
lcs_pts[1, 'lcs_pts'] = 2*1
lcs_pts[2, 'lcs_pts'] = 2*5
lcs_pts[3, 'lcs_pts'] = 2*1
lcs_pts[4, 'lcs_pts'] = 3*5
lcs_pts[5, 'lcs_pts'] = 1*5
lcs_pts[6, 'lcs_pts'] = 1*1
lcs_pts[7, 'lcs_pts'] = 3*3
lcs_pts[8, 'lcs_pts'] = 2*3
lcs_pts[9, 'lcs_pts'] = 1*1
lcs_pts[10, 'lcs_pts'] = 1*1


div_pts = SeriesPost %>%
  filter((round == 'ALCS' | round == 'NLCS') & yearID >= 1989) %>%
  rename(teamID = teamIDloser) %>%
  select(1, 2, 5) %>%
  inner_join(Batting) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(div_pts = n()) %>%
  select(name, div_pts) %>%
  unique()
div_pts = div_pts[-c(8:10, 12, 20, 22), ]
div_pts[1, 'div_pts'] = 5
div_pts[2, 'div_pts'] = 5
div_pts[3, 'div_pts'] = 5
div_pts[4, 'div_pts'] = 1*2
div_pts[5, 'div_pts'] = 2
div_pts[6, 'div_pts'] = 3
div_pts[7, 'div_pts'] = 3
div_pts[8, 'div_pts'] = 1
div_pts[9, 'div_pts'] = 1
div_pts[10, 'div_pts'] = 5*2
div_pts[11, 'div_pts'] = 1
div_pts[12, 'div_pts'] = 3
div_pts[13, 'div_pts'] = 1*2
div_pts[14, 'div_pts'] = 2
div_pts[15, 'div_pts'] = 1*2
div_pts[16, 'div_pts'] = 3*2
div_pts[17, 'div_pts'] = 1*2

ba_mon = rbind(ink_ba_al, ink_ba_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(ba_pts = n()*6) %>%
  select(playerID, name, ba_pts) %>%
  unique()

hr_mon = rbind(ink_hr_al, ink_hr_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(hr_pts = n()*4) %>%
  select(playerID, name, hr_pts) %>%
  unique()

rbi_mon = rbind(ink_rbi_al, ink_rbi_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(rbi_pts = n()*4) %>%
  select(playerID, name, rbi_pts) %>%
  unique()

r_mon = rbind(ink_r_al, ink_r_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(r_pts = n()*3) %>%
  select(playerID, name, r_pts) %>%
  unique()

h_mon = rbind(ink_h_al, ink_h_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(h_pts = n()*2) %>%
  select(playerID, name, h_pts) %>%
  unique()

sb_mon = rbind(ink_sb_al, ink_sb_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(sb_pts = n()*2) %>%
  select(playerID, name, sb_pts) %>%
  unique()

x2b_mon = rbind(ink_x2b_al, ink_x2b_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(x2b_pts = n()) %>%
  select(playerID, name, x2b_pts) %>%
  unique()

ink_x3b_al = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(X3B = sum(X3B)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, X3B) %>%
  arrange(yearID, desc(X3B)) 

ink_x3b_al = ink_x3b_al[order(ink_x3b_al$X3B,
                            decreasing = TRUE), ]
ink_x3b_al = Reduce(rbind,                                
                   by(ink_x3b_al,
                      ink_x3b_al["yearID"],
                      head,
                      n = 1))

ink_x3b_nl = Batting %>%
  filter(yearID >= 1989 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(X3B = sum(X3B)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, X3B) %>%
  arrange(yearID, desc(X3B))

ink_x3b_nl = ink_x3b_nl[order(ink_x3b_nl$X3B,
                            decreasing = TRUE), ]

ink_x3b_nl = Reduce(rbind,                                
                   by(ink_x3b_nl,
                      ink_x3b_nl["yearID"],
                      head,
                      n = 1))

x3b_mon = rbind(ink_x3b_al, ink_x3b_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(x3b_pts = n()) %>%
  select(playerID, name, x3b_pts) %>%
  unique()

career_bat = Batting %>%
  left_join(names) %>%
  left_join(Appearances) %>%
  filter(name == 'Alex Rodriguez' | name == 'Andruw Jones' | name == 'Bobby Abreu' |
           name == 'Brian McCann' | name == 'Carlos Beltran' | name == 'Chase Utley' |
           name == 'Curtis Granderson' | name == 'David Wright' | name == 'Dustin Pedroia' |
           name == 'Hanley Ramirez' | name == 'Ian Kinsler' | name == 'Ichiro Suzuki' |
           name == 'Jimmy Rollins' | name  == 'Manny Ramirez' | name == 'Omar Vizquel' |
           name == 'Russell Martin' | name == 'Torii Hunter' | name == 'Troy Tulowitzki') %>%
  group_by(playerID) %>%
  mutate(G = sum(G), AB = sum(AB), R = sum(R), H = sum(H), X2B = sum(X2B), X3B = sum(X3B),
         HR = sum(HR), RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB), SO = sum(SO),
         IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         BA = round(H / AB, 3), G_c = sum(G_c), G_1b = sum(G_1b), G_2b = sum(G_2b),
         G_3b = sum(G_3b), G_ss = sum(G_ss), G_lf = sum(G_lf), G_cf = sum(G_cf), 
         G_rf = sum(G_rf)) %>%
  select(playerID, name, 6:22, BA, 29:36) %>%
  unique()

h_2000 = career_bat %>%
  filter(H >= 2000 & H < 2500) %>%
  group_by(name) %>%
  mutate(pts_2000 = n()*4) %>%
  select(playerID, name, pts_2000)

h_2500 = career_bat %>%
  filter(H >= 2500 & H < 3000) %>%
  group_by(name) %>%
  mutate(pts_2500 = n()*15) %>%
  select(playerID, name, pts_2500)

h_3000 = career_bat %>%
  filter(H >= 3000 & H < 3500) %>%
  group_by(name) %>%
  mutate(pts_3000 = n()*40) %>%
  select(playerID, name, pts_3000)

h_3500 = career_bat %>%
  filter(H >= 3500) %>%
  group_by(name) %>%
  mutate(pts_3500 = n()*50) %>%
  select(playerID, name, pts_3500)

hr_300 = career_bat %>%
  filter(HR >= 300 & HR < 400) %>%
  group_by(name) %>%
  mutate(hr_300 = n()*3) %>%
  select(playerID, name, hr_300)

hr_400 = career_bat %>%
  filter(HR >= 400 & HR < 500) %>%
  group_by(name) %>%
  mutate(hr_400 = n()*10) %>%
  select(playerID, name, hr_400)

hr_500 = career_bat %>%
  filter(HR >= 500 & HR < 600) %>%
  group_by(name) %>%
  mutate(hr_500 = n()*20) %>%
  select(playerID, name, hr_500)

hr_600 = career_bat %>%
  filter(HR >= 600) %>%
  group_by(name) %>%
  mutate(hr_600 = n()*30) %>%
  select(playerID, name, hr_600)

ba.300 = career_bat %>%
  filter(BA > 0.300 & BA <= 0.315) %>%
  group_by(name) %>%
  mutate(ba.300 = n()*8) %>%
  select(playerID, name, ba.300)

c_1600 = career_bat %>%
  filter(G_c >= 1600) %>%
  group_by(name) %>%
  mutate(C_1600pts = n()*45) %>%
  select(playerID, name, C_1600pts)

c_1400 = career_bat %>%
  filter(G_c >= 1400) %>%
  group_by(name) %>%
  mutate(C_1400pts = n()*30) %>%
  select(playerID, name, C_1400pts) 

second_1800 = career_bat %>%
  filter(G_2b >= 1800 & G_2b <= 2100) %>%
  group_by(name) %>%
  mutate(G_2bpts = n()*15) %>%
  select(playerID, name, G_2bpts)

short_1800 = career_bat %>%
  filter(G_ss >= 1800 & G_ss <= 2100) %>%
  group_by(name) %>%
  mutate(G_sspts = n()*15) %>%
  select(playerID, name, G_sspts)

second_2100 = career_bat %>%
  filter(G_2b > 2100) %>%
  group_by(name) %>%
  mutate(G_2bpts2100 = n()*30) %>%
  select(playerID, name, G_2bpts2100)

short_2100 = career_bat %>%
  filter(G_ss > 2100) %>%
  group_by(name) %>%
  mutate(G_sspts2100 = n()*30) %>%
  select(playerID, name, G_sspts2100)

third_2000 = career_bat %>%
  filter(G_3b > 2000) %>%
  group_by(name) %>%
  mutate(G_3bpts = n()*15) %>%
  select(playerID, name, G_3bpts)

inf = career_bat %>%
  filter(G_2b > 2500 | G_3b > 2500 | G_ss > 2500) %>%
  group_by(name) %>%
  mutate(inf_pts = n()*15) %>%
  select(playerID, name, inf_pts)

ba_inf = career_bat %>%
  filter(BA > 0.275 & (G_2b > 1500 | G_ss > 1500 | G_3b > 1500)) %>%
  group_by(name) %>%
  mutate(both_pts = n()*15) %>%
  select(playerID, name, both_pts)

season_pit = Pitching %>%
  left_join(names) %>%
  left_join(Appearances) %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  group_by(playerID, yearID) %>%
  mutate(W = sum(W), L = sum(L), G = sum(G), GS = sum(GS), CG = sum(CG), SHO = sum(SHO),
         SV = sum(SV), IPouts = sum(IPouts), H = sum(H), ER = sum(ER), BB = sum(BB), SO = sum(SO),
         HR = sum(HR), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         IBB = sum(IBB), WP = sum(WP), BK = sum(BK), BFP = sum(BFP),
         GF = sum(GF), R = sum(R), Win_pct = round(W / (W+L), 3), IP = round(IPouts / 3, 2),
         BAOpp = round(H / BFP, 3), ERA = round((9*ER) / IP, 2)) %>%
  select(playerID, name, 6:30, 48, 49)

career_pit = Pitching %>%
  left_join(names) %>%
  left_join(Appearances) %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  group_by(playerID) %>%
  mutate(W = sum(W), L = sum(L), G = sum(G), GS = sum(GS), CG = sum(CG), SHO = sum(SHO),
         SV = sum(SV), IPouts = sum(IPouts), H = sum(H), ER = sum(ER), BB = sum(BB), SO = sum(SO),
         HR = sum(HR), HBP = sum(HBP), SH = sum(SH), SF = sum(SF), GIDP = sum(GIDP),
         IBB = sum(IBB), WP = sum(WP), BK = sum(BK), BFP = sum(BFP),
         GF = sum(GF), R = sum(R), Win_pct = round(W / (W+L), 3), IP = round(IPouts / 3, 2),
         BAOpp = round(H / BFP, 3), ERA = round((9*ER) / IP, 2), Decisions = W + L) %>%
  select(playerID, name, 6:30, 48, 49, 50) %>%
  unique()
career_pit = career_pit[-c(8), ]

w_15 = season_pit %>%
  left_join(names) %>%
  filter(W >= 15 & W < 18) %>%
  group_by(name) %>%
  mutate(w_15pts = n()*2) %>%
  select(playerID, name, w_15pts) %>%
  unique()

w_18 = season_pit %>%
  left_join(names) %>%
  filter(W >= 18 & W < 20) %>%
  group_by(name) %>%
  mutate(w_18pts = n()*4) %>%
  select(playerID, name, w_18pts) %>%
  unique()

w_20 = season_pit %>%
  left_join(names) %>%
  filter(W >= 20 & W < 23) %>%
  group_by(name) %>%
  mutate(w_20pts = n()*6) %>%
  select(playerID, name, w_20pts) %>%
  unique()

so_200 = season_pit %>%
  left_join(names) %>%
  filter(SO >= 200 & SO < 250) %>%
  group_by(name) %>%
  mutate(so_200pts = n()*2) %>%
  select(playerID, name, so_200pts) %>%
  unique()

so_250 = season_pit %>%
  left_join(names) %>%
  filter(SO >= 250 & SO < 300) %>%
  group_by(name) %>%
  mutate(so_250pts = n()*3) %>%
  select(playerID, name, so_250pts) %>%
  unique()

w_winpct = season_pit %>%
  left_join(names) %>%
  filter(W >= 14 & Win_pct >= 0.700) %>%
  group_by(name) %>%
  mutate(w_wpct = n()*2) %>%
  select(playerID, name, w_wpct) %>%
  unique()

era_3 = season_pit %>%
  left_join(names) %>%
  filter(ERA < 3.00 & IP >= 150) %>%
  group_by(name) %>%
  mutate(era_3pts = n()) %>%
  select(playerID, name, era_3pts) %>%
  unique()

era_2 = season_pit %>%
  left_join(names) %>%
  filter(ERA < 2.00 & IP >= 150) %>%
  group_by(name) %>%
  mutate(era_2pts = n()*4) %>%
  select(playerID, name, era_2pts) %>%
  unique()

sv_20 = season_pit %>%
  left_join(names) %>%
  filter(SV >= 20 & SV < 30) %>%
  group_by(name) %>%
  mutate(sv_20pts = n()) %>%
  select(playerID, name, sv_20pts) %>%
  unique()

sv_30 = season_pit %>%
  left_join(names) %>%
  filter(SV >= 30 & SV < 40) %>%
  group_by(name) %>%
  mutate(sv_30pts = n()*4) %>%
  select(playerID, name, sv_30pts) %>%
  unique()

sv_40 = season_pit %>%
  left_join(names) %>%
  filter(SV >= 40) %>%
  group_by(name) %>%
  mutate(sv_40pts = n()*7) %>%
  select(playerID, name, sv_40pts) %>%
  unique()

mvp_pit = AwardsPlayers %>%
  left_join(names) %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  filter(awardID == 'Most Valuable Player') %>%
  group_by(name) %>%
  mutate(pit_mvp_pts = 8*n()) %>%
  select(playerID, name, pit_mvp_pts) %>%
  unique()

as_pit = AllstarFull %>%
  left_join(names) %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  group_by(name) %>%
  mutate(pit_as_pts = 3*n()) %>%
  select(playerID, name, pit_as_pts) %>%
  unique()

roy_pit = AwardsPlayers %>%
  left_join(names) %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  filter(awardID == 'Rookie of the Year') %>%
  group_by(name) %>%
  mutate(pit_roy_pts = n()) %>%
  select(playerID, name, pit_roy_pts) %>%
  unique()

cy_pit = AwardsPlayers %>%
  left_join(names) %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  filter(awardID == 'Cy Young Award') %>%
  group_by(name) %>%
  mutate(pit_cy_pts = n()*5) %>%
  select(playerID, name, pit_cy_pts) %>%
  unique()

gg_pit = AwardsPlayers %>%
  left_join(names) %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  filter(awardID == 'Gold Glove') %>%
  group_by(name) %>%
  mutate(pit_gg_pts = n()) %>%
  select(playerID, name, pit_gg_pts) %>%
  unique()

pit_era_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(IP = sum(IPouts) / 3, ERA = round((9*sum(ER)) / (IP), 2)) %>%
  filter(IP > 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, ERA) %>%
  arrange(yearID, ERA) 

pit_era_al = pit_era_al[order(pit_era_al$ERA,
                              decreasing = FALSE), ]
pit_era_al = Reduce(rbind,                                
                    by(pit_era_al,
                       pit_era_al["yearID"],
                       head,
                       n = 1))

pit_era_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(IP = sum(IPouts) / 3, ERA = round((9*sum(ER)) / (IP), 2)) %>%
  filter(IP > 162) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, ERA) %>%
  arrange(yearID, ERA) 

pit_era_nl = pit_era_nl[order(pit_era_nl$ERA,
                              decreasing = FALSE), ]
pit_era_nl = Reduce(rbind,                                
                    by(pit_era_nl,
                       pit_era_nl["yearID"],
                       head,
                       n = 1))

era_mon = rbind(pit_era_al, pit_era_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(era_pts = n()*2) %>%
  select(playerID, name, era_pts) %>%
  unique()

pit_g_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(G = sum(G)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, G) %>%
  arrange(yearID, G) 

pit_g_al = pit_g_al[order(pit_g_al$G,
                              decreasing = TRUE), ]
pit_g_al = Reduce(rbind,                                
                    by(pit_g_al,
                       pit_g_al["yearID"],
                       head,
                       n = 1))

pit_g_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(G = sum(G)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, G) %>%
  arrange(yearID, G) 

pit_g_nl = pit_g_nl[order(pit_g_nl$G,
                              decreasing = TRUE), ]
pit_g_nl = Reduce(rbind,                                
                    by(pit_g_nl,
                       pit_g_nl["yearID"],
                       head,
                       n = 1))

g_mon = rbind(pit_g_al, pit_g_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(g_pts = n()) %>%
  select(playerID, name, g_pts) %>%
  unique()

pit_w_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(W = sum(W)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, W) %>%
  arrange(yearID, W) 

pit_w_al = pit_w_al[order(pit_w_al$W,
                          decreasing = TRUE), ]
pit_w_al = Reduce(rbind,                                
                  by(pit_w_al,
                     pit_w_al["yearID"],
                     head,
                     n = 1))

pit_w_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(W = sum(W)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, W) %>%
  arrange(yearID, W) 

pit_w_nl = pit_w_nl[order(pit_w_nl$W,
                          decreasing = TRUE), ]
pit_w_nl = Reduce(rbind,                                
                  by(pit_w_nl,
                     pit_w_nl["yearID"],
                     head,
                     n = 1))

w_mon = rbind(pit_w_al, pit_w_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(w_pts = n()) %>%
  select(playerID, name, w_pts) %>%
  unique()

pit_ip_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(IP = sum(IPouts) / 3) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, IP) %>%
  arrange(yearID, IP) 

pit_ip_al = pit_ip_al[order(pit_ip_al$IP,
                          decreasing = TRUE), ]
pit_ip_al = Reduce(rbind,                                
                  by(pit_ip_al,
                     pit_ip_al["yearID"],
                     head,
                     n = 1))

pit_ip_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(IP = sum(IPouts) / 3) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, IP) %>%
  arrange(yearID, IP) 

pit_ip_nl = pit_ip_nl[order(pit_ip_nl$IP,
                          decreasing = TRUE), ]
pit_ip_nl = Reduce(rbind,                                
                  by(pit_ip_nl,
                     pit_ip_nl["yearID"],
                     head,
                     n = 1))

ip_mon = rbind(pit_ip_al, pit_ip_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(ip_pts = n()) %>%
  select(playerID, name, ip_pts) %>%
  unique()

pit_winpct_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(Decisions = sum(W) + sum(L), Win_pct = sum(W) / (Decisions)) %>%
  filter(Decisions >= 16) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, Win_pct) %>%
  arrange(yearID, Win_pct) 

pit_winpct_al = pit_winpct_al[order(pit_winpct_al$Win_pct,
                            decreasing = TRUE), ]
pit_winpct_al = Reduce(rbind,                                
                   by(pit_winpct_al,
                      pit_winpct_al["yearID"],
                      head,
                      n = 1))

pit_winpct_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(Decisions = sum(W) + sum(L), Win_pct = sum(W) / (Decisions)) %>%
  filter(Decisions >= 16) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, Win_pct) %>%
  arrange(yearID, Win_pct) 

pit_winpct_nl = pit_winpct_nl[order(pit_winpct_nl$Win_pct,
                            decreasing = TRUE), ]
pit_winpct_nl = Reduce(rbind,                                
                   by(pit_winpct_nl,
                      pit_winpct_nl["yearID"],
                      head,
                      n = 1))

winpct_mon = rbind(pit_winpct_al, pit_winpct_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(winpct_pts = n()) %>%
  select(playerID, name, winpct_pts) %>%
  unique()

pit_so_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(SO = sum(SO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SO) %>%
  arrange(yearID, SO) 

pit_so_al = pit_so_al[order(pit_so_al$SO,
                                    decreasing = TRUE), ]
pit_so_al = Reduce(rbind,                                
                       by(pit_so_al,
                          pit_so_al["yearID"],
                          head,
                          n = 1))

pit_so_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(SO = sum(SO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SO) %>%
  arrange(yearID, SO) 

pit_so_nl = pit_so_nl[order(pit_so_nl$SO,
                                    decreasing = TRUE), ]
pit_so_nl = Reduce(rbind,                                
                       by(pit_so_nl,
                          pit_so_nl["yearID"],
                          head,
                          n = 1))

so_mon = rbind(pit_so_al, pit_so_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(so_pts = n()) %>%
  select(playerID, name, so_pts) %>%
  unique()

pit_sv_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(SV = sum(SV)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SV) %>%
  arrange(yearID, SV) 

pit_sv_al = pit_sv_al[order(pit_sv_al$SV,
                            decreasing = TRUE), ]
pit_sv_al = Reduce(rbind,                                
                   by(pit_sv_al,
                      pit_sv_al["yearID"],
                      head,
                      n = 1))

pit_sv_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(SV = sum(SV)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SV) %>%
  arrange(yearID, SV) 

pit_sv_nl = pit_sv_nl[order(pit_sv_nl$SV,
                            decreasing = TRUE), ]
pit_sv_nl = Reduce(rbind,                                
                   by(pit_sv_nl,
                      pit_sv_nl["yearID"],
                      head,
                      n = 1))

sv_mon = rbind(pit_sv_al, pit_sv_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(sv_pts = n()) %>%
  select(playerID, name, sv_pts) %>%
  unique()

pit_sho_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(SHO = sum(SHO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SHO) %>%
  arrange(yearID, SHO) 

pit_sho_al = pit_sho_al[order(pit_sho_al$SHO,
                            decreasing = TRUE), ]
pit_sho_al = Reduce(rbind,                                
                   by(pit_sho_al,
                      pit_sho_al["yearID"],
                      head,
                      n = 1))

pit_sho_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(SHO = sum(SHO)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, SHO) %>%
  arrange(yearID, SHO) 

pit_sho_nl = pit_sho_nl[order(pit_sho_nl$SHO,
                            decreasing = TRUE), ]
pit_sho_nl = Reduce(rbind,                                
                   by(pit_sho_nl,
                      pit_sho_nl["yearID"],
                      head,
                      n = 1))

sho_mon = rbind(pit_sho_al, pit_sho_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(sho_pts = n()) %>%
  select(playerID, name, sho_pts) %>%
  unique()



pit_cg_al = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'AL') %>%
  group_by(playerID, yearID) %>%
  mutate(CG = sum(CG)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, CG) %>%
  arrange(yearID, CG) 

pit_cg_al = pit_cg_al[order(pit_cg_al$CG,
                              decreasing = TRUE), ]
pit_cg_al = Reduce(rbind,                                
                    by(pit_cg_al,
                       pit_cg_al["yearID"],
                       head,
                       n = 1))

pit_cg_nl = Pitching %>%
  filter(yearID >= 1995 & yearID <= 2019 & lgID == 'NL') %>%
  group_by(playerID, yearID) %>%
  mutate(CG = sum(CG)) %>%
  left_join(names) %>%
  select(playerID, name, lgID, yearID, CG) %>%
  arrange(yearID, CG) 

pit_cg_nl = pit_cg_nl[order(pit_cg_nl$CG,
                              decreasing = TRUE), ]
pit_cg_nl = Reduce(rbind,                                
                    by(pit_cg_nl,
                       pit_cg_nl["yearID"],
                       head,
                       n = 1))

cg_mon = rbind(pit_cg_al, pit_cg_nl) %>%
  inner_join(hof_2025) %>%
  group_by(name) %>%
  mutate(cg_pts = n()*0.5) %>%
  select(playerID, name, cg_pts) %>%
  unique()


win_150 = career_pit %>%
  left_join(names) %>%
  filter(W >= 150 & W < 175) %>%
  group_by(name) %>%
  mutate(win_150pts = n()*5) %>%
  select(playerID, name, win_150pts) %>%
  unique()

win_200 = career_pit %>%
  left_join(names) %>%
  filter(W >= 200 & W < 225) %>%
  group_by(name) %>%
  mutate(win_200pts = n()*10) %>%
  select(playerID, name, win_200pts) %>%
  unique()

win_250 = career_pit %>%
  left_join(names) %>%
  filter(W >= 250) %>%
  group_by(name) %>%
  mutate(win_250pts = n()*20) %>%
  select(playerID, name, win_250pts) %>%
  unique()

win_pct550 = career_pit %>%
  left_join(names) %>%
  filter(Win_pct >= 0.550 & Win_pct < 0.575) %>%
  filter(Decisions >= 190) %>%
  group_by(name) %>%
  mutate(winpct_550pts = n()) %>%
  select(playerID, name, winpct_550pts) %>%
  unique()

win_pct600 = career_pit %>%
  left_join(names) %>%
  filter(Win_pct >= 0.600) %>%
  filter(Decisions >= 190) %>%
  group_by(name) %>%
  mutate(winpct_600pts = n()*5) %>%
  select(playerID, name, winpct_600pts) %>%
  unique()

sub3 = career_pit %>%
  left_join(names) %>%
  filter(ERA < 3.00) %>%
  filter(Decisions >= 190) %>%
  group_by(name) %>%
  mutate(sub_3pts = n()*10) %>%
  select(playerID, name, sub_3pts) %>%
  unique()

sv_300 = career_pit %>%
  left_join(names) %>%
  filter(SV >= 300) %>%
  group_by(name) %>%
  mutate(sv_300pts = n()*20) %>%
  select(playerID, name, sv_300pts) %>%
  unique()

g_850 = career_pit %>%
  left_join(names) %>%
  filter(G >= 850) %>%
  group_by(name) %>%
  mutate(g_850pts = n()*20) %>%
  select(playerID, name, g_850pts) %>%
  unique()

so_3000 = career_pit %>%
  left_join(names) %>%
  filter(SO >= 3000) %>%
  group_by(name) %>%
  mutate(so_3000pts = n()*10) %>%
  select(playerID, name, so_3000pts) %>%
  unique()

ws_s = PitchingPost %>%
  left_join(names) %>%
  filter(round == 'WS') %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  group_by(name) %>%
  mutate(GS = sum(GS)) %>%
  mutate(gs_pts = GS*2) %>%
  select(playerID, name, gs_pts) %>%
  unique()

ws_app = PitchingPost %>%
  left_join(names) %>%
  filter(round == 'WS') %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  group_by(name) %>%
  mutate(G = sum(G), GS = sum(GS), App = G - GS) %>%
  mutate(app_pts = App) %>%
  select(playerID, name, app_pts) %>%
  unique()

ws_w = PitchingPost %>%
  left_join(names) %>%
  filter(round == 'WS') %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  group_by(name) %>%
  mutate(W = sum(W)) %>%
  mutate(ws_wpts = W*2) %>%
  select(playerID, name, ws_wpts) %>%
  unique()

po_w = PitchingPost %>%
  left_join(names) %>%
  filter(round == 'ALDS1' | round == 'NLDS1' | round == 'ALDS2' | round == 'NLDS2' |
           round == 'ALCS' | round == 'NLCS') %>%
  filter(name == 'Fernando Rodney' | name == 'Francisco Rodriguez' | name == 'Mark Buehrle' |
           name == 'Billy Wagner' | name == 'Felix Hernandez' | name == 'Andy Pettitte' |
           name == 'CC Sabathia') %>%
  group_by(name) %>%
  mutate(W = sum(W)) %>%
  mutate(po_wpts = W) %>%
  select(playerID, name, po_wpts) %>%
  unique()

monitor = hof_2025 %>%
  left_join(award_pts) %>%
  left_join(bat_pts) %>%
  left_join(gg_pts) %>%
  left_join(ws_pts) %>%
  left_join(lcs_pts) %>%
  left_join(div_pts) %>%
  left_join(ba_mon) %>%
  left_join(hr_mon) %>%
  left_join(rbi_mon) %>%
  left_join(r_mon) %>%
  left_join(h_mon) %>%
  left_join(sb_mon) %>%
  left_join(x2b_mon) %>%
  left_join(x3b_mon) %>%
  left_join(h_2000) %>%
  left_join(h_2500) %>%
  left_join(h_3000) %>%
  left_join(h_3500) %>%
  left_join(hr_300) %>%
  left_join(hr_400) %>%
  left_join(hr_500) %>%
  left_join(hr_600) %>%
  left_join(ba.300) %>%
  left_join(c_1600) %>%
  left_join(c_1400) %>%
  left_join(second_1800) %>%
  left_join(second_2100) %>%
  left_join(short_2100) %>%
  left_join(third_2000) %>%
  left_join(inf) %>%
  left_join(ba_inf) %>%
  left_join(w_15) %>%
  left_join(w_18) %>%
  left_join(w_20) %>%
  left_join(so_200) %>%
  left_join(so_250) %>%
  left_join(w_winpct) %>%
  left_join(era_3) %>%
  left_join(sv_20) %>%
  left_join(sv_30) %>%
  left_join(sv_40) %>%
  left_join(as_pit) %>%
  left_join(cy_pit) %>%
  left_join(gg_pit) %>%
  left_join(era_mon) %>%
  left_join(w_mon) %>%
  left_join(ip_mon) %>%
  left_join(winpct_mon) %>%
  left_join(sv_mon) %>%
  left_join(sho_mon) %>%
  left_join(cg_mon) %>%
  left_join(win_150) %>%
  left_join(win_200) %>%
  left_join(win_250) %>%
  left_join(win_pct550) %>%
  left_join(win_pct600) %>%
  left_join(sv_300) %>%
  left_join(g_850) %>%
  left_join(so_3000) %>%
  left_join(ws_s) %>%
  left_join(ws_app) %>%
  left_join(ws_w) %>%
  left_join(po_w)

monitor = monitor %>%
  select(playerID, name, 12:65)
monitor[is.na(monitor)] = 0

monitor$'Monitor Pts' = rowSums(monitor[3:56])
monitor['Monitor Pts Threshold'] = 100
monitor = monitor %>%
  select(playerID, name, 'Monitor Pts', 'Monitor Pts Threshold')

hof_2025 = hof_2025 %>%
  left_join(monitor)

hitters = hof_2025 %>%
  filter(Position == 'Position')
hitters = hitters[, -c(8, 11)]
hitters = hitters %>%
  rename('Player ID' = playerID, Name = name, 'Years on Ballot' = YoB, 
         'Last Ballot' = Last_Ballot, 'HOF Pts' = HOF_pts, 'HOF Pts Average' = Avg_HOF,
         'Black Ink' = Black_Ink, 'Black Ink Average' = Avg_Black_Ink) %>%
  arrange(Name)
hitters = hitters %>%
  select(1:5, 8, 9, 6, 7, 10, 11)

pitchers = hof_2025 %>%
  filter(Position == 'Pitcher')
pitchers = pitchers[, -c(8, 11)]
pitchers = pitchers %>%
  rename('Player ID' = playerID, Name = name, 'Years on Ballot' = YoB, 
         'Last Ballot' = Last_Ballot, 'HOF Pts' = HOF_pts, 'HOF Pts Average' = Avg_HOF,
         'Black Ink' = Black_Ink, 'Black Ink Average' = Avg_Black_Ink) %>%
  arrange(Name)
pitchers = pitchers %>%
  select(1:5, 8, 9, 6, 7, 10, 11)


ui = fluidPage(
  titlePanel("Likelihood of the 2025 Hall of Fame Ballot Being Inducted"),
  navbarPage(
    "Players",
    tabPanel("Position Players",
             tags$p("Authors: Kaleb Jordan & Graham Dynis"),
             tags$p("The goal of our
             final project is to use Hall of Fame Metric Scores to determine whether or not
             players on the suspected 2025 HOF ballot will be inducted. For this lab, we constructed
             a shiny app that displays the hitters Standard and Black Ink scores compared to the average
             players in the HOF. One interesting thing to note is that these scores do not
             attribute to a player's overall success but rather just hitting. For the final, we
             want to add the HOF Monitor Scores and possibly other statistics to get a better
             understanding of what it takes to be inducted. After that,
             we will state our reasonings of why we believe some players should be inducted.
             We will only account for the stats provided and exclude external factors such as
             steroid usage or other controversies. We will use our findings to support our decisions
             on why we think certain players will make the HOF. The formula for these metrics can be found at",
                    tags$a("this link.", href = "https://www.baseball-reference.com/about/leader_glossary.shtml")),
             sidebarLayout(
               sidebarPanel(
                 selectInput("hitter", "Select a position player:", choices = unique(hitters$Name))
               ),
               mainPanel(
                 h4("Position Player Information:"),
                 tableOutput("hitter_info"),
                 plotOutput("hitter_plot")
               )
             )
    ),
    tabPanel("Pitchers",
             tags$p("Authors: Kaleb Jordan & Graham Dynis"),
             tags$p("The goal of our
             final project is to use Hall of Fame Metric Scores to determine whether or not
             players on the suspected 2025 HOF ballot will be inducted. For this lab, we constructed
             a shiny app that displays the pitchers Standard and Black Ink scores compared to the average
             players in the HOF. Two interesting thing to notes is that these scores do not
             attribute to a player's overall success but rather just pitching and relievers do
             not get a fair comparison to starters which negatively impacts them. For the final, we
             want to add the HOF Monitor Scores and possibly other statistics to get a better
             understanding of what it takes to be inducted. After that,
             we will state our reasonings of why we believe some players should be inducted.
             We will only account for the stats provided and exclude external factors such as
             steroid usage or other controversies. We will use our findings to support our decisions
             on why we think certain players will make the HOF. The formula for these metrics can be found at",
                    tags$a("this link.", href = "https://www.baseball-reference.com/about/leader_glossary.shtml")),
             sidebarLayout(
               sidebarPanel(
                 selectInput("pitcher", "Select a pitcher:", choices = unique(pitchers$Name))
               ),
               mainPanel(
                 h4("Pitcher Information:"),
                 tableOutput("pitcher_info"),
                 plotOutput("pitcher_plot")
               )
             )
    )
  )
)

server = function(input, output, session) {
  selected_hitter = reactive({
    hitters %>% filter(Name == input$hitter)
  })

  output$hitter_info = renderTable({
    selected_hitter()
  })

  output$hitter_plot = renderPlot({
    selected = selected_hitter()
    stats = c("HOF Pts", "HOF Pts Average", "Black Ink", "Black Ink Average", 
               'Monitor Pts', 'Monitor Pts Threshold')
    plot_data = selected %>% 
      select(all_of(stats)) %>% 
      pivot_longer(cols = everything(), names_to = "Statistics", values_to = "value")
    
    ggplot(plot_data, aes(x = Statistics, y = value, fill = Statistics)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("salmon", "lightgreen", "salmon", "lightgreen",
                                   "salmon", "lightgreen")) +
      labs(x = "HOF Metrics", y = "Values", title = "Position Player Statistics") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  selected_pitcher = reactive({
    pitchers %>% filter(Name == input$pitcher)
  })

  output$pitcher_info = renderTable({
    selected_pitcher()
  })

  output$pitcher_plot = renderPlot({
    selected = selected_pitcher()
    stats = c("HOF Pts", "HOF Pts Average", "Black Ink", "Black Ink Average", 
               'Monitor Pts', 'Monitor Pts Threshold')
    plot_data = selected %>% 
      select(all_of(stats)) %>% 
      pivot_longer(cols = everything(), names_to = "Statistics", values_to = "value")
    
    ggplot(plot_data, aes(x = Statistics, y = value, fill = Statistics)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("salmon", "lightgreen", "salmon", "lightgreen",
                                   "salmon", "lightgreen")) +
      labs(x = "HOF Metrics", y = "Values", title = "Pitcher Statistics") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
