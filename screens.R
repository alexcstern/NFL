library(ggthemes)
library(viridis)
library(teamcolors)
library(tidyverse)
library(scater)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

setwd("~/Desktop/nfl2019")
nfl <- read_csv("nfl_2019.csv")
# yards_gained, pass_length?, air_yards, yards_after_catch, epa, 
# air yards vs epa (lines for air_epa and yac_epa) for short/medium/long
# yac_epa line for yards to first down vs epa (screen passes)
# player chart of avg air_yards vs. avg yards_after_catch
# 3D fg%, yards to goal, 4th down conv %

names(nfl)
summary(nfl$pff_PASSWIDTH)
allp <- nfl %>% filter(pff_DOWN < 4, pff_DISTANCE > 0, pff_DISTANCE < 21)
allp_p <- allp %>% filter(pff_RUNPASS == "P", pff_PASSDEPTH > 2)
allp_p2 <- allp %>% filter(pff_RUNPASS == "P", pff_PASSDEPTH < 3, pff_PASSDEPTH > -5, 
                           pff_PASSWIDTH > 10, pff_PASSWIDTH < 43)
allp_r <- allp %>% filter(pff_RUNPASS == "R")
allp <- rbind(allp_p, allp_p2)
allp <- rbind(allp, allp_r)

screen <- filter(nfl, pff_RUNPASS == "P", pff_PASSDEPTH < 3, pff_PASSDEPTH > -5, 
                 pff_PASSWIDTH < 11 | pff_PASSWIDTH > 42, pff_DISTANCE > 0, pff_DISTANCE < 21, 
                 pff_DOWN < 4)

screen <- screen %>%
  mutate(succ_rate = ifelse(pff_DOWN == 1, ifelse(pff_GAINLOSS >= .4*pff_DISTANCE, 1, 0), 
                            ifelse(pff_DOWN == 2, ifelse(pff_GAINLOSS >= .5*pff_DISTANCE, 1, 0), 
                                   ifelse(pff_GAINLOSS >= pff_DISTANCE, 1, 0))))

pr <- nfl %>% filter(pff_RUNPASS == "P" | pff_RUNPASS == "R")
pr <- pr %>% 
  mutate(succ_rate = ifelse(pff_DOWN == 1, ifelse(pff_GAINLOSS >= .4*pff_DISTANCE, 1, 0), 
                                       ifelse(pff_DOWN == 2, ifelse(pff_GAINLOSS >= .5*pff_DISTANCE, 1, 0), 
                                              ifelse(pff_GAINLOSS >= pff_DISTANCE, 1, 0))))
  
allp <- allp %>%
  mutate(succ_rate = ifelse(pff_DOWN == 1, ifelse(pff_GAINLOSS >= .4*pff_DISTANCE, 1, 0), 
                            ifelse(pff_DOWN == 2, ifelse(pff_GAINLOSS >= .5*pff_DISTANCE, 1, 0), 
                                   ifelse(pff_GAINLOSS >= pff_DISTANCE, 1, 0))))

nrow(screen)
screen <- filter(screen, !is.na(succ_rate))
allp <- filter(allp, !is.na(succ_rate))
pr <- filter(pr, !is.na(succ_rate))
# median(pff_GAINLOSSNET)
summary(pr$succ_rate)
succ <- c()
yards_to_go <- c()
down <- c()
size <- c()
dists <- sort(unique(screen$pff_DISTANCE))
all_n <- 0
all_pos <- 0
all_neg <- 0
for (d in 1:3) {
  for (i in 1:length(dists)) {
    sub <- filter(screen, pff_DISTANCE == i, pff_DOWN == d)
    sub_all <- filter(allp, pff_DISTANCE == i, pff_DOWN == d)
    size_sub <- nrow(sub)
    size <- c(size, size_sub)
    succ_avg <- mean(sub$succ_rate)
    succ_all_avg <- mean(sub_all$succ_rate)
    diff <- succ_avg - succ_all_avg
    if (!is.na(diff)) {
      all_n <- all_n + size_sub
      if (diff > 0) {
        all_pos <- all_pos + size_sub
      }
      else {
        all_neg <- all_neg + size_sub
      }
    }
    succ <- c(succ, diff)
    yards_to_go <- c(yards_to_go, i)
    down <- c(down, toString(d))
  }
}

screen_med <- data_frame(Down = down, ytg = yards_to_go, succ_rate = succ, size = size)
screen_med

screen_med %>%
  ggplot(aes(x = ytg, y = succ_rate, color=Down)) + 
  geom_line(linetype='dashed') + 
  geom_point() +
  labs(x = "Yards to Go", 
       y = "(Screen Success Rate - Success Rate of All Other Plays)", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Effectiveness, 2019 NFL Regular Season", 
       subtitle = "40.3% of all screen passes are expected to be worse than the average play") + 
  scale_x_continuous(breaks = seq(1, 20, by = 1)) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) + 
  theme_calc() + scale_colour_wsj("colors6")





  # theme_wsj() + 
  # scale_colour_wsj("colors6")

all_neg / all_n

ggsave("screen_down.png", dpi=1000)


screen <- screen %>%
  mutate(good_choice = ifelse(
    (pff_DOWN == 1 & pff_DISTANCE < 11) | 
      (pff_DOWN == 2 & pff_DISTANCE < 8) | 
      (pff_DOWN == 3 & pff_DISTANCE < 3), 1, 0
  ))


# analysis by team
all_teams <- (unique(screen$pff_OFFTEAM))
team <- c()
freq <- c()
eff <- c()
good <- c()
for (n in 1:32) {
    t <- all_teams[n]
    print(t)
    sub <- screen %>% filter(pff_OFFTEAM == t)
    freq <- c(freq, nrow(sub))
    team <- c(team, t)
    eff <- c(eff, mean(sub$succ_rate))
    good <- c(good, mean(sub$good_choice))
}

screen_by_team <- data.frame(team, freq, eff, good)
screen_by_team

nfl_colors <- teamcolors %>% 
  filter(league == "nfl") %>% 
  select("team_full" = name, primary)

nfl_logos_df <- read_csv("nfl_teamlogos.csv")

screen_by_team <- screen_by_team %>% 
  left_join(nfl_logos_df, by = c("team" = "team_code")) %>%
  left_join(nfl_colors, by = "team_full") 

freq_avg <- mean(screen_by_team$freq)
freq_avg

screen_by_team %>% 
  ggplot(aes(x = team, y = freq)) + 
  labs(x = "Team", 
       y = "# of Screens Called", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Usage, 2019 NFL Regular Season") + 
  geom_hline(yintercept=freq_avg, linetype='dotted', color='red') +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) + 
  theme_calc() + 
  geom_col(aes(fill = primary, 
               color = primary), alpha = 0.7) +
  geom_image(aes(image = url), size = 0.05) +
  scale_color_identity(aesthetics = c("color", "fill"))

ggsave("screen_team.png", dpi = 1000)

middle_sr <- mean(screen_by_team$eff)
middle_freq <- mean(screen_by_team$freq)

# team efficiency vs. frequency 
screen_by_team %>%
  ggplot(aes(x = freq, y = eff)) + 
  geom_vline(xintercept=middle_freq, linetype='dotted', color='red') + 
  geom_hline(yintercept=middle_sr, linetype='dotted', color='red') + 
  geom_image(aes(image = url), size = 0.07) + 
  labs(x = "# of Screens Called", 
       y = "Success Rate", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Usage and Effectiveness, 2019 NFL Regular Season", 
       subtitle = "Red lines represent averages of axes") +  
  theme_bw() + 
  # theme(panel.border = element_blank(), 
        # axis.line.x = element_line(), 
        # axis.line.y = element_line()) + 
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        plot.title = element_text(size=16), 
        plot.subtitle = element_text(size=14, face = "italic"), 
        plot.caption = element_text(size=12))  


screen_by_team$score = screen_by_team$freq * 0.5 / screen_by_team$eff
screen_by_team[with(screen_by_team, order(-score)), ]

# thickness of bar and size of team logo represent frequency, 
# eff on y axis, just team on x

screen_by_team <- screen_by_team[with(screen_by_team, order(freq)), ]


middle_sr <- mean(screen_by_team$eff)
middle_cor <- mean(screen_by_team$good)

# team efficiency vs. frequency 
screen_by_team %>%
  ggplot(aes(x = good, y = eff)) + 
  geom_image(aes(image = url), size = 0.05) + 
  geom_vline(xintercept=middle_cor, linetype='dotted', color='red') + 
  geom_hline(yintercept=middle_sr, linetype='dotted', color='red') + 
  labs(x = "Proportion of Screens Called in Situations Where Screen Success Rate >= That of All Other Plays", 
       y = "Success Rate of Screens", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Efficiency and Play Calling, 2019 NFL Regular Season") +  
  # subtitle = "Scoring Opportunity: Field Goal Attempted or TD") + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line()) +
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        plot.title = element_text(size=16), 
        plot.subtitle = element_text(size=14), 
        plot.caption = element_text(size=12)) 

ggsave("screen_success_correct.png", dpi = 1000)

screen_by_team

sub_screen <- screen %>% filter(pff_OFFTEAM == 'LA' | 
                                  pff_OFFTEAM == 'DEN' | 
                                  pff_OFFTEAM == 'KC' | 
                                  pff_OFFTEAM == 'MIN') %>% 
  mutate(down = as.factor(pff_DOWN))

# facet by team, density % of screens called by down/distance
la <- screen %>% 
  filter(pff_OFFTEAM == 'LA')
la_m <- round(mean(la$succ_rate)*100, 1)
la_s <- paste(toString(la_m), "% Screen Success Rate Overall", sep="")
screen %>% 
  filter(pff_OFFTEAM == 'LA') %>% 
  mutate(Down = as.factor(pff_DOWN)) %>%
  ggplot(aes(x = pff_DISTANCE, fill = Down)) + 
  geom_density(alpha=0.4) +
  labs(x = "Yards to Go", 
       y = "Density of Screen Calls", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Usage, LA Rams, 2019 NFL Regular Season",
       subtitle = la_s) + 
  scale_x_continuous(breaks = seq(1, 20, by = 1)) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) + 
  theme_calc()
ggsave('rams_screen.png', dpi = 1000)

den <- screen %>% 
  filter(pff_OFFTEAM == 'DEN')
den_m <- round(mean(den$succ_rate)*100, 1)
den_s <- paste(toString(den_m), "% Screen Success Rate Overall", sep="")
screen %>% 
  filter(pff_OFFTEAM == 'DEN') %>% 
  mutate(Down = as.factor(pff_DOWN)) %>%
  ggplot(aes(x = pff_DISTANCE, fill = Down)) + 
  geom_density(alpha=0.4) +
  labs(x = "Yards to Go", 
       y = "Density of Screen Calls", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Usage, Denver Broncos, 2019 NFL Regular Season", 
       subtitle = den_s) + 
  scale_x_continuous(breaks = seq(1, 20, by = 1)) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) + 
  theme_calc()
ggsave('denver_screen.png', dpi=1000)

kc <- screen %>% 
  filter(pff_OFFTEAM == 'KC')
kc_m <- round(mean(kc$succ_rate)*100, 1)
kc_s <- paste(toString(kc_m), "% Screen Success Rate Overall", sep="")
screen %>% 
  filter(pff_OFFTEAM == 'KC') %>% 
  mutate(Down = as.factor(pff_DOWN)) %>%
  ggplot(aes(x = pff_DISTANCE, fill = Down)) + 
  geom_density(alpha=0.4) +
  labs(x = "Yards to Go", 
       y = "Density of Screen Calls", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Usage, KC Chiefs, 2019 NFL Regular Season", 
       subtitle = kc_s) + 
  scale_x_continuous(breaks = seq(1, 20, by = 1)) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) + 
  theme_calc()
ggsave('chiefs_screen.png', dpi=1000)

min <- screen %>% 
  filter(pff_OFFTEAM == 'MIN')
min_m <- round(mean(min$succ_rate)*100, 1)
min_s <- paste(toString(min_m), "% Screen Success Rate Overall", sep="")
screen %>% 
  filter(pff_OFFTEAM == 'MIN') %>% 
  mutate(Down = as.factor(pff_DOWN)) %>%
  ggplot(aes(x = pff_DISTANCE, fill = Down)) + 
  geom_density(alpha=0.4) +
  labs(x = "Yards to Go", 
       y = "Density of Screen Calls", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Usage, Minnesota Vikings, 2019 NFL Regular Season", 
       subtitle = min_s) + 
  scale_x_continuous(breaks = seq(1, 20, by = 1)) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) + 
  theme_calc()
ggsave('vikings_screen.png', dpi=1000)


screen %>% 
  mutate(Down = as.factor(pff_DOWN)) %>%
  ggplot(aes(x = pff_DISTANCE, fill = Down)) + 
  geom_density(alpha=0.4) +
  labs(x = "Yards to Go", 
       y = "Density of Screen Calls", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Screen Usage, All Teams, 2019 NFL Regular Season") + 
  scale_x_continuous(breaks = seq(1, 20, by = 1)) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) + 
  theme_calc()
ggsave('all_screen.png', dpi=1000)

nrow(screen)
nrow(filter(screen, pff_GAINLOSS <= 0))
nrow(filter(screen, pff_GAINLOSS <= 2))
86/1065
291/1065

first <- filter(screen, pff_DOWN == 1, pff_DISTANCE == 10)
nrow(first)
nrow(filter(first, pff_GAINLOSS <= 0))
nrow(filter(first, pff_GAINLOSS <= 2))
30/406
109/406










