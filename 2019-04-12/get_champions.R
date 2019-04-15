library(ggpubr)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(tidyverse)

args = commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
    stop("Please (only) specify an input (file of NCAA results)", call.=FALSE)
} 

results = read_csv(args[1], 
                   col_types = cols(), col_names = FALSE, guess_max = 10000) %>%
          data.frame(lapply(., function (x) { str_sub(gsub('@', ' ', x), 1, 68) })) 

results = lapply(results[ , 2], str_split, '\\s{2,}') %>% 
          unlist(.$X1.1) %>%
          matrix(ncol  = 5, byrow = TRUE) %>% data.frame(.) %>%
          set_colnames(c('Date', 'Winner', 'Win.Score', 'Loser', 'Lose.Score')) %>%
          mutate(Win.Score  = as.numeric(Win.Score)) %>%
          mutate(Lose.Score = as.numeric(Lose.Score)) %>%
          mutate(Winner = as.character(Winner)) %>%
          mutate(Loser = as.character(Loser))

champ_list = c(tail(results, 1)$Winner)
print(champ_list)
old_length = 1
new_length = 100
iteration  = 1
champ_df   = data.frame('Champions' = champ_list[[1]], 'Degree' = 0)

while (old_length != new_length) {
    old_length  = length(champ_list)
    winner_list = results %>% filter(Loser %in% champ_list) %>% .$Winner %>% unique(.)
    nth_degree  = setdiff(winner_list, champ_list)
    
    champ_df    = rbind(champ_df, data.frame('Champions' = nth_degree, 'Degree' = rep(iteration, length(nth_degree))))
    
    champ_list  = unique(c(champ_list, winner_list))
    new_length  = length(champ_list)
    iteration   = iteration + 1
}

tot_teams = length(unique(c(results$Winner, results$Loser)))
plot_df   = champ_df %>% group_by(Degree) %>% summarise(n = n()) %>% 
            mutate(n = cumsum(n)) %>% mutate(Fraction = n/tot_teams)
win_df    = left_join(champ_df, 
                      full_join(results %>% group_by(Winner) %>% 
                                  summarise(Num.Wins = n()), 
                                results %>% group_by(Loser) %>% 
                                  summarise(Num.Losses = n()), 
                                by = c('Winner' = 'Loser')) %>% 
                        mutate(Win.Pct = round(Num.Wins/(Num.Wins + Num.Losses), 3)), 
                      by = c('Champions' = 'Winner'))
num_plot  = ggplot(plot_df, aes(x = Degree, y = n)) + geom_point(color = "#FF6666") + geom_line(color = "#FF6666") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
            geom_text(aes(label = n, y = n + max(n)/12, x = Degree - max(Degree)/32), cex = 3, angle = -45) + 
            ylab('Number of Transitive Champions') + xlab('Degree of Champion') + ggtitle(paste0('Total Teams = ', tot_teams))
frac_plot = ggplot(plot_df, aes(x = Degree, y = Fraction*100)) + geom_point(color = "#FF6666") + geom_line(color = "#FF6666") + theme_fivethirtyeight() + theme(axis.title = element_text()) + 
            geom_text(aes(label = round(Fraction*100, 1), y = Fraction*100 + max(Fraction*100)/12, x = Degree - max(Degree)/32), cex = 3, angle = -45) +
            ylab('Percentage of Champion Teams') + xlab('Degree of Champion')
win_plot  = ggplot(win_df, aes(x = Degree, y = Win.Pct)) + geom_point(color = "#FF6666") + theme_fivethirtyeight() + theme(axis.title = element_text()) +
            ylab('Win Percentage') + xlab('Degree of Champion') + 
            ggtitle(paste0('Pearson Rho = ', round(cor.test(win_df$Degree, win_df$Win.Pct)$estimate, 3)))

ggsave(plot = ggarrange(ggarrange(num_plot, frac_plot, nrow = 2), win_plot, ncol = 2),
       filename = gsub('.tsv', '.png', args[1]), device = 'png', width = 250, height = 150, units = 'mm', dpi = 150)

write_tsv(x = win_df, path = gsub('.tsv', '_degree_pct.tsv', args[1]))