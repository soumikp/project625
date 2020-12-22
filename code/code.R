## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
suppressMessages(require(tidyverse))
suppressMessages(require(pdftools))
suppressMessages(require(gtsummary))
suppressMessages(require(here))
suppressMessages(require(janitor))
suppressMessages(require(glue))
suppressMessages(require(EpiEstim))
suppressMessages(require(gghighlight))
suppressMessages(require(jtools))
suppressMessages(require(simpleboot))
suppressMessages(require(ggridges))
suppressMessages(require(gt))
suppressMessages(require(gridExtra))
suppressMessages(require(grid))
suppressMessages(require(lmPerm))
suppressMessages(require(ggrepel))
suppressMessages(require(coin))
suppressMessages(require(latex2exp))
suppressMessages(require(gt))
suppressMessages(require(scales))
suppressMessages(require(httr))
suppressMessages(require(XML))
suppressMessages(require(ggpubr))


## ----data_demo, echo = FALSE------------------------------------------------------------------
### 1: list of governors by political affiliation ####

## 1A: get data from wikipedia ##
library(httr)
library(XML)
url <- "https://en.wikipedia.org/wiki/List_of_current_United_States_governors"
r <- GET(url)
doc <- readHTMLTable(doc=content(r, "text"))

## 1B: create list of states+territories+D.C. stratified by status of governor ##
#50 states
states <- doc[[1]]$V1[-c(1:2)]
party <- ifelse(doc[[1]]$V5[-c(1:2)] == "Republican", "R", "D")
name <- doc[[1]]$V3[-c(1:2)]
#5 territories - rectify U.S. Virgin Islands
states <- c(states,
            doc[[2]]$V1[-c(1,2)])
party <- c(party,
           ifelse(grepl("Rep", doc[[2]]$V5[-c(1:2)]), "R", "D"))
name <- c(name,
          doc[[2]]$V3[-c(1:2)])
states[55] <- "Virgin Islands"
#District of Columbia
states <- c(states,
            doc[[3]]$V1[-c(1,2)])
party <- c(party,
           ifelse(doc[[3]]$V5[-c(1,2)] == "Republican", "R", "D"))
name <- c(name,
          doc[[3]]$V3[-c(1:2)]) ## done!

rep <- states[party == "R"]

r.col <- "#e9141d"
d.col <- "#0015bc"

#### 2: list of state populations and gender of governor ####
url.2 <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
pop <- GET(url.2)
doc.2 <- readHTMLTable(doc=content(pop, "text"))

States2 <- doc.2[[1]]$V3[-c(1:2)]
populous <- doc.2[[1]]$V4[-c(1:2)]
populous[53] <- 168485
populous[54] <- 106235
populous[55] <- 51433
populous[56] <- 49437
populous <- as.numeric(gsub(",","",populous))

female.govs <- c("Rhode Island","Oregon","Alabama","Iowa","Michigan","New Mexico","Maine","South Dakota","Laura Kelly")

#### 3: state demographics info ####
state.info <- as_tibble(cbind(States2,populous)) %>%
  add_column(Rank = 1:length(States2)) %>%
  arrange(States2) %>%
  filter(Rank <= 56) %>%
  mutate(name = States2, 
         pop = populous) %>%
  mutate(gender = (ifelse(name %in% female.govs, "F","M")), 
         party = ifelse(name %in% rep, "R", "D")) %>% 
  select(name, pop, gender, party)


## ----data_covid, echo = FALSE-----------------------------------------------------------------
### 2: covid-counts ####

## pull data (NYT Github) ##
dat <- suppressMessages(read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))

## only keep states which have had some covid cases. ##
keep1.r <- dat %>%
  select(state, date,  cases) %>%
  arrange(state, date) %>%
  dplyr::rename(tot.case = cases) %>%
  group_by(state) %>%
  mutate(new.case = tot.case - dplyr::lag(tot.case), 
         length = seq_along(tot.case)) %>%
  dplyr::summarise(max.tot = max(tot.case, na.rm = TRUE),
                   duration = max(length),
                   .groups = "drop") %>%
  filter(max.tot >= 1000,  #1000 total cases at least
         duration >= 10) %>% #at least 200 new cases in a day once.
  pull(state)

## total cases and new cases defined ##
data <- dat %>%
  dplyr::select(state, date,  cases) %>%
  filter(state %in% keep1.r) %>%
  arrange(state, date) %>%
  dplyr::rename(tot.case = cases) %>%
  group_by(state) %>%
  mutate(new.case = tot.case - dplyr::lag(tot.case)) %>%
  filter(tot.case >= 100) %>%
  ungroup() %>%
  dplyr::rename(total = tot.case,
         cases = new.case)

## 1 or 2 days kicked out b/c of bad reporting (negative case counts) ##
data <- data %>% filter(cases > 0)

#### 3: create R(t) estimates ####

## function to create estimate + CI ##
estR0_out <- function(dat) {

  t_start <- seq(2, nrow(dat) - 4)
  t_end   <- t_start + 4

  # from EpiEstim
  res <- estimate_R(
    incid = dat$cases,
    method = "parametric_si",
    config = make_config(list(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = 4321))
  )

  tibble(
    date_num = res$dates
  ) %>% left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    add_column(date = dat$date) %>%
    dplyr::select(-date_num) %>%
    select(date, everything())
}


## get state-wise estimates of time-varying R(t) ##
options(warn = -1)

r0_est <- data %>%
  filter(date <= "2020-11-09") %>%
  dplyr::select(date, cases, state) %>%
  dplyr::rename(name = state) %>%
  nest(data = c(-name)) %>%
  mutate(
    estR0 = map(data, ~estR0_out(dat = .x))
  ) %>%
  unnest(estR0) %>%
  dplyr::select(-data)

options(warn = 1)

## summarise curves and stratify by political affiliation ##
rep <- states[party == "R"]

r.stat <- r0_est %>%
  drop_na() %>%
  group_by(name) %>%
  dplyr::summarise(mean = mean(r, na.rm = TRUE),
            median = median(r, na.rm = TRUE),
            max = max(r, na.rm = TRUE),
            last = dplyr::last(r),
            sd = sd(r, na.rm = TRUE),
            lcb.q = quantile(r, na.rm = TRUE, probs = 0.025),
            ucb.q = quantile(r, na.rm = TRUE, probs = 0.975),.groups = "drop") %>%
  mutate(party = ifelse(name %in% rep, "R", "D"))

data <- inner_join(state.info %>% mutate(pop = as.numeric(pop)), r.stat, by = "name") %>% 
  dplyr::select(-party.y) %>% 
  dplyr::rename(party = party.x) %>% 
  dplyr::mutate(pop = pop/100000)



## ----party_tab1, echo = FALSE-----------------------------------------------------------------
#### present table with summary statistics of R(t), political affiliation, gender of governor and population density ####

data %>%
  select(name,
         mean, median, max, last,
         party, pop, gender) %>% 
  gt() %>%
  data_color(
    columns = vars(mean),
    colors = scales::col_bin(c("#d8f5d5", "#fae0de"),
                             domain = NULL,
                             bins = c(1.077, 1.164,1.30), pretty = F)
  ) %>%
  data_color(
    columns = vars(median),
    colors = scales::col_bin(c("#d8f5d5", "#fae0de"),
                             domain = NULL,
                             bins = c(0.976, 1.0723,1.30), pretty = F)
  ) %>%
  data_color(
    columns = vars(max),
    colors = scales::col_bin(c("#d8f5d5", "#fae0de"),
                             domain = NULL,
                             bins = c(2.18, 3.706,100), pretty = F)
  ) %>%
  data_color(
    columns = vars(last),
    colors = scales::col_bin(c("#d8f5d5", "#fae0de"),
                             domain = NULL,
                             bins = c(0.8981, 1.3867,3), pretty = F)
  ) %>%
  data_color(
    columns = vars(party),
    colors = scales::col_factor(c(r.col, d.col),
                                domain = NULL)(factor(c("R", "D")))
  ) %>%
  data_color(
    columns = vars(gender),
    colors = scales::col_factor(c("#F4C2C2", "#89CFF0"),
                                domain = NULL)(factor(c("F", "M")))
  ) %>%
  fmt_number(
    columns  = vars(mean, median, max, last, pop),
    decimals = 2
  ) %>%
  tab_header(
    title    = md("**Table 1: State-wise summary statistics for states in our dataset**")
  ) %>%
  cols_align(
    align   = "center",
    columns = vars(mean, median, max, last, party, gender)
  ) %>%
  cols_align(
    align   = "center",
    columns = vars(pop)
  ) %>% 
  cols_label(
    name = html("Name of State"),
    mean = html("Mean<sup>1, 2</sup>"),
    median = html("Median<sup>1, 2</sup>"),
    max = html("Maximum<sup>1, 2</sup>"),
    last = html("Last<sup>1, 2</sup>"),
    party = html("Party<sup>3</sup>"), 
    gender = html("Gender<sup>4</sup>"), 
    pop = html("Population<sup>5</sup>"), 
  ) %>%
  tab_source_note(
    source_note = md("<sup>1</sup>Summary values of time-varying R over duration of study.<br><sup>2</sup>Cells coloured red (green) based on poor (good) performance of region relative to national performance of the USA.<br><sup>3</sup>Political affiliation of governor of respective state.<br><sup>4</sup>Gender of governor of respective state.<br><sup>5</sup>Population (in 100,000) of respective state.")
  ) %>%
  tab_source_note(
    source_note = md("Study duration from time at which each state crossed 1000 cases respectively \n till 11-09-2020, when the first vaccine was announced")
  ) %>%
  tab_source_note(
    source_note = md("Source: NYT Github.<br>Table generated using gt package in R.")
  )


## ----party_boxplot, fig.align="center", fig.width=12, fig.height = 12, echo = FALSE-----------
## create boxplot of time-varying R(t) for each state, colour-coded by blue or red for democrat or republican respectively ##

order <- r.stat %>% select(name, median) %>% arrange(median) %>% pull(name)

state.col <- ifelse(order %in% rep, r.col, d.col)

r0_est$name <- factor(r0_est$name, levels = order)

r0_est <- r0_est %>% filter(r < 3.5)

fig <- suppressWarnings(r0_est %>%
         mutate(party = ifelse(name %in% rep, "R", "D"))  %>%
         drop_na() %>%
         ggplot(aes(x = r, y = name, group = name)) +
         geom_boxplot(aes(fill = party), color = "gray60") +
         scale_fill_manual(values = c("R" = r.col, "D" = d.col),
                           name = TeX("\\textit{Political affiliation of governor}"),
                           labels = c("Democrat", "Republican")) +
         #xlim(c(0, 3.5)) +
         ylab(TeX("\\textbf{Name of state (colour-coded by political affiliation of governor)}")) +
         xlab(TeX("\\textbf{State-wise time-varying effective reproduction number $R_t$}")) +
         theme_bw( ) +
         theme(legend.position = "bottom",
               axis.text.y = element_text(colour = state.col, face = "bold")) +
         labs(title= TeX("\\textbf{(1): Boxplots of time varying $R_t$, stratified by political affiliation of governor}"),
              subtitle = TeX("\\textit{Boxplots truncated above for all values $R_t \\geq 3.5$ }"),
              caption = "Source: NYT Github
                        Duration of study: time at which each state crossed 500 cases respectively till 11-09-2020, when the first vaccine was announced") +
         guides(fill=guide_legend(nrow=2,byrow=TRUE)))
fig


## ----party_density, echo = FALSE, fig.align="center", fig.width=12, fig.height=12-------------

## stratified (by party) density plots of mean time-varying R(t) ##
fig.2a <- r.stat %>%
  ggplot(aes(x = mean)) +
  geom_density_ridges(aes(fill = party, y = party)) +
  scale_fill_manual(values = c("R" = r.col, "D" = d.col),
                    name = TeX("\\textit{Political affiliation of governor}"),
                    labels = c("Democrat", "Republican")) +
  scale_color_manual(values = c("R" = r.col, "D" = d.col)) +
  #coord_flip() +
  xlab(TeX("\\textbf{Mean of time varying effective reproduction number $(R_t)$")) +
  ylab(TeX("\\textbf{Density (stratified by political affiliation of governor)}")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_vline(data = r.stat %>%
               group_by(party) %>%
               dplyr::summarize(r = median(mean, na.rm = TRUE), .groups = "drop"),
             aes(xintercept = r),
             color = "black",
             size = 0.8,
             linetype = "dashed") +
  geom_label_repel(data = r.stat %>%
                     group_by(party) %>%
                     dplyr::summarize(r = median(mean, na.rm = TRUE), .groups = "drop"),
                   aes(x = r,
                       y = c(1.75, 2.75),
                       label = c("Median across all Democrat-led states: 1.16",
                                 "Median across all Republican-led states: 1.16"),
                       fill = NULL,
                       color = party),
                   hjust = c(-0.75, -0.75),
                   show.legend = F,
                   segment.colour = "black") +
  labs(title= "2(A): Density plots of state-specific means of time varying R(t), \nstratified by political affiliation of governor",
       subtitle = "Vertical dotted lines indicate group medians associated with mean time-varying \nR(t) stratified by political affiliation of governor")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

## stratified (by party) density plots of median time-varying R(t) ##
fig.2b <- r.stat  %>%
  ggplot(aes(x = median)) +
  geom_density_ridges(aes(fill = party, y = party)) +
  scale_fill_manual(values = c("R" = r.col, "D" = d.col),
                    name = TeX("\\textit{Political affiliation of governor}"),
                    labels = c("Democrat", "Republican")) +
  scale_color_manual(values = c("R" = r.col, "D" = d.col)) +
  #coord_flip() +
  xlab(TeX("\\textbf{Median of time varying effective reproduction number $(R_t)$")) +
  ylab(TeX("\\textbf{Density (stratified by political affiliation of governor)}")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_vline(data = r.stat %>%
               group_by(party) %>%
               dplyr::summarize(r = median(median, na.rm = TRUE), .groups = "drop"),
             aes(xintercept = r),
             color = "black",
             size = 0.8,
             linetype = "dashed") +
  geom_label_repel(data = r.stat %>%
                     group_by(party) %>%
                     dplyr::summarize(r = median(median, na.rm = TRUE), .groups = "drop"),
                   aes(x = r,
                       y = c(1.75, 2.75),
                       label = c("Median across all Democrat-led states: 1.06",
                                 "Median across all Republican-led states: 1.08"),
                       fill = NULL,
                       color = party),
                   hjust = c(1.75, 1.75),
                   show.legend = F,
                   segment.colour = "black") +
  labs(title= "2(B): Density plots of state-specific medians of time varying R(t), \nstratified by political affiliation of governor",
       subtitle = "Vertical dotted lines indicate group medians associated with median time-varying \n R(t) stratified by political affiliation of governor")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

## stratified (by party) density plots of maximum time-varying R(t) ##
fig.2c <- r.stat  %>%
  ggplot(aes(x = max)) +
  geom_density_ridges(aes(fill = party, y = party)) +
  scale_fill_manual(values = c("R" = r.col, "D" = d.col),
                    name = TeX("\\textit{Political affiliation of governor}"),
                    labels = c("Democrat", "Republican")) +
  scale_color_manual(values = c("R" = r.col, "D" = d.col)) +
  #coord_flip() +
  xlab(TeX("\\textbf{Maximum of time varying effective reproduction number $(R_t)$")) +
  ylab(TeX("\\textbf{Density (stratified by political affiliation of governor)}")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_vline(data = r.stat %>%
               group_by(party) %>%
               dplyr::summarize(r = median(max, na.rm = TRUE), .groups = "drop"),
             aes(xintercept = r),
             color = "black",
             size = 0.8,
             linetype = "dashed") +
  geom_label_repel(data = r.stat %>%
                     group_by(party) %>%
                     dplyr::summarize(r = median(max, na.rm = TRUE), .groups = "drop"),
                   aes(x = r,
                       y = c(1.75, 2.75),
                       label = c("Median across all Democrat-led states: 3.89",
                                 "Median across all Republican-led states: 3.64"),
                       fill = NULL,
                       color = party),
                   hjust = c(-0.75, -0.75),
                   show.legend = F,
                   segment.colour = "black") +
  labs(title= "2(C): Density plots of state-specific maxima of time varying R(t), \nstratified by political affiliation of governor",
       subtitle = "Vertical dotted lines indicate group medians associated with maximum time-varying \n R(t) stratified by political affiliation of governor") + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

## stratified (by party) density plots of most recent time-varying R(t) ##
fig.2d <- r.stat  %>%
  ggplot(aes(x = last)) +
  geom_density_ridges(aes(fill = party, y = party)) +
  scale_fill_manual(values = c("R" = r.col, "D" = d.col),
                    name = TeX("\\textit{Political affiliation of governor}"),
                    labels = c("Democrat", "Republican")) +
  scale_color_manual(values = c("R" = r.col, "D" = d.col)) +
  #coord_flip() +
  xlab(TeX("\\textbf{Most recent (as of 11-09-2020) time varying effective reproduction number $(R_t)$")) +
  ylab(TeX("\\textbf{Density (stratified by political affiliation of governor)}")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_vline(data = r.stat %>%
               group_by(party) %>%
               dplyr::summarize(r = median(last, na.rm = TRUE), .groups = "drop"),
             aes(xintercept = r),
             color = "black",
             size = 0.8,
             linetype = "dashed") +
  geom_label_repel(data = r.stat %>%
                     group_by(party) %>%
                     dplyr::summarize(r = median(last, na.rm = TRUE), .groups = "drop"),
                   aes(x = r,
                       y = c(1.75, 2.75),
                       label = c("Median across all Democrat-led states: 1.39",
                                 "Median across all Republican-led states: 1.40"),
                       fill = NULL,
                       color = party),
                   hjust = c(-0.25, -0.25),
                   show.legend = F,
                   segment.colour = "black") +
  labs(title= "2(D): Density plots of state-specific most recent values of time \nvarying R(t), stratified by political affiliation of governor",
       subtitle = "Vertical dotted lines indicate group medians associated with most recent \ntime-varying R(t) stratified by political affiliation of governor") + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE))



annotate_figure(ggarrange(fig.2a, fig.2b, fig.2c, fig.2d, 
          ncol = 2, nrow = 2, 
          common.legend = TRUE, 
          legend = "bottom"), 
          bottom = text_grob("Study duration from time at which each state crossed 1000 cases respectively till 11-09-2020, when the first vaccine was announced. \n(Source: NYT Github) [Figure generated using ggplot and ggpubr packages in R]", 
                             hjust =1, x = 1, size =10))
          


## ----party_boot, echo = FALSE-----------------------------------------------------------------
stat <- NULL
lcb <- NULL
ubc <- NULL

## bootstrapping difference of medians of mean of time varying R(t) ##
bs.sample <- simpleboot::two.boot(r.stat %>% filter(party == "D") %>% pull(mean),
                                  r.stat %>% filter(party == "R") %>% pull(mean),
                                  median,
                                  1000)
stat <- c(stat, bs.sample$t0)
lcb <- c(lcb, quantile(bs.sample$t, 0.025))
ubc <- c(ubc, quantile(bs.sample$t, 0.975))

## bootstrapping difference of medians of median of time varying R(t) ##
bs.sample <- simpleboot::two.boot(r.stat %>% filter(party == "D") %>% pull(median),
                                  r.stat %>% filter(party == "R") %>% pull(median),
                                  median,
                                  1000)
stat <- c(stat, bs.sample$t0)
lcb <- c(lcb, quantile(bs.sample$t, 0.025))
ubc <- c(ubc, quantile(bs.sample$t, 0.975))

## bootstrapping difference of medians of max of time varying R(t) ##
bs.sample <- simpleboot::two.boot(r.stat %>% filter(party == "D") %>% pull(max),
                                  r.stat %>% filter(party == "R") %>% pull(max),
                                  median,
                                  1000)
stat <- c(stat, bs.sample$t0)
lcb <- c(lcb, quantile(bs.sample$t, 0.025))
ubc <- c(ubc, quantile(bs.sample$t, 0.975))

## bootstrapping difference of medians of most recent of time varying R(t) ##
bs.sample <- simpleboot::two.boot(r.stat %>% filter(party == "D") %>% pull(last),
                                  r.stat %>% filter(party == "R") %>% pull(last),
                                  median,
                                  1000)
stat <- c(stat, bs.sample$t0)
lcb <- c(lcb, quantile(bs.sample$t, 0.025))
ubc <- c(ubc, quantile(bs.sample$t, 0.975))

op <- as_tibble(cbind(stat, lcb, ubc)) %>%
  add_column(type = c("Difference of means",
                      "Difference of medians",
                      "Difference of maxima",
                      "Difference of last")) %>%
  dplyr::select(type, stat, lcb, ubc) %>%
  dplyr::rename("Test statistic" = type,
         "Sample value" = stat,
         "Lower bootstrap CI" = lcb,
         "Upper bootstrap CI" = ubc)


op <- suppressWarnings(op %>%
  dplyr::mutate('Test statistic' = c("Mean", "Median", "Maxima", "Last")))

## table presents true statistic, and 95% bootstrap CI of difference of median of statistics ##
op %>%
  gt() %>%
  fmt_number(
    columns  = vars("Sample value", "Lower bootstrap CI", "Upper bootstrap CI"),
    decimals = 3
  ) %>%
  tab_header(
    title    = md("**Table 2: Investigating time-varying reproduction number R for USA**"),
    subtitle = md("Difference of statistics computed by resampling methods<sup>1</sup>")
  ) %>%
  cols_align(
    align   = "center",
    columns = vars("Sample value", "Lower bootstrap CI", "Upper bootstrap CI")
  ) %>%
  cols_label(
    "Test statistic" = html("Statistic of interest"),
    "Sample value" = html("Sample value<sup>2</sup>"),
    "Lower bootstrap CI" = html("Lower 95% bootstrap CI<sup>3</sup>"),
    "Upper bootstrap CI" = html("Upper 95% bootstrap CI<sup>3</sup>")
  ) %>%
  tab_source_note(
    source_note = md("<sup>1</sup>Between-group difference computed by stratifying according to political affiliation of governor of state.<br><sup>2</sup>True value computed based on data from source (see below).<br><sup>3</sup>Quantile-based onfidence intervals obtained by computing between-group differences for resampled data.")
  ) %>%
  tab_source_note(
    source_note = md("Study duration from time at which each state crossed 1000 cases respectively \n till 11-09-2020, when the first vaccine was announced.")
  ) %>%
  tab_source_note(
    source_note = md("Source: NYT Github.<br>Table generated using gt package in R.")
  )



## ----party_pca, echo = FALSE------------------------------------------------------------------
## extract PH indices from PDF from website ###

states <- c(state.abb, "DC")

state.scores <- NULL

for(i in states){
  
  url <- ifelse(i!="NC", 
                paste0("https://nhspi.org/wp-content/uploads/2018/02/StateProfiles2020_", 
                       i, 
                       "-1.pdf"), 
                "https://nhspi.org/wp-content/uploads/2014/12/StateProfiles2020_NC-1.pdf")
  
  
  pdf <- pdf_text(url) %>% readr::read_lines()
  
  score.pos <- c(grep("DOMAIN 1", pdf), 
                 grep("DOMAIN 2", pdf), 
                 grep("DOMAIN 3", pdf), 
                 grep("DOMAIN 4", pdf), 
                 grep("DOMAIN 5", pdf), 
                 grep("DOMAIN 6", pdf))
  
  scores <- NULL
  
  for(j in score.pos){
    text <- suppressWarnings(as.numeric(unlist(strsplit(pdf[j], split = " "))))
    scores <- c(scores, text[(!is.na(text))][1])
  }
  
  state.scores <- rbind(state.scores, 
                        c(i, scores))
  #print(paste0(i, " done!"))
}

### create PHI index matrix ###
ph.stat <- as_tibble(state.scores, .name_repair =~ c("name", "HSS", "CPEP", "IIM", "HD", "CM", "EOH")) %>% 
  add_column(name2 = c(state.name, "District of Columbia")) %>% 
  arrange(name2) %>% 
  dplyr::select(-name) %>% 
  dplyr::rename(name = name2)

ph.stat$HSS <- as.numeric(ph.stat$HSS)
ph.stat$CPEP <- as.numeric(ph.stat$CPEP)
ph.stat$IIM <- as.numeric(ph.stat$IIM)
ph.stat$HD <- as.numeric(ph.stat$HD)
ph.stat$CM <- as.numeric(ph.stat$CM)
ph.stat$EOH <- as.numeric(ph.stat$EOH)


### create PCs of PHI scores ###
ph.stat.pca <- prcomp((ph.stat[,-7]),center = T, scale. = T)

ph.pca <- as_tibble(ph.stat.pca$x[,c(1:3)]) %>% 
  add_column(name = ph.stat %>% pull(name))

kableExtra::kable(round(summary(ph.stat.pca)$importance, 2), 
                  caption = "**Table 3: Summary of principal component analysis**") %>%
  kableExtra::kable_classic() 

inv.norm <- function(x){
  qnorm((rank(x,na.last="keep")-0.5)/sum(!is.na(x)))
}

## create data for linear model by inverse normalising mean R(t), creating covariate matrix ###
lm.data <- inner_join(data, ph.pca, by = "name") %>% 
  dplyr::select(pop, gender, party, mean, PC1, PC2, PC3) %>% 
  dplyr::mutate(mean = inv.norm(mean), 
                pop = pop/100) %>% 
  dplyr::rename(Population = pop, 
                Gender = gender, 
                Party = party)




## ----party_lm, echo = FALSE-------------------------------------------------------------------
## present summary of linear model ##
tbl_regression(lm(mean ~., data = lm.data)) %>% 
  as_gt() %>% 
  gt::tab_header(title = md("**Table 4: Summary of Linear Model**"))

