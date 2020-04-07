library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(purrr)

# Added for reproducibility when sharing with others
my_name <- "name"
her_name <- "name"

# Read in chat file
raw <- readLines("_chat.txt")

# Fix random issues
for (i in seq_along(raw[-1])){
  if(!str_detect(raw[i+1], pattern = "^\\[")){
    raw[i] <- paste0(raw[i], " ", raw[i+1])
    raw[i+1] <- NA
  }
}
raw_fix <- raw %>%
  str_split(pattern = "[[:print:]](?=\\[[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}, [[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}\\])") %>%
  unlist()

# Convert to dataframe
transformed <- raw_fix[!is.na(raw_fix)] %>%
  as.data.frame(stringsAsFactors = F) %>%
  setNames("text") %>%
  mutate(text = str_replace(text, pattern = "^NA ", replacement = ""),
         datetime = str_extract(text, pattern = "^\\[[[:print:]]+?\\]"),
         date = str_extract(datetime, pattern = "[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}"),
         date = dmy(date),
         time = str_extract(datetime, pattern = "[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}"),
         text = str_replace(text, pattern = "^\\[[[:print:]]+?\\]", replacement = ""),
         sender = trimws(str_extract(text, pattern = "^[[:print:]]+?(?=:)")),
         text = str_replace(text, pattern = "^[[:print:]]+?:", replacement = ""),
         text = trimws(tolower(str_replace_all(text, pattern = "[[:punct:]]", replacement = ""))),
         text = if_else(str_detect(text, pattern = "^[[:digit]]+ [[:alpha:]]+ [[:alpha:]]+ image omitted"),
                        "z_IMAGE_SENT",
                        text)) %>%
  filter(!is.na(date)) %>%
  select(date, time, sender, text) %>%
  slice(-1)

# Counting messages
# Over time
ts <- data.frame(date = seq.Date(from = min(transformed$date),
                                 to = max(transformed$date),
                                 by = 1),
                 time = NA,
                 sender = NA,
                 text = NA,
                 stringsAsFactors = FALSE) %>%
  filter(!date %in% transformed$date)
dates <- transformed %>%
  bind_rows(ts) %>%
  group_by(date) %>%
  mutate(daycount = n()) %>%
  ungroup() %>%
  filter(!is.na(sender))

m_o_t <- ggplot(dates) +
  geom_line(aes(x = date, y = daycount), col = "#89f285") +
  geom_smooth(aes(x = date, y = daycount, col = "Average"), col = "blue", size = 0.5) + 
  labs(x = "Date",
       y = "No. of messages") +
  lims(y = c(0, 500)) +
  scale_x_date(breaks = ymd(c("2016-11-01", "2017-01-01", "2017-07-01", "2018-01-01", "2018-07-01", "2019-01-01", "2019-07-01", "2020-01-01", "2020-02-10")),
               labels = c("2016", "", "2017", "", "2018", "", "2019", "", "2020")) +
  ggtitle("Message volume between October 2016 and February 2020") +
  theme_hc() +
  theme(axis.ticks.x = element_line(size = c(rep(c(0,1), 4), 0)),
        axis.text.x = element_text(vjust = 4))
ggsave(m_o_t, filename = "messages_over_time_2.png",  bg = "transparent", width = 10, height = 4)

# Day of the week
wd <- transformed %>%
  mutate(day = weekdays(date, abbreviate = FALSE)) %>%
  group_by(day) %>%
  summarise(n = n(),
            avg = n()/173.1429) # the dataset covers 173 weeks
wd$day <- ordered(wd$day, levels = c("Sunday","Saturday","Friday","Thursday","Wednesday","Tuesday","Monday"))
weekdays <- ggplot(wd, aes(x = day, y = avg)) + 
  geom_bar(aes(fill = day), show.legend = FALSE, stat="identity") +
  geom_text(aes(x = day, y = (avg+8), label = round(avg, 1))) +
  labs(x = "Day of the week",
       y = "") +
  ggtitle("Average no. of messages per day of the week") +
  theme_hc() +
  theme(line = element_line(colour = "black"),
        panel.grid.minor.y = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0)) +
  coord_flip() + 
  scale_y_continuous(position = "right")
ggsave(weekdays, filename = "days_of_week_2.png", bg = "transparent", width = 7, height = 4)

# Time of day
tod <- transformed %>%
  mutate(hour = as.numeric(substr(time,1,2)),
         min = as.numeric(substr(time,4,5)),
         sec = as.numeric(substr(time,7,8)),
         mins = (hour*60)+min) %>%
  count(mins)
time_of_day <- ggplot(tod, aes(x=mins, y = n/1212)) + 
  geom_line(color = "#89f285") + 
  geom_smooth(se = FALSE, color = "blue") +
  labs(x = "Time of Day",
       y = "Average messages (per minute)") +
  scale_x_continuous(breaks = seq(from = 0, to = 1380, by = 60),
                     labels = c("Midnight", paste0(seq(from = 1, to = 11, by = 1), "am"), "Midday",
                                paste0(seq(from = 1, to = 11, by = 1), "pm"))) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1)) + 
  ggtitle("Volume of messages over the course of the day") +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45))
ggsave(time_of_day, filename = "time_of_day_2.png",  bg = "transparent", width = 10, height = 4)

# Most common words
library(tidytext)
library(formattable)
stopwords <- c(stop_words$word, "im", "ive", "dont", "didnt", "youre", "ill", "okk", "dunno", "probs")
words <- transformed %>%
  filter(!str_detect(text, pattern = "omitted")) %>%
  unnest_tokens(input = text, output = "word", token = "words") %>%
  filter(word!= "",
         !word %in% stopwords) %>%
  count(sender, word)
me <- words %>%
  filter(sender == my_name) %>%
  arrange(desc(n)) %>%
  filter(!str_detect(word, pattern = "haha"),
         word != "ahh") %>%
  top_n(10, n)
her <- words %>%
  filter(sender == her_name) %>%
  arrange(desc(n)) %>%
  filter(!str_detect(word, pattern = "haha"),
         word!="yehh",
         word!="ahh") %>%
  top_n(10,n)
her$word <- ordered(her$word, levels = her$word[order(her$n)])
me$word <- ordered(me$word, levels = me$word[order(me$n)])
hwords <- ggplot(her, aes(x = word, y = n)) +
  geom_bar(fill = "pink", show.legend = FALSE, stat = "identity") +
  geom_text(aes(label = n, y = (n+100))) +
  coord_flip() +
  theme_hc() +
  scale_fill_brewer(type = "qual") +
  labs(x = "", y = "") + 
  theme(rect = element_rect(fill = "transparent"),
        line = element_line(colour = "black"),
        axis.text = element_text(color = "black", size = 18),
        axis.text.x = element_text(size = 0),
        axis.ticks = element_line(size = 0),
        panel.grid.minor.y = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Her")
ggsave(lwords, filename = "topwords_her.png", bg = "transparent", width = 4.5, height = 6)

mwords <- ggplot(me, aes(x = word, y = n)) +
  geom_bar(fill = "lightblue", show.legend = FALSE, stat = "identity") +
  geom_text(aes(label = n, y = (n+100))) +
  coord_flip() +
  theme_hc() +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  scale_fill_brewer(type = "qual") +
  labs(x = "", y = "") + 
  theme(line = element_line(colour = "black"),
        axis.text = element_text(color = "black", size = 18),
        axis.text.x = element_text(size = 0),
        axis.ticks = element_line(size = 0),
        panel.grid.minor.y = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0),
        plot.title = element_text(hjust = 0.55)) +
  ggtitle("Me")
ggsave(cwords, filename = "topwords_me.png", bg = "transparent", width = 4.5, height = 6)

# Combine the two for blog post
library(grid)
library(gridExtra)
words_plots <- grid.arrange(mwords, hwords, ncol = 2)
ggsave(words_plots, filename = "wordsplot_blogpost.png", width = 10, height = 6)

loveyou <- transformed %>%
  filter(!str_detect(text, pattern = "omitted")) %>%
  unnest_tokens(input = text, output = "word", token = "ngrams", n = 2) %>%
  filter(word!= "") %>%
  count(sender, word) %>%
  filter(word == "love you")
loveyou3 <- transformed %>%
  filter(!str_detect(text, pattern = "omitted")) %>%
  unnest_tokens(input = text, output = "word", token = "ngrams", n = 3) %>%
  filter(word!= "") %>%
  count(sender, word) %>%
  filter(word == "i love you")

# Emojis
library(emo)
library(ggimage)
library(ggtext)

emoji_messages <- transformed %>%
  mutate(emojis = emo::ji_extract_all(text)) %>%
  select(sender, emojis) %>%
  unnest(cols = c(emojis)) %>%
  count(sender, emojis)

memojis <- emoji_messages %>%
  filter(sender == my_name) %>%
  top_n(10, wt = n) %>%
  arrange(desc(n)) %>%
  mutate(img_name = c("eyeroll",
                   "beaming",
                   "grimace",
                   "crylaugh",
                   "redheart",
                   "unamused",
                   "eyes",
                   "disappointed",
                   "smirking",
                   "sadface"))

hemojis <- emoji_messages %>%
  filter(sender == her_name) %>%
  top_n(10, wt = n) %>%
  arrange(desc(n)) %>%
  mutate(img_name = c("seenoevil",
                   "crylaugh",
                   "pensive",
                   "beaming",
                   "grimace",
                   "weary",
                   "eyeroll",
                   "speaknoevil",
                   "blowkiss",
                   "disappointed"))
memojis$emojis <- factor(memojis$emojis, levels = memojis$emojis[order(memojis$n)])
hemojis$emojis <- factor(hemojis$emojis, levels = hemojis$emojis[order(hemojis$n)])

my_emojiplot <- memojis %>%
  ggplot(aes(x = emojis, y = n)) +
  geom_col(fill = "lightblue") +
  geom_image(aes(y = -60, image = paste0("img/", img_name, ".png")), size = 0.08) +
  geom_text(aes(label = n, y = n+70), size = 4) +
  coord_flip() +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  scale_fill_brewer(type = "qual") +
  labs(x = "", y = "") +
  theme_hc() +
  theme(line = element_line(colour = "black"),
        axis.text = element_text(size = 0),
        axis.ticks = element_line(size = 0),
        panel.grid.minor.y = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0),
        plot.title = element_text(hjust = 0.55)) +
  ggtitle("Me")

her_emojiplot <- hemojis %>%
  ggplot(aes(x = emojis, y = n)) +
  geom_col(fill = "pink") +
  geom_image(aes(y = -60, image = paste0("img/", img_name, ".png")), size = 0.08) +
  geom_text(aes(label = n, y = n+70), size = 4) +
  coord_flip() +
  scale_x_discrete(position = "top") +
  scale_fill_brewer(type = "qual") +
  labs(x = "", y = "") +
  theme_hc() +
  theme(line = element_line(colour = "black"),
        axis.text = element_text(size = 0),
        axis.ticks = element_line(size = 0),
        panel.grid.minor.y = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0),
        plot.title = element_text(hjust = 0.55)) +
  ggtitle("Her")

emoji_plots <- grid.arrange(my_emojiplot, her_emojiplot, ncol = 2)
ggsave(emoji_plots, filename = "emojiplot_blogpost.png", width = 10, height = 6)
