################################################################################
#Tidy Tuesday: 2024, Week 34
#English Monarchs and Marriages
library(dplyr)
library(gsubfn)
library(ggplot2)
library(geomtextpath)
library(ggrepel)
library(gganimate)
library(transformr)
library(gifski)
library(png)
library(plotly)
library(htmlwidgets)

#get data, read directly from github
english_monarchs_marriages_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')

#tidy the data
#only keep rows with information in each col
df_no_missing <- english_monarchs_marriages_df[which(english_monarchs_marriages_df$king_age!='–' & english_monarchs_marriages_df$consort_name!='–'&
                                                       english_monarchs_marriages_df$consort_age!='–'&english_monarchs_marriages_df$year_of_marriage!='–'),]
#remove monarchs where there is a ? instead of data in any value
#should lose roughly 10 obs, started at 68, ended at 58 yay!
df_no_unknown <- df_no_missing %>%  subset(king_age!='?' & consort_name!='?' & consort_age!='?' & year_of_marriage!='?')

#remove (?) from cells aka king_age 50(?) I want to be 50
df_no_questions <- data.frame(lapply(df_no_unknown, function(x) {
  gsub("\\(\\?\\)", "", x) #this is the regex pattern for (?)
}))

#remove special characters aka where the name has Æ I am not sure how well this would plot
df_cleaned <- data.frame(lapply(df_no_questions, function(y) {
  gsub("Æ", "Ae", y) 
}))

#create a new variable which is age difference between king and consort
#make age variables numeric
df<- df_cleaned %>%
  mutate(
    king_age = as.numeric(king_age),
    consort_age = as.numeric(consort_age),
    age_difference = king_age - consort_age
  )

#change variable name from king name to monarch name because some of them are queens
names(df)[names(df) == "king_age"] <- "monarch_age"
names(df)[names(df) == "king_name"] <- "monarch_name"

#count how many with a negative age difference aka consort is older than monarch
nrow(df[df$age_difference <0, ])

#are there any with no age difference
nrow(df[df$age_difference ==0, ])

#make these variables numeric for plotting
df<- df %>%
  mutate(
    year_of_marriage = as.numeric(year_of_marriage),
    age_difference = as.numeric(age_difference)
  )

#allow max overlaps for ggrepel
options(ggrepel.max.overlaps = Inf)

#plot the scatter/line graph situation
p <- ggplot(df, aes(x = year_of_marriage)) +
  geom_ribbon(aes(ymin = consort_age, ymax = monarch_age), fill = "lightblue", alpha = 0.3) + #fills the area between the two lines of data
  geom_line(aes(y = monarch_age, color = "Monarch Age"), size = 1) + #sets one line to show monarch age
  geom_line(aes(y = consort_age, color = "Consort Age"), size = 1) + #sets one line to show consort age
  geom_point(aes(y = monarch_age, color = "Monarch Age")) + #plots points of monarch ages along the above lines
  geom_point(aes(y = consort_age, color = "Consort Age")) + #plots points of consort ages along the above lines
  geom_label_repel(aes(y = monarch_age, label = monarch_name),size = 3, label.size=0.01, vjust = 0.5, hjust = 0.5, color = "aquamarine4", position="dodge") +  #this creates the name labels for the animated graph (have not figured out how to show on interactive plot)
  geom_label_repel(aes(y = consort_age, label = consort_name), size = 3, label.size=0.01, vjust = 0.5, hjust = 0.5, color = "palegreen2", position="dodge") + #this creates the name labels for the animated graph (have not figured out how to show on interactive plot)
  xlab('Year of Marriage') + #label the x axis
  ylab('Age') + #label the y axis
  scale_color_manual(values = c("Monarch Age" = "aquamarine4", "Consort Age" = "palegreen2"))+ #creates the legend showing which line colour is associated with either monarch age or consort age
  labs(color = "Age Type") + #label legend
  theme_minimal() + #clean theme with white background to plot
  theme( #general formatting stuff, text size
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) + transition_reveal(year_of_marriage) # this line is important for producing an animated plot in the next bit of code

#Animated plot
animate(p, duration = 50, height = 800, width =800) #produce a moving plot
anim_save("monarchs.gif") #save as a gif

#Interactive plot
ggplotly(p) #produce an interactive plot using plotly package, need to twiddle with this because I can't figure out how to show names on this
#saveWidget(p, "monarchs.html") #save as html so can be shared, this didn't work so I did it manually using the export function in the viewer panel.

