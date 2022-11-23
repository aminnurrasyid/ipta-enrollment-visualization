
# Dataset
my_data <- read.csv('students_enrolment.csv')
   

# Data Pre-processing

colnames(my_data) <- c("YEAR", "COURSE", "MAN", "WOMAN", "TOTAL", "PERCENTAGE_OF_MAN", "PERCENTAGE_OF_WOMAN")
my_data$COURSE[my_data$COURSE=="Sains Sosial, Perniagaan dan Perundangan"]<-"Social Science, Business, and Law"
my_data$COURSE[my_data$COURSE=="Program Asas/ Basic Programmes"]<-"Basic Programmes"
my_data$COURSE[my_data$COURSE=="Pendidikan"]<-"Education"
my_data$COURSE[my_data$COURSE=="Perkhidmatan"]<-"Service"
my_data$COURSE[my_data$COURSE=="Kesihatan dan Kebajikan"]<-"Health and Welfare"
my_data$COURSE[my_data$COURSE=="Pertanian dan Veterinar"]<-"Agriculture and Veterinary"
my_data$COURSE[my_data$COURSE=="Kejuruteraan, Pembuatan dan Pembinaan"]<-"Engineering, Manufacture and Construction"
my_data$COURSE[my_data$COURSE=="Sastera dan Kemanusiaan"]<-"Literature and Humanities"
my_data$COURSE[my_data$COURSE=="Sains, Matematik dan Komputer"]<-"Science, Mathematics and Computer"


# Data Wrangling

# To identify the course with the highest enrollment between the years 2017 until 2021. 


library(tidyverse)


highest_enrolment <- my_data %>% group_by(COURSE) %>% summarise(Frequency = sum(TOTAL))

highest_enrolment <- highest_enrolment[order(-highest_enrolment$Frequency),]

highest_enrolment$COURSE <- factor(highest_enrolment$COURSE, levels = highest_enrolment$COURSE[order(highest_enrolment$Frequency)])

highest_enrolment


# -- visualization
colseq_color <- c("#5b9bd5","#5b9bd5","#5b9bd5","#5b9bd5",
                  "#5b9bd5","#5b9bd5","#266196","#266196","#266196")

ggplot(highest_enrolment,aes(x=Frequency, y=COURSE, fill=COURSE)) +
  geom_col(width=0.7) +
  scale_fill_manual(values=colseq_color) +
  labs(title = "Enrollments of IPTA by Courses from 2017 until 2021") +
  theme( 
    plot.title = element_text(color="gray50", size=15, face="bold"),
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15), color="gray60"),
    axis.text.x = element_text(color="gray60"),
    axis.text.y = element_text(color="gray60"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color="gray"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour="gray"),
    legend.position = "none",
    panel.grid.major.x = element_line(color="gray95"), 
    panel.grid.minor = element_blank() ) +
  xlab("Number of Enrollments") +
  scale_x_continuous(labels=scales::comma,expand=expansion(mult = c(0, 0.05)))

# - to save higher quality plot
#ggsave(file="colplot.png", dpi=500)



# To research which gender dominates each STEM-related courses between the year 2017 to 2021.


gender_stem_per_course <- my_data %>% select (YEAR, COURSE, MAN, WOMAN, TOTAL)

gender_stem_per_course <- filter(gender_stem_per_course, COURSE == "Engineering, Manufacture and Construction" | COURSE == "Science, Mathematics and Computer" | COURSE == "Health and Welfare")

gender_stem_per_course <- gender_stem_per_course %>% group_by(COURSE) %>% summarise(across(c(MAN, WOMAN,TOTAL), sum))

# - transforming dataframe for visualization
alternate = gender_stem_per_course %>% select(-COURSE,-TOTAL) %>% pivot_longer(everything())
years = rbind(gender_stem_per_course, gender_stem_per_course) %>% arrange(COURSE) %>% select(COURSE,TOTAL)
gender_filled = merge(years, alternate, by = 0, sort = FALSE)
gender_filled <- gender_filled[order(-gender_filled$TOTAL, gender_filled$name),]
gender_filled$COURSE <- factor(gender_filled$COURSE, levels = rev(unique(gender_filled$COURSE)), ordered=TRUE)

# -- visualization
ggplot(gender_filled, aes(fill=name, y=COURSE, x=value)) + 
  geom_col(position = "dodge", width=0.75) +
  labs(title = "STEM Courses Enrollments of IPTA from 2017 until 2021") +
  annotate("text", x = 310000, y = 3.2, label = "Female", color="#ed7c31",
           fontface=2, size=4.6) + 
  annotate("text", x = 372500, y = 2.825, label = "Male", color="#5b9bd5",
           fontface=2, size=4.6) +
  scale_fill_manual(values = c("#5b9bd5", "#ed7c31")) +
  theme( 
    plot.title = element_text(color="gray50", size=16, vjust = 2, 
      face = 'bold'),
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15), color="gray60",
      size=11),
    axis.text.x = element_text(color="gray60", size=10),
    axis.text.y = element_text(color="gray60", size=10),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color="gray"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour = "gray"),
    legend.position = "none",
    panel.grid.major.x = element_line(colour = "gray95"), 
    panel.grid.minor = element_blank()) +
  xlab("Number of Enrollments") +
  scale_x_continuous(labels=scales::comma,expand=expansion(mult = c(0, 0.05)))
  
# - to save higher quality plot
#ggsave(file="dodgebarplot.png", dpi=500)



# To investigate the trend among the number of applicants, by gender, between 2017 - 2021.


# Trend among men applicants 

trend_admission_man <- my_data %>% select (YEAR, COURSE, MAN)

trend_admission_man <- trend_admission_man %>% group_by(YEAR) %>% summarise(Frequency = sum(MAN))

colnames(trend_admission_man) <- c("Year", "Male")

trend_admission_man


# Trend among women applicants

trend_admission_woman <- my_data %>% select (YEAR, COURSE, WOMAN)

trend_admission_woman <- trend_admission_woman %>% group_by(YEAR) %>% summarise(Frequency = sum(WOMAN))

colnames(trend_admission_woman) <- c("Year", "Female")


# Both trend

trend_admission_woman

trend = merge(trend_admission_man, trend_admission_woman, on = 'Year', sort = FALSE)

trend

# -- visualization
install.packages("ggrepel")
library(ggrepel)

ggplot(trend, aes(x=Year)) +
  geom_rect(aes(xmin=2020, xmax=2021, ymin=-Inf, ymax=Inf), 
    fill='gray85') +
  geom_line(aes(y=Male), color="#5b9bd5", size=2) + 
  geom_point(aes(y=Male), color = "#5b9bd5", size = 4) +
  geom_text_repel(aes(label = Male, y=Male), size=3.8,
            color="gray40", nudge_y = 6000) +
  geom_line(aes(y=Female), color="#ed7c31", size=2) +
  geom_point(aes(y=Female), color = "#ed7c31", size = 4) +
  geom_text_repel(aes(label = Female, y=Female), size=3.8,
            color="gray40", nudge_y = -6000) +
  annotate("text", x = 2021.35, y = 360000, label = "Female", color="#ed7c31",
           fontface=2, size=4.6) + 
  annotate("text", x = 2021.28, y = 231000, label = "Male", color="#5b9bd5",
           fontface=2, size=4.6) +
  annotate("text", x = 2020.5, y = 380000, label = "COVID-19 Pandemic", 
           color="white",fontface=2, size=4) +
  labs(title = "IPTA Enrollments througout the year 2017 to 2021",
       subtitle = "Number of Enrollments") +
  theme( 
    plot.title = element_text(color="gray50", size=16,face="bold",
      vjust = 8),
    plot.subtitle = element_text(color="gray50", hjust = -0.1,
      vjust = 3),
    panel.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color="gray50"),
    axis.text.y = element_text(color="gray50"),
    axis.ticks.y = element_line(color="gray"),
    axis.ticks.x = element_line(color="gray"),
    axis.line.y = element_line(colour="gray"),
    axis.line.x = element_line(colour="gray"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(1.5,1,1,1), "cm"))

# - to save higher quality plot
#ggsave(file="lineplot.png", dpi=500)
















