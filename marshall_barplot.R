library(ggplot2)
library(gt)
library(tidyverse)
library(viridis)
library(wesanderson)
library(ggsci)

ryg.palette<-c("#DB4325", "#EDA247", "#E6E1BC","#57C4AD", "#006164")
ryg.palette.long<-c("#DB4325", "#EDA247", "#E6E1BC","#57C4AD", "#006164", "grey")
ryg.palette.short<-c("#EDA247", "#E6E1BC","#57C4AD", "#006164")

getwd()
## [1] "/Users/matthewmiller/Marshall_Descriptive"
wave.one<-read.csv("./marshall_w1_deID.csv")
#head(wave.one)
#str(wave.one)
#Graphing
Categories = c("In My Home", "In My Neighborhood", "Everywhere")
color = c("#EDA247", "#E6E1BC","#57C4AD")
labx="Loc"
laby=""
barplot(Dry_Cough, main="Dry Cough", xlab=labx, ylab=laby, names.arg = Categories, col=color, border = "black")
Categories = c("Dry Cough", "Wet Cough", "Wheeze", "Itchy or Watery Eyes", "Sore Throat", "Headache", "Shortness of Breath", "Difficult or labored breathing", "Sneezing or stuffy nose", "Nausea or vomiting", "Allergic skin reaction", "Strange taste in your mouth")
lege = c("Everywhere", "In My Neighborhood", "In My Home")
location = c("In My Home", "In My Neighborhood", "Everywhere")
color = c("#EDA247", "#E6E1BC","#57C4AD")
color2 = c("#57C4AD", "#E6E1BC","#EDA247") ### reorder legend colors to coordinate with bar charts
labx=""
laby=""
#ggplot data manipulation function
data_managing = function(count,t){
  as.data.frame(count) %>% 
    mutate(location=location,
           symptom=t)
}

ggplotting = function(count, symp){
  my_vec=character()
  symp = symp
  for(i in count) {
    word = ifelse(length(location[which(count==i)])==2,"fix",location[which(count==i)])
    output = rep(word, i)
    my_vec = c(my_vec, output)
  }
  as.data.frame(my_vec) %>% 
    mutate(symptom=symp)
}
test = ggplotting(Dry_Cough, "Dry Cough")
# Output of iteration i
output2 = rep(symp, z)           # Output of iteration i
my_list[[z]] = output        # Store output in list

#Counts
wave_grouped = wave.one %>% 
  group_by(impact_cat) %>% 
  sum(.$symptoms_where_1_1)
  wave.one$symptoms_where_1_1
  
Dry_Cough = c(sum(wave.one$symptoms_where_1_1, na.rm=T),sum(wave.one$symptoms_where_1_2, na.rm=T),sum(wave.one$symptoms_where_1_3, na.rm=T))
Wet_Cough = c(sum(wave.one$symptoms_where_2_1, na.rm=T),sum(wave.one$symptoms_where_2_2, na.rm=T),sum(wave.one$symptoms_where_2_3, na.rm=T))
Wheeze = c(sum(wave.one$symptoms_where_3_1, na.rm=T),sum(wave.one$symptoms_where_3_2, na.rm=T),sum(wave.one$symptoms_where_3_3, na.rm=T))
Itchy_Watery_Eyes = c(sum(wave.one$symptoms_where_4_1, na.rm=T),sum(wave.one$symptoms_where_4_2, na.rm=T),sum(wave.one$symptoms_where_4_3, na.rm=T))
Sore_Throat = c(sum(wave.one$symptoms_where_5_1, na.rm=T),sum(wave.one$symptoms_where_5_2, na.rm=T),sum(wave.one$symptoms_where_5_3, na.rm=T))
Headache = c(sum(wave.one$symptoms_where_6_1, na.rm=T),sum(wave.one$symptoms_where_6_2, na.rm=T),sum(wave.one$symptoms_where_6_3, na.rm=T))
Shortness_of_Breath = c(sum(wave.one$symptoms_where_7_1, na.rm=T),sum(wave.one$symptoms_where_7_2, na.rm=T),sum(wave.one$symptoms_where_7_3, na.rm=T))
Difficult_or_labored_breathing = c(sum(wave.one$symptoms_where_8_1, na.rm=T),sum(wave.one$symptoms_where_8_2, na.rm=T),sum(wave.one$symptoms_where_8_3, na.rm=T))
Sneezing_or_stuffy_nose = c(sum(wave.one$symptoms_where_9_1, na.rm=T),sum(wave.one$symptoms_where_9_2, na.rm=T),sum(wave.one$symptoms_where_9_3, na.rm=T))
Nausea_or_vomiting = c(sum(wave.one$symptoms_where_10_1, na.rm=T),sum(wave.one$symptoms_where_10_2, na.rm=T),sum(wave.one$symptoms_where_10_3, na.rm=T))
Allergic_skin_reaction = c(sum(wave.one$symptoms_where_11_1, na.rm=T),sum(wave.one$symptoms_where_11_2, na.rm=T),sum(wave.one$symptoms_where_11_3, na.rm=T))
Strange_taste_in_your_mouth = c(sum(wave.one$symptoms_where_12_1, na.rm=T),sum(wave.one$symptoms_where_12_2, na.rm=T),sum(wave.one$symptoms_where_12_3, na.rm=T))
m= matrix(c(Dry_Cough, Wet_Cough, Wheeze, Itchy_Watery_Eyes, Sore_Throat, Headache, Shortness_of_Breath, Difficult_or_labored_breathing, Sneezing_or_stuffy_nose, Nausea_or_vomiting, Allergic_skin_reaction, Strange_taste_in_your_mouth), nrow =3, ncol = 12)

#ggplot data frame
#add impact_cat as column for mapping
df = rbind(ggplotting(Dry_Cough,"Dry Cough"),
           ggplotting(Wet_Cough,"Wet Cough"),
           ggplotting(Wheeze,"Wheeze"),
           ggplotting(Itchy_Watery_Eyes,"Itchy/Watery Eyes"),
           ggplotting(Sore_Throat,"Sore Throat"),
           ggplotting(Headache,"Headache"),
           ggplotting(Shortness_of_Breath,"Shortness of Breath"),
           ggplotting(Difficult_or_labored_breathing,"Difficult or labored breathing"),
           ggplotting(Sneezing_or_stuffy_nose,"Sneezing or stuffy nose"),
           ggplotting(Nausea_or_vomiting,"Nausea or vomiting"),
           ggplotting(Allergic_skin_reaction,"Allergic skin reaction"),
           ggplotting(Strange_taste_in_your_mouth,"Strange taste in your mouth"))  

symptoms = c("Dry Cough", "Wet Cough", "Wheeze", "Itchy or Watery Eyes", "Sore Throat", "Headache", "Shortness of Breath", "Difficult or labored breathing", "Sneezing or stuffy nose", "Nausea or vomiting", "Allergic skin reaction", "Strange taste in your mouth")
ggplot(data=df, aes(x=symptom, fill=my_vec))+
  geom_bar()+
  ggtitle(paste0("Total number of Respondants: ",sum(!is.na(wave.one$progress))),"Locations of Symptoms")+
  xlab("Symptoms")+
  ylab("Number of People")+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

# barplot(m, main="Location of Symptoms", xlab=labx, ylab=laby, las=2, cex.names=0.7, names.arg = Categories, col=color, border = "black")
# legend("topright", lege, fill=color2, cex = 0.5) 
# text(x=p,y=m,labels=m)