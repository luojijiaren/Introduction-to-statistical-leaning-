# DS502 group project
# data visualization
# written by Ziqi Lin
# last edited Nov 26, 2016

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)
library(RColorBrewer)
library(maps)
library(lattice)
library(plotly)
library(data.table)
# using the subset.csv
data <- read.csv("C:/Users/zlin3/Downloads/clean_subset.csv")
# pie chart - loan status
loan_status_new=data%>%
  group_by(loan_status) %>%
  summarise(n=length(loan_status))
loan_status_new=as.data.frame(loan_status_new)

plot_ly(loan_status_new,
        type = "pie", 
        labels = ~loan_status, 
        values = ~n, 
        hole = 0.5,
        marker = list(colors = brewer.pal(7, "Pastel2"),
                      line = list(width = 1, color = "rgb(52, 110, 165)")),
        sort = F,
        direction = "counterclockwise",
        rotation = 90,
        textinfo = "label+percent",
        textfont = list(size = 14),
        opacity = 1,
        textposition = "outside") %>%
    layout(title = 'LOAN STATUS',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# pie chart - grade
grade_new=data%>%
  group_by(grade) %>%
  summarise(n=length(grade))
grade_new=as.data.frame(grade_new)

plot_ly(grade_new,
        type = "pie", 
        labels = ~grade, 
        values = ~n, 
        hole = 0.5,
        marker = list(colors = brewer.pal(7, "Pastel2"),
                      line = list(width = 1, color = "rgb(52, 110, 165)")),
        sort = F,
        direction = "counterclockwise",
        rotation = 90,
        textinfo = "label+percent",
        textfont = list(size = 14),
        opacity = 1,
        textposition = "outside") %>%
  layout(title = 'Grade STATUS',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# default percentage of each state
l <- list(color = toRGB("steelblue"), width = 1)
g = list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = F, lakecolor = toRGB('white'))
s_new=data%>%
  group_by(addr_state) %>%
  summarise(n=length(addr_state))
de = subset(data,is_bad=='2')
sd_new=de%>%
  group_by(addr_state) %>%
  summarise(n=length(addr_state))
s_new=as.data.frame(s_new)
sd_new=as.data.frame(sd_new)
for (i in 1:50){
  for (j in 1:46){
    if (s_new[i,1]==sd_new[j,1]){
      tmp=s_new[i,2]
      s_new[i,2]=sd_new[j,2]/tmp
    }
  }
}
s_new[c(13,21,28,29),2]=c(0,0,0,0) 

#plot_geo(s_new, locationmode = 'USA-states') %>%
#  add_trace(
#    z = ~n, locations = ~addr_state,
#    color = ~n, colors = 'Purples'
#  ) %>%
#  colorbar(title = "Millions USD") %>%
#  layout(
#    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
#    geo = g
#  )

# plotting the map:
#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}
s_new$addr_state=stateFromLower(s_new$addr_state)
colnames(s_new)=c("region","n")
states <- map_data("state")
map.df <- merge(states,s_new,by="region", all.s_new=T)

ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=n))+
  geom_path()+
  scale_fill_gradientn(colours=rev(heat.colors(9)),na.value="grey90")+
  coord_map()


