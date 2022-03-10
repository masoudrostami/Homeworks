

bloom_df <- read.csv('bloom_df.csv')

head(bloom_df)


names(bloom_df)

str(bloom_df)

getwd()
########################### Task 1 

### Task 1 A

ggplot(bloom_df ,aes(x=logbodysize,y=trophic_position,colour=reg)) + 
  geom_point(size=4)+stat_smooth(method="lm", size=2)+theme_bw()+
  theme(legend.position = 'bottom', plot.title = element_text(size = 20, colour="black",face = "bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size = 14, colour="black",face = "bold"),
        panel.border = element_rect(size = 2, colour = "black"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



### Task 1 B


ggplot(bloom_df ,aes(x=logbodysize,y=trophic_position)) + 
  geom_point(size=4)+stat_smooth(method="lm", size=2)+theme_bw()+
  theme(legend.position = 'bottom', plot.title = element_text(size = 20, colour="black",face = "bold"),
        axis.text = element_text(size=10),
        axis.title = element_text(size = 14, colour="black",face = "bold"),
        panel.border = element_rect(size = 2, colour = "black"),
        legend.title = element_text(size = 12, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())





################# TAsk 2

names(bloom_df)

Bloom_long_df <- bloom_df %>%
  pivot_longer(cols = c(logbodysize, trophic_position), names_to = "cat", values_to = "value")


head(Bloom_long_df,10)




plot1<-ggplot(data = Bloom_long_df, aes(x = reg, y = value, fill = cat)) +
  facet_wrap(~cat, nrow = 1, scales = "free") + 
  stat_summary(fun = mean,geom = "bar") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",color = "black")+theme_bw()+
  theme(plot.title = element_text(size = 20, colour="black",face = "bold"),
        axis.text = element_text(size=8),
        panel.border = element_rect(size = 1.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot1




plot2 <- ggplot(data = Bloom_long_df, aes(x = reg, y = value, colour = cat)) +
  facet_wrap(~cat, nrow = 1, scales = "free") +  
  stat_summary(fun.data = mean_cl_boot,geom = "errorbar", color = "black") + 
  stat_summary(fun = mean,geom = "point", size = 5)+theme_bw()+
  theme(plot.title = element_text(size = 20, colour="black",face = "bold"),axis.text = element_text(size=8),
        panel.border = element_rect(size = 1.5, colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot2


plot3 <- ggplot(data = Bloom_long_df, aes(x = reg, y = value, fill = cat)) +
  facet_wrap(~cat, nrow = 1, scales = "free") + geom_boxplot()+theme_bw()+theme_bw()+
  theme(plot.title = element_text(size = 20, colour="black",face = "bold"),
        axis.text = element_text(size=8),
        panel.border = element_rect(size = 1.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot3


plot4 <- ggplot(data = Bloom_long_df, aes(x = reg, y = value, colour = cat)) +
  facet_wrap(~cat, nrow = 1, scales = "free") + geom_point(size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               color = "black") + stat_summary(fun = mean, geom = "point",
                                               size = 5, colour = "black")+theme_bw()+
  theme(plot.title = element_text(size = 20, colour="black",face = "bold"),
        axis.text = element_text(size=8),
        panel.border = element_rect(size = 1.5, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot4



plot1  + plot2  + plot3  + plot4  + plot_layout(ncol = 2,nrow = 2)

ggarrange(plot1, plot2, plot3, plot4,ncol = 2,nrow = 2 )

install.packages("ggpubr")
library(ggpubr)









###################################### Task 3


########### Worst plot

names(bloom_df)
Bad_plot <- ggplot(data = bloom_df, aes(x = genus, y = logbodysize)) + geom_bar(stat = "identity", position = "dodge") + ggtitle("Worst") +
  theme(title = element_text(size = 30,colour = "black", face = "bold"))
Bad_plot



########### Great plot


names(bloom_df)




Good_plot <- ggplot(data = bloom_df, aes(x = genus, y = logbodysize,fill=reg)) + geom_bar(stat = "identity", position = "dodge") + 
  theme_bw()+ ggtitle("Good Plot") +theme(legend.position = 'bottom', axis.text.x = element_text(angle=90),
        axis.text = element_text(size=8),
        axis.title = element_text(size = 20, colour="black",face = "bold"),
        panel.border = element_rect(size = 1.5, colour = "black"),
        legend.title = element_text(size = 16, colour="black",face = "bold",vjust = 1),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


Good_plot



Bad_plot + Good_plot 





















  


  
