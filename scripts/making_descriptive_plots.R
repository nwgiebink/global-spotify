library(tidyverse)


# making the preprocess bar plot - number of attributes
data <- data.frame(Sets = c("Orignial Data Set","Cleaned Data Set","Music Attributes"),counts =c(59,25,13))


ggplot(data = data, aes(x = reorder(Sets, -counts), y = counts, fill = Sets)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=counts), vjust=-0.4, size=3.5)+
  scale_fill_manual(values=c("#069680","#960664","#380696")) +
  labs(x = "Number of attributes") +
  theme_classic() +
  theme(axis.title = element_text(size = rel(1.8))) 



# barplot of hierarchie, sidewise
hierar <- data.frame(layers = c("playlist", "album","song"), counts = c(23,535,638))
  
ggplot(data = hierar, aes(x = reorder(layers, -counts), y = counts, fill = layers)) +
  geom_bar(stat = "identity", width = 0.4) + 
  geom_text(aes(label=counts), hjust=-0.4, size=3.5)+
  scale_fill_manual(values=c("#069680","#960664","#380696")) +
  labs(x = "Layers", y="Counts") +
  theme_classic() +
  theme(axis.title = element_text(size = rel(1.8))) +
  coord_flip() 

dev.new(width = 100, height = 150, unit = "px")

# second, not preselected attemp of makeing a hierarchy

hierar2 <- data.frame(
  layers = c("track.type","track.album.album_type","country","track.album.name","track.album.id","track.name","track.id"),                    counts = c(1,3,23,530,535,634,638))

ggplot(data = hierar2, aes(x = reorder(layers, -counts), y = counts, fill = layers)) +
  geom_bar(stat = "identity", width = 0.4) + 
  geom_text(aes(label=counts), hjust=-0.4, size=3.5)+
  scale_fill_manual(values=c("#069680","#960664","#380696", "#28B7EC","#D20D71","#D35612","#D96C37")) +
  labs(x = "Layers", y="Counts") +
  theme_classic() +
  theme(axis.title = element_text(size = rel(1.8))) +
  coord_flip() 
dev.new(width = 100, height = 150, unit = "px")
