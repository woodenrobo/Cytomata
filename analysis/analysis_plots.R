
sample_size_bars <- function {
    count.table <- as.data.frame(table(factor(exprs_set$sample, levels = stringr::str_sort(unique(sample), numeric = TRUE))))
    setwd(fullpath)
    write.csv(x = count.table, file = paste0(data_sub,'_count_table.csv'))  
    
}

batch_size_bars <- function {
    
}

## save a table with counts per sample for absolute cell number calculation from wbc later ###############################################################
count.table <- as.data.frame(table(factor(exprs_set$sample, levels=stringr::str_sort(unique(sample), numeric = TRUE))))
setwd(fullpath)
write.csv(x = count.table, file = paste0(data_sub,'_count_table.csv'))



#plot a barchart with event number by sample

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
setwd(fullpath)
png(type='cairo',filename = paste0(naming,'_events_per_sample_', date, '.png'),
    width = 2500*(length(unique(exprs_set$sample))/37),
    height = 2000)
print(ggplot(data.frame(exprs_set),
  aes(x=factor(exprs_set$sample, levels=stringr::str_sort(unique(sample), numeric = TRUE))))+
  geom_bar(fill='darkorange')+
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25, size=8)+
  xlab(element_blank())+
  ylab('Event count')+
  theme_cowplot()+
  theme(text=element_text(size=40),
  axis.text.x = element_text(color = "black", size = 30, angle = 90, hjust = 1, vjust = 0.5, face = "plain"),
  axis.text.y = element_text(color = "black", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain")
  )
)
invisible(dev.off())