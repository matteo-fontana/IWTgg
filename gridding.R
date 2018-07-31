library(ggplot2)
library(gridExtra)
library(ggpubr)
#####gridding#####
data_global_plot=ggarrange(plot_list$data_plot,plot_list$pval_plot_F,ncol=2,legend = 'bottom')
data_global_plot




coeff_global_plot=ggarrange(plot_list$coeff_plot,plot_list$pval_t_plot,ncol=2,common.legend=T,legend='bottom')
coeff_global_plot

p1=plot_list$coeff_plot
p1$
  
  