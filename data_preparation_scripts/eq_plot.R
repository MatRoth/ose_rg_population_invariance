
frq_tab <- function(responses){
  f_tab<-tibble(scale = sort(unique(responses)),
                freq = as.numeric(table(responses)),
                cum_freq = cumsum(freq),
                rel_cum_freq = cum_freq/sum(freq),
                rel_cum_freq_mid = map2_dbl(rel_cum_freq,
                                            c(0,rel_cum_freq[-length(rel_cum_freq)]),
                                            ~((..1-..2)/2)+..2))
  zero_row <- rep(0,5)
  names(zero_row) <- names(f_tab)
  bind_rows(zero_row,f_tab)
}

add_x_axis <- function(plt,x_tab){
  plt+
    #line
    geom_segment(aes(x = scale[1],
                     xend= scale[length(scale)],
                     y = 0,
                     yend = 0),
                 size = 1.5)+
    #major ticks
    geom_segment(data = x_tab[-nrow(x_tab),],
                 aes(x = scale+0.5,
                     xend = scale+0.5,
                     y = -3,
                     yend = 0),
                 size = 1.5,inherit.aes = F)+
    #minor ticks
    geom_segment(aes(x = scale,
                     xend = scale,
                     y = -1.5,
                     yend = 0),
                 size = 1.5)+
    #major tick labels
    geom_text(data = x_tab[-1,],
              aes(x = scale-0.5,
                  y = -5,
                  label = scale),
              size = 5)
}


plot_base <- function(tab){
  plt<-tab %>%
    ggplot(aes(x=scale,y=rel_cum_freq*100))+
    geom_line(color="#39586d",size=2)+
    geom_point(aes(scale,rel_cum_freq*100),
               color="#39586d",size=4)+
    theme_void(base_size = 15,base_family = "Calibri")+
    theme(text = element_text(size=28))+
    
    #y-axis line
    geom_segment(aes(x = 0, xend = 0,y = 0, yend = 100),
                 size = 1.5)+
    #y-axis ticks
    geom_segment(data = tibble(x = -0.1,
                               xend = 0,
                               y = seq(0,100,20),
                               yend = seq(0,100,20)),
                 aes(x = x,y = y, xend = xend, yend = yend),
                 size = 1.5)+
    coord_fixed(ratio = 1/(100/nrow(tab)),clip = "off",expand = T)
}

add_y_ticks <- function(plt,tab){
  plt+
    #y-axis tick labels
    geom_text(data = tibble(x = -sqrt(max(tab$scale))*0.2,y = seq(0,100,20)),
              aes(x,y,label = y),
              size = 5)
}

add_labs <- function(plt,tab){
  plt + 
    geom_text(data = tab[-1,],
              aes(x = (max(tab$scale)-min(tab$scale))/2,
                  y = -13,
                  label = if(tab[1,1] == "x") "Scores of instrument X" else "Scores of instrument Y"),
              size = 8) +
    geom_text(data = tibble(x = -sqrt(max(tab$scale))*0.5,y = 50, label = "Interpolated percentiles"),
              aes(x = x,y = y,label = label,angle = 90),
              size = 8)
}


plot_eq <- function(x,y,resp_opt = NULL){
  if(is.null(resp_opt)) resp_opt <- (1:length(unique(x)))+1 else resp_opt <- resp_opt+1
  #prepare data
  x_tab <- frq_tab(x) %>% mutate(var = "x",.before=1)
  y_tab <- frq_tab(y) %>% mutate(var = "y",.before=1)
  eq_obj <- equate(freqtab(x),freqtab(y),type = "e",smoothmethod = "bump",jmin = 0.00000001)
  x_tab <- x_tab %>%
    left_join(eq_obj$concordance %>% tibble) %>%
    select(-contains("...")) %>%
    mutate(across(c(yx,se),~if_else(is.na(.),0,.)))
  
  #create x plot
  x_plot<-plot_base(x_tab) %>% add_x_axis(x_tab) %>% add_y_ticks(x_tab) %>% add_labs(x_tab)
  x_plot<-x_plot + 
    #vline
    geom_segment(data = x_tab[resp_opt,],aes(x = scale-0.5,xend = scale-0.5, y = 0, yend = rel_cum_freq_mid*100),
                 linetype = "dashed",
                 size = 1.5,
                 color = "#07b3a5")+
    #hline
    geom_segment(data = x_tab[resp_opt,],aes(x = scale-0.5, xend = max(x_tab$scale)+0.2*max(x_tab$scale),y=rel_cum_freq_mid*100,yend=rel_cum_freq_mid*100),
                 linetype = "dashed",
                 size = 1.5,
                 color = "#07b3a5")
  if(length(resp_opt)==1){
    x_plot <- x_plot + geom_text(data = x_tab[resp_opt,],
                                 aes(x = scale+1.5,
                                     y = (rel_cum_freq_mid*100)+15,
                                     label = paste0("P(",x_tab[resp_opt,"scale"],") = ",round(x_tab[resp_opt,"rel_cum_freq_mid"],2)*100)),
                                 size=8)
  }
  #y plot
  y_plot <- plot_base(y_tab) %>% add_x_axis(y_tab) %>% add_y_ticks(y_tab) %>% add_labs(y_tab)
  y_plot <- y_plot + 
    #vline
    geom_segment(data = x_tab[resp_opt,],aes(x = yx-0.5,xend = yx-0.5, y = rel_cum_freq_mid*100, yend = 0),
                 size = 1.5,
                 color = "#07b3a5",
                 arrow = arrow())+
    #hline
    geom_segment(data = x_tab[resp_opt,],aes(x = yx-0.5, xend = -0.5,y=rel_cum_freq_mid*100,yend=rel_cum_freq_mid*100),
                 linetype = "dashed",
                 size = 1.5,
                 color = "#07b3a5")
  if(length(resp_opt)==1){
    y_plot <- y_plot +
      geom_richtext(data = x_tab[resp_opt,],
                    aes(x = yx+1,
                        y = (rel_cum_freq_mid*100)-5,
                        label = paste0("<em>Q</em><sup>-1</sup>",
                                       "(",
                                       round(x_tab[resp_opt,"rel_cum_freq_mid"],2)*100,
                                       ") = ",
                                       round(x_tab[resp_opt,"yx"],1)
                        )),
                    size=8,fill = NA, label.color = NA)
  }
  x_plot+y_plot
}

