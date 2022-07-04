get_spend_target_trend <- function(spend_data,media_names_vec,trg,footprint='All',show_tab = T){
    
    plot_title_str = 'Overall'
    ##If the plot is for all data, set footprint to both OPT and SDL
    if(footprint == 'All'){
        footprint = c('Optimum','Suddenlink')
        
    }else{
        plot_title_str = footprint
    }
    
    ##Extract the qy levels by data
    qy_levels <- spend_data %>%
                 select(qy) %>%
                 distinct() %>%
                separate(qy, c("q", "y"), "_",remove = F) %>%
                arrange(y,q) %>%
                select(qy) %>%
                pull()
    
    ##Prepare the data to be plot
    plot_data = spend_data %>%
            filter(brand %in% footprint) %>%
            select(qy,trg,media_names_vec) %>%
            mutate(qy = factor(qy,levels = qy_levels),
                   spend = rowSums(.[media_names_vec])) %>%
            group_by(qy) %>%
            summarize_at(.vars = vars(!!rlang::sym(trg),spend),.funs = sum) %>%
            rename('trg' = trg)
    
    ##For the display table
    display_table = plot_data%>%
                    mutate(qy = gsub(pattern = '_',replacement = ' ',x = qy),
                          spend = format_money(spend,0),
                          trg =format(trg,big.mark = ',')) %>%
                    rename('Period' = 'qy',
                           'Spend($)' = 'spend',
                           'Target' = 'trg')
    
    ##For the dual axis plot, get the max spend and target to scale
    max_trg <- max(plot_data$trg)
    #min_trg <- min(plot_data$trg)
    max_spend <- max(plot_data$spend)
    #min_spend <- min(plot_data$spend)
    #spend_lim <- c(min_spend,max_spend)
    #target_lim <- c(min_trg,max_trg)
    #b <- diff(target_lim)/diff(spend_lim)
    #a <- target_lim[1] - b*spend_lim[1] 
    coeff <- max_trg/max_spend
    
    
    colors <- c('Target' = 'blue','Spend' = 'red')
    plt <- ggplot(data = plot_data , aes(x = qy,group = 1)) +
                     geom_line( aes(y=(trg),color = 'Target'),size=1) +
                     geom_line( aes(y=spend*coeff,color = 'Spend'),size = 1) + 
                      
  
                     scale_y_continuous(
    
                     # Features of the first axis
                     name = "Total Target",
                     label = comma,
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~./coeff, name="Total Spend ($)",label = comma)
                                       )  + 
            scale_color_manual(values = colors) +
            labs(title = sprintf('%s - Spend vs Target',plot_title_str)) +
            theme_stata() +
            theme(plot.title = element_text(hjust = 0.5),
                  axis.line.y.left = element_line(color = "blue"), 
                  axis.ticks.y.left = element_line(color = "blue"),
                  axis.text.y.left = element_text(color = "blue",angle = 45), 
                  axis.title.y.left = element_text(color = "blue"),
                  legend.title = element_blank(),
                  axis.line.y.right = element_line(color = "red"), 
                  axis.ticks.y.right = element_line(color = "red"),
                  axis.text.y.right = element_text(color = "red",angle = 45), 
                  axis.title.y.right = element_text(color = "red"),
                  panel.grid.major = element_line(colour = "black",size=0.1),
                  panel.grid.minor = element_line(colour = "black",linetype="dashed",size=0.1),
                  panel.grid.major.x = element_line(colour = "black",size=0.1))
    if(show_tab){
    tt <- gridExtra::ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
         tbl <- tableGrob(display_table, rows=NULL, theme=tt)
          
          grid.arrange(plt, tbl,
                       nrow=2,
                       as.table=TRUE,
                       heights=c(6,5))
        }
    else{
        plt
    }
}