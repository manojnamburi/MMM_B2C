###Function to get the spend patterns by media and quarter
get_spend_plot <- function(spend_data,media,display = 'percent',display_media = NULL){
    if(!('qy' %in% colnames(spend_data))){
        spend_data = get_quarters(spend_data)
    }
    ##For human readable graphics output
    if(is.null(display_media)){
        ##Remove spend if present
        display_media = sapply(media,function(x){gsub(pattern = 'spend|Spend',replacement = '',x)})
        display_media = sapply(display_media,Caps)
    }
    
    
    #quo_media = rlang::sym(media)
    
    ##Get the order of the graph
    qy_levels = spend_data %>%
                dplyr::select(qy) %>%
                dplyr::distinct() %>%
                rowwise() %>%
                dplyr::mutate(year = str_split(qy,'_')[[1]][2]) %>%
                dplyr::arrange(year,qy) %>%
                select(qy) %>%
                dplyr::distinct() %>%
                pull()
    ##Get the plot data based on the type of display chosen. If percentage, display percentage of the entire spend data or 
    ##display as an actual dollar value
    
    if(display=='percent'){
        all_spend =  spend_data %>% 
                     dplyr::group_by(qy) %>%
                     dplyr::summarize_at(.vars = vars(media), .funs = sum) %>%
                     dplyr::mutate(total_qy_spend = rowSums(.[media])) %>%
                     dplyr::mutate_at(.vars = vars(media),.funs = ~(./sum(total_qy_spend))) %>%
                     rowwise() %>%
                     dplyr::mutate(year = str_split(qy,'_')[[1]][2]) %>%
                     dplyr::arrange(year) %>%
                     dplyr::select(qy,media) %>%
                     dplyr::mutate(qy = factor(qy,levels = qy_levels)) %>%
                     tidyr::gather(media,spend,-qy) %>%
                     dplyr::mutate(media = recode(media,!!!display_media),
                                   media = factor(media,levels = as.character(display_media))) %>%
                     dplyr::group_by(qy) %>%
                     dplyr::arrange(qy,desc(media)) %>%
                     dplyr::mutate(label = percent(round(spend,6),accuracy = 0.01),
                                   lab_ypos = cumsum(spend) -0.5*spend) %>%
                     as.data.frame()
        
    }else{
        all_spend = spend_data %>% 
                    dplyr::group_by(qy) %>%
                    dplyr::summarize_at(.vars = vars(media), .funs = sum) %>%
                    rowwise() %>%
                    dplyr::mutate(year = str_split(qy,'_')[[1]][2]) %>%
                    dplyr::arrange(year) %>%
                    dplyr::select(qy,media) %>%
                    dplyr::mutate(qy = factor(qy,levels = qy_levels)) %>%
                    tidyr::gather(media,spend,-qy) %>%
                    dplyr::mutate(media = recode(media,!!!display_media),
                                  media = factor(media,levels = as.character(display_media))) %>%
                    dplyr::group_by(qy) %>%
                    dplyr::arrange(qy,desc(media)) %>%
                    dplyr::mutate(label = format_money(spend,0),
                                  lab_ypos = cumsum(spend) -0.5*spend) %>%
                    as.data.frame()
        
    }
    
    ##Get the totals by quarter
    all_qy_totals = spend_data %>%
                    dplyr::select(qy,media) %>%
                    tidyr::gather(key = media,value = spend,-qy) %>%
                    dplyr::group_by(qy) %>%
                    dplyr::summarize(total_spend = sum(spend)) %>%
                    dplyr::mutate(label_text =format_money(total_spend,0))
    
    ##Get the position to display the total spend
    max_y_data = all_spend %>%
                 dplyr::group_by(qy) %>%
                 dplyr::summarize(y_lim = sum(spend))
    
    ##Join to the totals df to get the final dataset to display quarter totals
    all_qy_totals <- all_qy_totals %>%
                     dplyr::left_join(max_y_data,by = 'qy')
    
    
    ##Build the final plot
    spend_plot <-   ggplot(data = all_spend,aes(x = qy,y = spend)) + 
                    geom_col(aes(fill = media)) + 
                    geom_text(aes(y = lab_ypos, label = label, group =media)) +
                    geom_label(data = all_qy_totals,aes(y = y_lim, label = label_text),vjust = -0.5,fill="#69b3a2")+
                    theme_stata() +
                    labs(fill = 'Media') +
                    theme(axis.ticks.y = element_blank(),
                          axis.text.y = element_blank(),
                         axis.line.y = element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank())
    
    
    return(spend_plot)
    
}