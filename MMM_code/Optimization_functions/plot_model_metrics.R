##Function to generate Facebook Robyn style one pager - Metrics plot
##Outputs the media contribution by media, spend contribution, elasticity as well as fit metrics in subtitle of the plot
plot_elas_contrib_plot <- function(plot_contrib_data,plot_elastic_data,fit_metrics,dma_name){
    
    scaling_fac = max(plot_elastic_data['value'])/max(plot_contrib_data['value'])*1.1

    
    ##Generate the plot
    out_plot <- ggplot(data = plot_contrib_data,aes(x = media,y=value,fill = variable)) +
                geom_bar(stat = 'identity',position = 'dodge',width = 0.5) + 
                geom_text(aes(label = paste0(round(value, 2), "%")), color = "darkblue", position = position_dodge(width = 0.5), fontface = "bold") +
                geom_line(data = plot_elastic_data,aes(x = media,y = value/scaling_fac,group = 1,color = variable),inherit.aes = FALSE) +
                geom_point(data = plot_elastic_data,aes(x = media,y = value/scaling_fac,group = 1,color = variable ),inherit.aes = FALSE,size = 2) +
                geom_text(data = plot_elastic_data,aes(label = round(value, 2),x = media,y = value/scaling_fac,group = 1,color = variable ),hjust = -0.5,inherit.aes = FALSE, fontface = "bold") +
                scale_y_continuous(sec.axis = sec_axis(~ . * scaling_fac),name = 'Elasticity') +
                labs(title = sprintf('Contribution and Elasticity plot %s',dma_name),
                     subtitle = sprintf('R2: %s MAPE: %s Total Media Contribution: %s',
                                        scales::percent(fit_metrics$R2,accuracy = 0.01),
                                        scales::percent((fit_metrics$MAPE/100),accuracy = 0.01),
                                        scales::percent((fit_metrics$Media_contribution/100),accuracy = 0.01))) +
                scale_fill_brewer(palette = "Paired") +
                coord_flip() +
                theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank(),
                     axis.title.y = element_blank(),legend.title = element_blank(),
                      plot.subtitle = element_text(face = 'bold')) 
    
    return(out_plot)

    
}

##Generate the metrics plot for each DMA and brand
gen_contrib_elas_plots <- function(results_df,activity_based_media = NULL,elas_col = 'Elasticity_up_0.1'){
    output_list_brand <- list()
    output_list_dma <- list()
    
    brand_vec <- toupper(c('Altice','Optimum','Suddenlink'))
    
    ##Plotting the brand level results
    for(brand in brand_vec){
        brand_metrics <- results_df %>%
                         filter(dma == brand) %>%
                         select(media,mean_spend,Media_contribution,!!rlang::sym(elas_col),R2,MAPE)
        
        plot_contrib_data <- brand_metrics %>%
                             filter(media!='Total') %>%
                             mutate(mean_spend = ifelse(media%in%activity_based_media,0,mean_spend)) %>%
                             mutate(Spend_contribution = (mean_spend*100)/mean_spend[media == 'spend_media_total']) %>%
                             filter(media != 'spend_media_total') %>%
                             select(media,Media_contribution,Spend_contribution) %>%
                             gather(key = variable, value = value,-media) %>%
                             mutate(media = factor(media,levels = rev(media_names_vec)))
        
        plot_elastic_data <- brand_metrics %>%
                             select(media,value = !!rlang::sym(elas_col)) %>%
                             mutate(variable = 'Elasticity',
                                   media = factor(media,levels = rev(media_names_vec)))
        
        fit_metrics <- brand_metrics %>%
                       filter(media == 'Total') %>%
                       select(R2,MAPE,Media_contribution)
        
        brand_plot <- plot_elas_contrib_plot(plot_contrib_data,plot_elastic_data,fit_metrics,brand)
        
        tmp_list <- list(brand_plot)
        names(tmp_list) <- brand
        output_list_brand <- append(output_list_brand,tmp_list)        
        
    }
    
    ##Generate DMA level outputs
    for(DMA in dma_names_vec[-var_id_opt]){
        dma_metrics <- results_df %>%
                         filter(dma == DMA) %>%
                         select(media,mean_spend,Media_contribution,!!rlang::sym(elas_col),R2,MAPE)
        
        plot_contrib_data <- dma_metrics %>%
                             filter(media!='Total') %>%
                             mutate(mean_spend = ifelse(media%in%activity_based_media,0,mean_spend)) %>%
                             mutate(Spend_contribution = (mean_spend*100)/mean_spend[media == 'spend_media_total']) %>%
                             filter(media != 'spend_media_total') %>%
                             select(media,Media_contribution,Spend_contribution) %>%
                             gather(key = variable, value = value,-media) %>%
                             mutate(media = factor(media,levels = rev(media_names_vec)))
        
        plot_elastic_data <- brand_metrics %>%
                             select(media,value = !!rlang::sym(elas_col)) %>%
                             mutate(variable = 'Elasticity',
                                   media = factor(media,levels = rev(media_names_vec)))
        
        fit_metrics <- dma_metrics  %>%
                       filter(media == 'Total') %>%
                       select(R2,MAPE,Media_contribution)
        
        dma_plot <- plot_elas_contrib_plot(plot_contrib_data,plot_elastic_data,fit_metrics,DMA)
        
        tmp_list <- list(dma_plot)
        names(tmp_list) <- DMA
        output_list_dma <- append(output_list_dma,tmp_list)        
        
    }
    
    output_list <- list('brand_plots' = output_list_brand,
                       'DMA_plots' = output_list_dma)
    
    return(output_list)
}