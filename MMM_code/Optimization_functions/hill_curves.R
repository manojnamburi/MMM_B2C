if((!require(tidyverse))){install.packages("tidyverse",repos="http://cran.us.r-project.org")}
if((!require(ggdark))){install.packages("ggdark",repos="http://cran.us.r-project.org")}
if((!require(gridExtra))){install.packages("gridExtra",repos="http://cran.us.r-project.org")}




#source('common_functions_mmm.R')
###Get the take-off point and inflection point by DMA and media
getSpendRegion<- function(hill_df,
                          dma_name,
                          media_name,
                          min_media_mat,
                          max_media_mat,
                          prospect_mat_media,
                          media_names_vec,
                          K,S){
    
    media_id = which(media_name == media_names_vec)
    dma_id = which(dma_name == dma_names_vec)    
    K_media = K[media_id]
    S_media = S[media_id]
    ##Locate the starting and ending point of possible inflection point
    ##Round the hill value to 3rd decimal place
    hill_df['hill_value'] = round(hill_df$hill_value,3)
    ##Using an alternate breakthrough point - Where the second derivative is maximum
    start_point = hill_df[max(which(hill_df$rate_of_slope>0)),'x']
    end_point = hill_df[min(which(hill_df$rate_of_slope<0)),'x']
    ##Assuming saturation point is the point where the second derivative is minimum
    saturation_point = hill_df[which(hill_df$rate_of_slope==min(hill_df$rate_of_slope,na.rm = T)),'x']
    
    ##Checks to see if there are no inflection points in the given region, then default to the max_value of the hill spend range
    if(is.na(end_point)){
      print('No Inflection point found as slope is continuously increasing in the range\n')
      end_point = start_point
      spend_start = hill_df[which(hill_df$rate_of_slope==max(hill_df$rate_of_slope,na.rm = T)),'x']
      inflection_point = max(hill_df$x)
    }else{
      start_point_norm = normalize_x_dma(x = start_point,min_media_mat = min_media_mat,max_media_mat = max_media_mat,
                                         prospect_mat_media = prospect_mat_media,media = media_name,dma = dma_name)
      end_point_norm = normalize_x_dma(x = end_point,min_media_mat = min_media_mat,max_media_mat = max_media_mat,
                                       prospect_mat_media = prospect_mat_media,media = media_name,dma = dma_name)
      
      
      vector_x = linspace(x1 = start_point_norm,x2 = end_point_norm,n = 1000)
      
      
      xroot_norm = uniroot(f = hill_second_derivative,lower = start_point_norm, upper = end_point_norm, K = K_media, S = S_media)$root
      
      
      
      if(length(xroot_norm)<2){
        spend_start = hill_df[which(hill_df$rate_of_slope==max(hill_df$rate_of_slope,na.rm = T)),'x']
        inflection_point = (xroot_norm*(max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id])+min_media_mat[dma_id,media_id])*prospect_mat_media[dma_id,media_id]
      }else if(length(xroot_norm)>=2){
        spend_start_norm = xroot_norm[1]
        spend_end_norm = xroot_norm[2]
        spend_start = (spend_start_norm*(max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id])+min_media_mat[dma_id,media_id])*prospect_mat_media[dma_id,media_id]
        inflection_point = (spend_end_norm*(max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id])+min_media_mat[dma_id,media_id])*prospect_mat_media[dma_id,media_id]
      }
    }
    
    
    spend_region = c('start_spend' = spend_start,
                     'end_spend' = inflection_point,
                     'saturation_point' = saturation_point)
    
    return(spend_region)
}

###All media hill curves
plotHill <- function(K,S,
                     cleaned_names_vec,
                     media_spend_df = NULL,
                     x_normal_show = F,
                     lim_x = NULL, lim_y = 0.7){
  ##Input a n*media_names_vec data frame to get the hill curves for all the media
  ##Compute the result matrix for the media
  #cleaned_names_vec = c("Radio","TV","Print","DM","Audio","Display","Video","BrandedSearch", "UnbrandedSearch", "Social")
  cat('Make sure the cleaned_names_vec has the same order as media_names_vec')
  #if(length(cleaned_names_vec)!=length(media_names_vec)){stop(cat('Please ensure the cleaned names has the same length as that of the media names vector.'))}
  if(is.null(media_spend_df)==T){
    x <- list()
    for(i in c(1:length(cleaned_names_vec))){
      x[[i]] <- linspace(0,1)
    }
    x_df <- matrix(unlist(x), nrow=100, byrow=F)
    colnames(x_df) <- cleaned_names_vec
    media_matrix_norm <- x_df
    x_normal_show = T
  }else{
    media_matrix <- as.matrix(media_spend_df)
    colnames(media_matrix) <- cleaned_names_vec
    
    media_matrix_norm <- (media_matrix - min(media_matrix))/(max(media_matrix)-min(media_matrix))
  }
  
  hill_x <- t(apply(media_matrix_norm,FUN = function(x){hill(x,K,S)},MARGIN = 1))
  
  if(x_normal_show){
    x_melt <- melt(media_matrix_norm,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = " "
  }else{
    x_melt <- melt(media_matrix,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = "Spend($)"
  }
  
  
  plot_y_data <- melt(hill_x, value.name = "y", varnames = c("id","media"),as.is = T)
  
 
  plot_data <- cbind.data.frame(x_melt,'y' = plot_y_data$y)
  plot_data$id <- NULL
  
  if(is.null(lim_x)==T){
    lim_x = max(plot_data$x)
  }
  
  display_table = data.frame('Media' = cleaned_names_vec,
                             'K' = K,
                             'S' = S)
  #lim_y = plot_data %>% filter(x <= lim_x) %>% summarize(max = max(y)) %>% pull()
  
         plt <- ggplot(data =plot_data, aes(x=x, y=y, colour=media)) +geom_line(size = 1) +
         xlim(0,lim_x) + ylim(0,lim_y) +
          dark_theme_gray() +
         ggtitle('Hill curve - Media') +
         theme(legend.position="top",
          legend.text = element_text(size=12,face = 'bold'),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
              ) +
           scale_color_brewer(palette = "Paired") + xlab(x_label) + ylab("Hill(x)")
  
         tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
         tbl <- tableGrob(display_table, rows=NULL, theme=tt)
          
          grid.arrange(plt, tbl,
                       nrow=2,
                       as.table=TRUE,
                       heights=c(3,1))
         
  #list('Plot' = plt,"Plot Data" = plot_data)
  
}


###All media hill curves
plotHill2 <- function(K,S,
                     cleaned_names_vec,
                     media_spend_df = NULL,
                     x_normal_show = F,
                     lim_x = NULL, lim_y = 0.7){
  ##Input a n*media_names_vec data frame to get the hill curves for all the media
  ##Compute the result matrix for the media
  #cleaned_names_vec = c("Radio","TV","Print","DM","Audio","Display","Video","BrandedSearch", "UnbrandedSearch", "Social")
  cat('Make sure the cleaned_names_vec has the same order as media_names_vec')
  #if(length(cleaned_names_vec)!=length(media_names_vec)){stop(cat('Please ensure the cleaned names has the same length as that of the media names vector.'))}
  if(is.null(media_spend_df)==T){
    x <- list()
    for(i in c(1:length(cleaned_names_vec))){
      x[[i]] <- linspace(0,1)
    }
    x_df <- matrix(unlist(x), nrow=100, byrow=F)
    colnames(x_df) <- cleaned_names_vec
    media_matrix_norm <- x_df
    x_normal_show = T
  }else{
    media_matrix <- as.matrix(media_spend_df)
    colnames(media_matrix) <- cleaned_names_vec
    
    media_matrix_norm <- (media_matrix - min(media_matrix))/(max(media_matrix)-min(media_matrix))
  }
  
  hill_x <- t(apply(media_matrix_norm,FUN = function(x){hill2(x,K,S)},MARGIN = 1))
  
  if(x_normal_show){
    x_melt <- melt(media_matrix_norm,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = " "
  }else{
    x_melt <- melt(media_matrix,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = "Spend($)"
  }
  
  
  plot_y_data <- melt(hill_x, value.name = "y", varnames = c("id","media"),as.is = T)
  
 
  plot_data <- cbind.data.frame(x_melt,'y' = plot_y_data$y)
  plot_data$id <- NULL
  
  if(is.null(lim_x)==T){
    lim_x = max(plot_data$x)
  }
  
  display_table = data.frame('Media' = cleaned_names_vec,
                             'K' = K,
                             'S' = S)
  #lim_y = plot_data %>% filter(x <= lim_x) %>% summarize(max = max(y)) %>% pull()
  
         plt <- ggplot(data =plot_data, aes(x=x, y=y, colour=media)) +geom_line(size = 1) +
         xlim(0,lim_x) + ylim(0,lim_y) +
          dark_theme_gray() +
         ggtitle('Hill curve - Media') +
         theme(legend.position="top",
          legend.text = element_text(size=12,face = 'bold'),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
              ) +
           scale_color_brewer(palette = "Paired") + xlab(x_label) + ylab("Hill(x)")
  
         tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
         tbl <- tableGrob(display_table, rows=NULL, theme=tt)
          
          grid.arrange(plt, tbl,
                       nrow=2,
                       as.table=TRUE,
                       heights=c(3,1))
         
  #list('Plot' = plt,"Plot Data" = plot_data)
  
}

##Hill curves by DMA

plotHillDMA <- function(K,S,beta_media_mat,
                     cleaned_names_vec,
                     media_spend_df = NULL,
                     x_normal_show = F,
                     lim_x = NULL, lim_y = 0.7){
  ##Input a n*media_names_vec data frame to get the hill curves for all the media
  ##Compute the result matrix for the media
  #cleaned_names_vec = c("Radio","TV","Print","DM","Audio","Display","Video","BrandedSearch", "UnbrandedSearch", "Social") 
  cat('Make sure the cleaned_names_vec has the same order as media_names_vec')
  cat('\n')
  dma_names_vec1 <- c('Optimum','Suddenlink','Altice',dma_names_vec)
  ##To ensure the betas are aligned to the media names vector names.
  #beta_media_mat <- beta_media_mat[,media_names_vec]  
  beta_df = data.frame(beta_media_mat)
  colnames(beta_df) <- cleaned_names_vec
  #if(length(cleaned_names_vec)!=length(media_names_vec)){stop(cat('Please ensure the cleaned names has the same length as that of the media names vector.'))}
  if(is.null(media_spend_df)==T){
    x <- list()
    for(i in c(1:length(cleaned_names_vec))){
      x[[i]] <- linspace(0,1)
    }
    x_df <- matrix(unlist(x), nrow=100, byrow=F)
    colnames(x_df) <- cleaned_names_vec
    media_matrix_norm <- x_df
    x_normal_show = T
  }else{
    media_matrix <- as.matrix(media_spend_df)
    colnames(media_matrix) <- cleaned_names_vec
    
    media_matrix_norm <- (media_matrix - min(media_matrix))/(max(media_matrix)-min(media_matrix))
  }
    
  output_plot_list = list()  
    
  
  for(DMA in dma_names_vec1){
  ##Get the beta values of all media for the DMA selected
  if(DMA == 'Optimum')
  {
      dma_id = which(dma_names_vec %in% optimum_dma)
      dma_beta_vec = as.numeric(beta_df[dma_id,])
  }else if(DMA == 'Suddenlink'){
      cat('For Suddenlink DMA\n')
      cat('Using the mean beta values across DMAs\n')
      dma_id = -1*which(dma_names_vec %in% optimum_dma)
      beta_subset = beta_df[dma_id,, drop = FALSE]
      dma_beta_vec = as.numeric(apply(beta_subset, FUN = mean, MARGIN = 2))
      
  }else if(DMA == 'Altice'){
      cat('For Altice DMA\n')
      cat('Using the mean beta values across DMAs\n')
      dma_beta_vec = as.numeric(apply(beta_df, FUN = mean, MARGIN = 2))
  }else{
      dma_id = which(dma_names_vec %in% DMA)
      dma_beta_vec = as.numeric(beta_df[dma_id,])
      
  }
  hill_x <- t(apply(media_matrix_norm,FUN = function(x){hill(x,K,S)},MARGIN = 1)*dma_beta_vec)
  
  if(x_normal_show){
    x_melt <- melt(media_matrix_norm,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = " "
  }else{
    x_melt <- melt(media_matrix,value.name = "x", varnames = c("id","media"),as.is = T)
    x_label = "Spend($)"
  }
  
  
  plot_y_data <- melt(hill_x, value.name = "y", varnames = c("id","media"),as.is = T)
  
 
  plot_data <- cbind.data.frame(x_melt,'y' = plot_y_data$y)
  plot_data$id <- NULL
  
  if(is.null(lim_x)==T){
    lim_x = max(plot_data$x)
  }
  
  display_table = data.frame('Media' = cleaned_names_vec,
                             'K' = K,
                             'S' = S,
                             'beta' = dma_beta_vec
                             )
  #lim_y = plot_data %>% filter(x <= lim_x) %>% summarize(max = max(y)) %>% pull()
  
         plt <- ggplot(data =plot_data, aes(x=x, y=y, colour=media)) +geom_line(size = 1) +
         xlim(0,lim_x) + ylim(0,lim_y) +
          dark_theme_gray() +
         ggtitle(sprintf('Hill curve - Media (%s)',DMA)) +
         theme(legend.position="top",
          legend.text = element_text(size=12,face = 'bold'),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
              ) +
           scale_color_brewer(palette = "Paired") + xlab(x_label) + ylab(expression(beta*"Hill(x)"))
  
         tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
         tbl <- tableGrob(display_table, rows=NULL, theme=tt)
          
          final_plt <- gridExtra::arrangeGrob(plt, tbl,
                       nrow=2,
                       as.table=TRUE,
                       heights=c(3,1))
      temp_list = list(final_plt)
      temp_list = setNames(temp_list,DMA)
      output_plot_list = append(output_plot_list,temp_list) 
  #list('Plot' = plt,"Plot Data" = plot_data)
  }
  names(output_plot_list) = dma_names_vec1
  return(output_plot_list)
}




###Hill curves by DMA and media
plot_hill_media <- function(data_XyZ,
                            media_names_spend,
                            media_names_vec,
                            cleaned_names_vec,
                            min_media_mat,
                            max_media_mat,
                            ratio_dma,prospect_mat_media,
                            target_max_vec,
                            dma_name,media_name,beta_media_mat,
                            target_name_var,
                            K,S,
                            var_optimization_lb = 0.7,
                            var_optimization_ub = 1.3)
{
    ##If the input DMA is Suddenlink/Altice just compute the projected graph and build hill curves and derivative curves for all the respective DMAs

  dma_vec = c(dma_name)
  dma_plot_list = list()
  
  ##If the data doesnt contain the quarter details, add the necessary columns
  quarter_cols <- c('quarter','qy')
  
  if(!(all(quarter_cols%in%colnames(data_XyZ)))){
    data_XyZ <- get_quarters(data_XyZ)
  }
  
  mean_period_spend = data_XyZ %>% 
    dplyr::group_by(dma) %>% 
    select(media_names_spend) %>%  
    dplyr::summarize_at(.vars = media_names_spend,.funs = exclude_zero_spend_weeks) %>%  
    dplyr::ungroup() %>%
    dplyr::rename_at(media_names_spend,~media_names_vec) %>% select(media_names_vec) %>%
    as.matrix()
  ##Compute the actual min and max period spend for the given DMA and media
  ##Names of the cols of the minimum and maximum actual spend
  min_name <- paste(media_name,'min',sep='_')
  max_name <- paste(media_name,'max',sep='_')
  ##Min max actual by DMA and media
  min_max_dma_media_act <- data_XyZ %>%
    dplyr::select(dma,period,media_names_spend) %>%
    dplyr::rename_at(.vars = vars(media_names_spend),.funs = ~media_names_vec) %>%
    dplyr::select(dma,media_names_vec) %>%
    dplyr::group_by(dma) %>%
    dplyr::summarize_at(.vars = vars(media_names_vec),.funs = c('min' = min,'max' = max)) %>%
    dplyr::select(-dma) %>%
    as.matrix()
  
  
  cleaned_names_list = as.list(cleaned_names_vec)
  
  names(cleaned_names_list) = media_names_vec 
  
  qy_spend_summary = data_XyZ %>%
    dplyr::group_by(dma,qy) %>% 
    dplyr::select(media_names_spend) %>% 
    dplyr::summarise_all(mean) %>% 
    dplyr::ungroup() %>%
    dplyr::rename_at(media_names_spend,~media_names_vec) %>% 
    select(dma,qy,media_names_vec)
  
  if(dma_name=='Suddenlink'){
    dma_vec = dma_names_vec[!(dma_names_vec %in% optimum_dma)]
  }else if(dma_name == 'Altice'){
    dma_vec = dma_names_vec
  }
  
  current_spend_fprint <- 0
  fprint_plot_data_df <- data.frame(matrix(0,ncol = 2,nrow = 100))
  colnames(fprint_plot_data_df) <- c('spend','target')
  min_actual_spend_fprint <- 0
  max_actual_spend_fprint <- 0
  ideal_spend_region_fprint <- c(0,0,0)
  target_vec_fprint <- rep(0,6)
  ##Initializing the normalized x vector from 0 to 5*max normalized value(1)
  x_norm_vec <- linspace(0,5)
  
  for(DMA in dma_vec){
  print(sprintf('Computing the hill plots for: %s',DMA),flush=T)
  dma_id = which(dma_names_vec==DMA)
  media_id = which(media_names_vec==media_name)
  ##Denormalizing the value based on dma and media
  x_denorm_vec = x_norm_vec*(max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id]) + min_media_mat[dma_id,media_id]
  x_denorm_vec = x_denorm_vec*prospect_mat_media[dma_id,media_id]
  
  min_media_spend <- min(x_denorm_vec)
  #min_media_spend <- 600000
  #Get the max media spend by DMA and media
  max_media_spend <- max(x_denorm_vec)

  ##Get the actual min and max spend to show later in the graph
  min_actual_spend <- min_max_dma_media_act[dma_id,min_name]
  max_actual_spend <- min_max_dma_media_act[dma_id,max_name]
  
  ratio_dma_opt = as.numeric(ratio_dma[dma_id,])
  names(ratio_dma_opt) <- media_names_vec
  
  #alternate_min <- (mean_period_spend*var_optimization_lb)[dma_id,2]
  
  x0 <- seq(min_media_spend,max_media_spend,length.out = 100)

  ##Exclude the DMAs whose spend has been zero for the media from the calculation of the saturation and breakthrough as they are estimated but never spent 
  if(max_actual_spend == 0){
    cat('Skipping ahead of this DMA as the max spend is zero\n')
    next
  }
    
  media_norm <- normalize_x_dma(x0,min_media_mat,max_media_mat,prospect_mat_media,media_name,DMA)
  
  hill_y =  hill(media_norm, K = K[media_id], S = S[media_id])
  
  hill_y[is.na(hill_y)] <- 0
  
  y = beta_media_mat[dma_id,media_id]*hill_y*target_max_vec[dma_id]*target_capita_norm_col[dma_id]
  
  
  
  ##Calculate the specific value of hill at the current spend value
  current_spend <- mean_period_spend[dma_id,media_id]
  
  current_spend_norm <- normalize_x_dma(current_spend,min_media_mat,max_media_mat,prospect_mat_media,media_name,DMA)
  
  y_current <- get_target_dma_media(current_spend,
                                    min_media_mat,
                                    max_media_mat,
                                    beta_media_mat,
                                    prospect_mat_media,
                                    media_name,
                                    DMA,K,S,
                                    target_capita_norm_col,
                                    target_max_vec)
  y_current <- round(y_current,0)
  ##Calculate the hill function value for the average quarterly spend 
  quarterly_spend <- qy_spend_summary %>% filter(dma == DMA) %>% select(qy,media_name) 
  
  quarterly_spend_norm <- normalize_x_dma(quarterly_spend[media_name],min_media_mat,max_media_mat,prospect_mat_media,media_name,DMA)
  
  y_quarter <- beta_media_mat[dma_id,media_id]*hill(quarterly_spend_norm, K = K[media_id], S = S[media_id])*target_max_vec[dma_id]*target_capita_norm_col[dma_id]
  y_quarter[is.na(y_quarter)] <- 0
  
  x_vec <- unlist(quarterly_spend[media_name])
  y_vec <- unlist(y_quarter)
  color_vec <- unlist(quarterly_spend['qy'])
  
  y_hat = hill_derivative(media_norm, K = K[media_id], S = S[media_id])
  
  y_sec_der = hill_second_derivative(media_norm, K = K[media_id], S = S[media_id])
  
  hill_df <- data.frame("x" = x0, "hill_value" = hill_y, "hill_deriv" = y_hat, "rate_of_slope" = y_sec_der)
  
  
  
  plot_data <- data.frame("spend" = x0,"target" = y)
  
  ##Compute point of influxion,saturation and breakthrough
  ideal_spend_region = getSpendRegion(hill_df,DMA,media_name,min_media_mat,max_media_mat,prospect_mat_media,media_names_vec,K,S)
    
  cleaned_media_name <- cleaned_names_list[media_name]
  ##Get the target values at the specific points
  
  
  breakthrough_target <- plot_data %>%
                      filter(spend == ideal_spend_region[1]) %>%
                      select(target) %>%
                      summarize_at(.vars = vars('target'),.funs=~round(.,0)) %>%
                      pull()

  
  current_spend_target = round(y_current,0)
  
  inflection_spend = ideal_spend_region[2]
  
  y_inflection <- get_target_dma_media(inflection_spend,
                                    min_media_mat,
                                    max_media_mat,
                                    beta_media_mat,
                                    prospect_mat_media,
                                    media_name,
                                    DMA,K,S,
                                    target_capita_norm_col,
                                    target_max_vec)
  inflection_target <- round(y_inflection,0)
 
  min_actual_target <- get_target_dma_media(min_actual_spend,
                                           min_media_mat,
                                           max_media_mat,
                                           beta_media_mat,
                                           prospect_mat_media,
                                           media_name,
                                           DMA,K,S,
                                           target_capita_norm_col,
                                           target_max_vec)
  
  min_actual_target <- round(min_actual_target,0)
  
  
  max_actual_target <- get_target_dma_media(max_actual_spend,
                                            min_media_mat,
                                            max_media_mat,
                                            beta_media_mat,
                                            prospect_mat_media,
                                            media_name,
                                            DMA,K,S,
                                            target_capita_norm_col,
                                            target_max_vec)
  max_actual_target = round(max_actual_target)
  
  saturation_target = plot_data %>%
                      filter(spend == ideal_spend_region[3]) %>%
                      select(target) %>%
                      summarize_at(.vars = vars('target'),.funs=~round(.,0)) %>%
                      pull()
  
  dma_target_vec = c(breakthrough_target,current_spend_target,inflection_target,min_actual_target,max_actual_target,saturation_target)
  display_table = data.frame('TypeOfSpend' = c('Breakthrough','Current spend','Inflection point','Minimum Spend','Maximum Spend','Saturation Point'),
                             'Spend' = c(format_money(ideal_spend_region[1],0),format_money(current_spend,0),format_money(ideal_spend_region[2],0),format_money(min_actual_spend,0),format_money(max_actual_spend,0),format_money(ideal_spend_region[3],0)),
                             'Target' = dma_target_vec
                             )
  
  
  ##Aggregate the current spend, target and inflection points at the fprint level
  

  current_spend_fprint <- current_spend_fprint + current_spend
  fprint_plot_data_df['spend'] <-  fprint_plot_data_df['spend'] + x0
  fprint_plot_data_df['target'] <- fprint_plot_data_df['target'] + y
  min_actual_spend_fprint <- min_actual_spend_fprint + min_actual_spend
  max_actual_spend_fprint <- max_actual_spend_fprint + max_actual_spend
  ideal_spend_region_fprint <- ideal_spend_region_fprint + ideal_spend_region
  max_plot_x <- max(ideal_spend_region,max_actual_spend,na.rm = T)
  target_vec_fprint <- target_vec_fprint + dma_target_vec
  ##Projected target - DMA
  plt<- ggplot(data = plot_data,aes(x=spend,y=target)) + geom_line() + 
    geom_line(aes(x=current_spend, color = 'Current spend'),size = 1) +
    geom_line(aes(x=current_spend*var_optimization_ub,color = 'Optimization region'),size = 1) +
    geom_line(aes(x=current_spend*var_optimization_lb,color = 'Optimization region'),size = 1)+
    geom_line(aes(x=min_actual_spend,color = 'Min Max Spend'),size = 1)+
    geom_line(aes(x=max_actual_spend,color = 'Min Max Spend'),size = 1)+
    #annotate("segment", x = tv_spend_region[2], xend = tv_spend_region[2], y = max(plot_data$target)+25, yend = max(plot_data$target), colour = "green", size=1, alpha=0.6, arrow=arrow()) + 
    geom_line(aes(x = ideal_spend_region[1],color = 'Breakthrough Point'),size = 1)+
    annotate("text",x = ideal_spend_region[1],y = max(plot_data$target)/2,label = 'Breakthrough point',angle = 90,vjust = -0.5)+
    geom_line(aes(x = ideal_spend_region[2],color = 'Inflection point'),size = 1) +
    annotate("text",x = ideal_spend_region[2],y = max(plot_data$target)/2,label = 'Inflection point',angle = 90,vjust = -0.5)+
    geom_line(aes(x = ideal_spend_region[3],color = 'Saturation point'),size = 1) +
    annotate("text",x = ideal_spend_region[3],y = max(plot_data$target)/2,label = 'Saturation point',angle = 90,vjust = -0.5)+
    #annotate("rect", xmin=ideal_spend_region[1], xmax=ideal_spend_region[2], ymin=0, ymax=max(plot_data$target), alpha=0.2, fill="green")+
    annotate("rect", xmin=current_spend*var_optimization_lb, xmax=current_spend*var_optimization_ub, ymin=0, ymax=max(plot_data$target), alpha=0.2, fill="yellow")+
    #geom_point(data = quarterly_spend,aes(x=x_vec, y = y_vec, color = color_vec),size = 5)+
    ggtitle(sprintf('%s projection for %s,%s',target_name_var,cleaned_media_name,DMA))+
    dark_theme_gray() +
    scale_color_manual(values = c('hotpink1','yellow4','violetred1','dodgerblue','yellow','red2'))+
    xlab('Spend($)') +
    xlim(0,max_plot_x) + ##Limiting to the max of either saturation or inflection or max spend
    ylab(sprintf('%s',Caps(target_var))) +
    theme(legend.position="top",
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(display_table, rows=NULL, theme=tt)
  
  a1 <- gridExtra::arrangeGrob(plt, tbl,
               nrow=2,
               as.table=TRUE,
               heights=c(3,1))
  
  ##Hill curve
  a2<- ggplot(data = hill_df, aes(x = x0,y = hill_y)) + geom_line() +
    geom_line(aes(x=current_spend),color = "red",size = 2) +
    geom_line(aes(x=current_spend*var_optimization_ub),color = 'yellow',size = 2) +
    geom_line(aes(x=current_spend*var_optimization_lb),color = 'yellow',size = 2) +
    dark_theme_gray() +
    xlab('Spend($)') +
    ylab('Hill(x)') +
    ggtitle(sprintf('Hill curve for %s,%s',cleaned_media_name,DMA))+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
         )
  
  #ggsave(sprintf('%s_%s_hill_curve_%s.png',DMA,media_name,model_type))
  
  hill_df$diff <- hill_df$hill_deriv - dplyr::lag(hill_df$hill_deriv,n=1)
  ideal_spend_region_mid <- mean(ideal_spend_region)

  b <- ggplot(data = hill_df, aes(x = x0)) + 
    geom_line(aes(y = hill_deriv,colour = "First derivative")) +
    geom_line(aes(x=current_spend, y = rate_of_slope/3,colour = 'Current spend'),size = 1) +
    geom_line(aes(y = rate_of_slope/3,colour = "Second derivative")) +
    scale_y_continuous(sec.axis = sec_axis(~.*3,name = 'Rate of slope change(Sec derivative)'))+
    geom_point(aes(x = ideal_spend_region[2],y=0))+
    annotate("text",x = ideal_spend_region[2],y = 0,label = 'Inflection point',vjust = -1)+
    #annotate("rect", xmin=ideal_spend_region[1], xmax=ideal_spend_region[2], ymin=-Inf, ymax=max(hill_df$hill_deriv,na.rm = T), alpha=0.2,color = 'green', fill="green")+
    #annotate("text",x = ideal_spend_region_mid,y = (max(hill_df$hill_deriv,na.rm = T)),label = 'Region of maximum return',vjust = -0.5) +
    #geom_line(aes(x=current_spend*var_optimization_lb),color = 'yellow',size = 2) +
    dark_theme_gray() +
    xlab('Spend($)') +
    ylab('Hill deriv(x)') +
    scale_color_manual(values = c('red','green','blue')) + 
    ggtitle(sprintf('Deriv Hill curve for %s,%s',cleaned_media_name,DMA)) +
    theme(
        legend.position="top",
        legend.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()
         )
  
  #ggsave(sprintf('%s_%s_hill_curve_deriv_%s.png',DMA,media_name,model_type))
  
  c <- ggplot(data = hill_df, aes(x = x0,y = y_sec_der)) + geom_line() +
    geom_line(aes(x=current_spend),color = "red",size = 2) +
    geom_line(aes(x=current_spend*var_optimization_ub),color = 'yellow',size = 2) +
    geom_line(aes(x=current_spend*var_optimization_lb),color = 'yellow',size = 2) +
    dark_theme_gray() +
    xlab('Spend($)') +
    ylab('Hill sec deriv(x)') +
    ggtitle(sprintf('Sec.derivative Hill curve for %s,%s',cleaned_media_name,DMA)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10,face = 'bold'),
          axis.text.y = element_text(size = 10,face = 'bold'),
          axis.title.x = element_text(size = 12,face = 'bold'),
          axis.title.y = element_text(size = 12,face = 'bold')
         )
  
  #ggsave(sprintf('%s_%s_hill_curve_sec_deriv_%s.png',DMA,media_name,model_type))
  
  temp_list <- list('Projected_target' = a1,
                      'Hill_curve' = a2,
                      'Hill_deriv_curve' = b,
                      'Hill_df' = hill_df)
  
  #temp_list = setNames(temp_list,DMA)
  dma_plot_list[[DMA]] = temp_list
  
  }
  display_table = data.frame('TypeOfSpend' = c('Breakthrough','Current spend','Inflection point','Minimum Spend','Maximum Spend','Saturation Point'),
                             'Spend' = c(format_money(ideal_spend_region_fprint[1],0),format_money(current_spend_fprint,0),format_money(ideal_spend_region_fprint[2],0),format_money(min_actual_spend_fprint,0),format_money(max_actual_spend_fprint,0),format_money(ideal_spend_region_fprint[3],0)),
                             'Target' = target_vec_fprint)
  
  max_plot_x <- max(ideal_spend_region_fprint,max_actual_spend_fprint,na.rm = T)
  
  ##Projected target - Footprint (Aggregated)
  plt<- ggplot(data = fprint_plot_data_df,aes(x=spend,y=target)) + geom_line() + 
    geom_line(aes(x=current_spend_fprint, color = 'Current spend'),size = 1) +
    geom_line(aes(x=current_spend_fprint*var_optimization_ub,color = 'Optimization region'),size = 1) +
    geom_line(aes(x=current_spend_fprint*var_optimization_lb,color = 'Optimization region'),size = 1)+
    geom_line(aes(x=min_actual_spend_fprint,color = 'Min Max Spend'),size = 1)+
    geom_line(aes(x=max_actual_spend_fprint,color = 'Min Max Spend'),size = 1)+
    #annotate("segment", x = tv_spend_region[2], xend = tv_spend_region[2], y = max(plot_data$target)+25, yend = max(plot_data$target), colour = "green", size=1, alpha=0.6, arrow=arrow()) + 
    geom_line(aes(x = ideal_spend_region_fprint[1],color = 'Breakthrough point'),size = 1)+
    annotate("text",x = ideal_spend_region_fprint[1],y = max(fprint_plot_data_df$target)/2,label = 'Breakthrough point',angle = 90,vjust = -0.5)+
    geom_line(aes(x = ideal_spend_region_fprint[2],color = 'Inflection point'),size = 1) +
    annotate("text",x = ideal_spend_region_fprint[2],y = max(fprint_plot_data_df$target)/2,label = 'Inflection point',angle = 90,vjust = -0.5)+
    geom_line(aes(x = ideal_spend_region_fprint[3],color = 'Saturation point'),size = 1) +
    annotate("text",x = ideal_spend_region_fprint[3],y = max(fprint_plot_data_df$target)/2,label = 'Saturation point',angle = 90,vjust = -0.5)+
    #annotate("rect", xmin=ideal_spend_region[1], xmax=ideal_spend_region[2], ymin=0, ymax=max(plot_data$target), alpha=0.2, fill="green")+
    annotate("rect", xmin=current_spend_fprint*var_optimization_lb, xmax=current_spend_fprint*var_optimization_ub, ymin=0, ymax=max(fprint_plot_data_df$target), alpha=0.2, fill="yellow")+
    #geom_point(data = quarterly_spend,aes(x=x_vec, y = y_vec, color = color_vec),size = 5)+
    ggtitle(sprintf('%s projection for %s,%s',target_name_var,cleaned_media_name,dma_name))+
    dark_theme_gray() +
    scale_color_manual(values =c('hotpink1','yellow4','violetred1','dodgerblue','yellow','red2'))+
    xlab('Spend($)') +
    xlim(0,max_plot_x) + ##Limiting to the max of either saturation or inflection or max spend
    ylab(sprintf('%s',Caps(target_var))) +
    theme(legend.position="top",
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(display_table, rows=NULL, theme=tt)
  
  a1 <- gridExtra::arrangeGrob(plt, tbl,
                     nrow=2,
                     as.table=TRUE,
                     heights=c(3,1))
  
  return_list = list('dma_output_list' = dma_plot_list,
                     'agg_plot' = a1,
                     'spend_points' = ideal_spend_region_fprint)
  return(return_list)
}
