# Replicated spatial pattern function
#'
#' This function will compute replicated patterns from envelope data
#' @param method It should be 'normal' or 'size500' depenging on the method to construct replicated patterns. Need to compute before  dbh_pattern_to_murge. Defaults to "normal"
#' @param pattern_to_murge list with Country_ID and stand pattern
#' @param Status "living" or "dead"
#' @param mark NA, "DBH" or "species"
#' @return list by countrie of replicated spp for each stand
#' @keywords replicated_pattern
#' @export
#' @examples
#' eplicate_patterns(Status="living",mark=NA,method="size500",dbh_pattern_to_murge)
#'

replicate_patterns <- function(Status,method,mark, pattern_to_murge) {
                                        #
    #method == "remove" OR "size500" if we remove 500 size plot or put every plot at 500 size respectively
    library(data.table)
    library(spatstat)
    mainDir <- "/media/tiphaine/73134CEF234D24BB/These/PostDoc/Analyses"#path do the main folder without "/" to the end

    pot500 <- spatstat::disc(radius=12.62,centre=c(0,0),poly=256)
    pot1000 <- spatstat::disc(radius=17.846,centre=c(0,0),poly=256)
    curve_set_pooled_all <- NULL
      #  if(method=="size500") {
      #      pattern_to_murgea <- pattern_to_murge_500
      #      }

    for (i in 1:length(names(pattern_to_murge))) {
       country_name <- names(pattern_to_murge[i]) #by country

       full_name_stand <- gsub("([^_]*)_[^_]*.*", "\\1", names(pattern_to_murge[[i]]))
        unique_stand_name <- unique(full_name_stand)

   #     if(method=="remove") {

            random_labelling_result_country_select <-  pattern_to_murge[[i]]
    #        }

#        if(method=="size500") {
 #           random_labelling_result_country_select <-pattern_to_murge_500[[i]]
 #           }


        data_plot_selected <- data_plot[as.character(data_plot$Site_and_Plot_ID)%in%names(pattern_to_murge[[i]]),] # select the data plot to have the plot size using the names of the selected stands
                                        #   sapply(tapply( data_plot_selected$Plot_Size,as.character(data_plot_selected$Site_ID),unique),length)>1 #compute if there is more than 1 plot size per stands (output TRUE ofr more than 1, else FALSE)
                                        #   tapply( as.factor(data_plot_selected$Plot_Size),as.character(data_plot_selected$Site_ID),summary)[1]#compute the count of stands by plot size 500 and 1000
                                        #
                                        #   tapply( as.factor(data_plot_selected$Plot_Size),as.character(data_plot_selected$Site_ID),summary)%in% sapply(tapply( as.factor(data_plot_selected$Plot_Size),as.character(data_plot_selected$Site_ID),summary),max)#compute the count of stands by plot size 500 and 1000

        table_plot_size_Log <- NULL
        curve_set_pooled_per_country <- NULL





 library(doMC)#only for Linux,used doSNOW for Windows and uncomment the above lines
  #      clusterN <-  detectCores()-1  ### choose number of nodes to add to cluster in doSNOW ; Compute the number of core available from the computer less 1
  #      cl = makeCluster(clusterN, rscript="Rscript.exe", type='SOCK')  ###Cluster for doSNOW
        core <- detectCores()-1#Compute the number of core available from the computer less 1
        if(is.na(core)) {
            core <- 8-1}
        registerDoMC(core)#reg


         master_list <- NULL
         curve_set_pooled_per_country <- foreach(y=1:length( unique_stand_name),.packages=c("spatstat","spptest","marksummary","data.table"),.multicombine=TRUE) %dopar% {


   #     for (y in 1:length( unique_stand_name)) { #by stand



             select_in_rank_result <- which(full_name_stand==unique_stand_name[y])
            rank_result_stand <- random_labelling_result_country_select[select_in_rank_result]
                                        #names(rank_result_stand)
            names_rank_result_selected <- names( rank_result_stand )


            data_tree_selected <- data[as.character(data$Site_and_Plot_ID)%in% names_rank_result_selected,]


              data_tree_selected$distance <- sqrt ( data_tree_selected$x_m^2+  data_tree_selected$y_m^2)





            data_tree_selected_stands_normal <-   data_tree_selected[data_tree_selected$distance<=17.846]#selecte tree of plot size 1000 within a plot size 500 radius of 12.62m from the center

            number_tree_per_for_each_stands_normal<- tapply( data_tree_selected_stands_normal$Global_ID,as.character( data_tree_selected_stands_normal$Site_and_Plot_ID)   ,length)

            number_of_trees_and_plot_size_normal <- rbind( number_tree_per_for_each_stands_normal, data_plot_selected[select_in_rank_result]$plotsize )

            number_of_trees_and_plot_size_normal <- as.data.table(t(number_of_trees_and_plot_size_normal))
            colnames(number_of_trees_and_plot_size_normal) <- c("Number_of_trees","plotsize")
            rownames(number_of_trees_and_plot_size_normal) <-  names( rank_result_stand )
            resume_number_of_tree_per_plot_size_normal <- tapply(number_of_trees_and_plot_size_normal$Number_of_trees,as.character(number_of_trees_and_plot_size_normal$plotsize),sum)#compute the number of trees per plot size for the stand "y"

             if ( length(unique(data_plot_selected$plotsize))>2) {
                 resume_number_of_tree_per_plot_size_500 <- NA
                 } else {

            data_tree_selected_stands_500 <-   data_tree_selected[data_tree_selected$distance<=12.62]#selecte tree of plot size 1000 within a plot size 500 radius of 12.62m from the center
            number_tree_per_for_each_stands_500 <- tapply(data_tree_selected_stands_500$Global_ID,as.character(data_tree_selected_stands_500$Site_and_Plot_ID)   ,length)


                     number_of_trees_and_plot_size_500 <- rbind( number_tree_per_for_each_stands_500, gsub("1000", "1000_resize_500",  data_plot_selected[select_in_rank_result]$plotsize))
            assign("last.warning", NULL, envir = baseenv())#remove warnings() from rbind

            number_of_trees_and_plot_size_500 <- as.data.table(t(number_of_trees_and_plot_size_500))
            colnames(number_of_trees_and_plot_size_500) <- c("Number_of_trees","plotsize")
            rownames(number_of_trees_and_plot_size_500) <-  names( rank_result_stand )
            resume_number_of_tree_per_plot_size_500 <- tapply(as.numeric(number_of_trees_and_plot_size_500$Number_of_trees),as.character(number_of_trees_and_plot_size_500$plotsize),sum)#compute the number of trees per plot size for the stand "y"
            resume_number_of_tree_per_plot_size_500 <- resume_number_of_tree_per_plot_size_500[1]
                 }


            inter <- tapply( factor(data_plot_selected$plotsize,levels= as.character( unique(data_plot$plotsize))),as.character(data_plot_selected$Site_ID),summary)[y]
                                        # names(inter)
                                        # names(inter[[1]])
                                        # as.numeric(inter[[1]])
            data_plot_size_stand <- cbind(country_name,
                                          names(inter),
                                          names(inter[[1]]),
                                          as.numeric(inter[[1]]))
            colnames( data_plot_size_stand ) <- c("Country","Stand_name","plotsize","Number_of_plots")
            data_plot_size_stand <- as.data.table(data_plot_size_stand)

            data_plot_size_stand_normal <- cbind(data_plot_size_stand[ as.character(plotsize) %in% names(resume_number_of_tree_per_plot_size_normal),],Number_of_trees=resume_number_of_tree_per_plot_size_normal)#add the number of trees per plot size for the selected stand 'y' and remove Plot__size with 0 trees
            #compute number of trees for resize 500 plots

            if(dim( data_plot_size_stand_normal )[1] >= 2) { #run the 1000 resize 500 only for stands with more than 1 type size
                plot_1000_resize_500 <-  data_plot_size_stand_normal[plotsize==1000]
                plot_1000_resize_500 <- as.data.table(plot_1000_resize_500)
                plot_1000_resize_500 <- plot_1000_resize_500[plotsize == plot_1000_resize_500[,plotsize],plotsize := names( resume_number_of_tree_per_plot_size_500)]
                plot_1000_resize_500 <- plot_1000_resize_500[Number_of_trees==plot_1000_resize_500[,Number_of_trees],Number_of_trees := resume_number_of_tree_per_plot_size_500[[1]]][]
                                        #end computing resize of 500 plots
                data_plot_size_stand <- rbind( plot_1000_resize_500, data_plot_size_stand_normal )#combine normal and resize information for LOG
            } else {
                data_plot_size_stand <-   data_plot_size_stand_normal
            }

            table_plot_size_Log <- rbind(  table_plot_size_Log, data_plot_size_stand)#use for LOG to know the number of trees per stands per plot size class


            if(sapply(tapply( data_plot_selected$plotsize,as.character(data_plot_selected$Site_ID),unique),length)[[y]]==1) { #if result is TRUE then compute plot size normally otherwise need to choose the method

                r <-  rank_result_stand[[1]]$r
                pooled_sims <- matrix(nrow=nrow(rank_result_stand[[1]]$sim_m), ncol=ncol(rank_result_stand[[1]]$sim_m))
                obs_df <- NULL
                pooled_sim_temp <- NULL
                number_sim <- dim(rank_result_stand[[1]]$sim_m)[2]
                for (z in  names(rank_result_stand)) {

                                        #   if (data_plot[data_plot$Site_and_Plot_ID==z,]$Plot_Size[1]==500) {
                                        #       r <- seq(0,max(pot500$xrange)/2,length.out=513)# CAUTIION HERE. Need a plot with 500m2
                                        #   }
                                        #
                                        #   if(data_plot[data_plot$Site_and_Plot_ID==z,]$Plot_Size[1]==1000) {
                                        #       r <- seq(0,max(pot1000$xrange)/2,length.out=513)# CAUTIION HERE. Need a plot with 1000m2
                                        #   }

                    data_curve_rank <- rank_result_stand[[z]]$obs
                    obs_df <- cbind(obs_df, data_curve_rank)
                                        #  temp_sim_m <-rank_result_stand[[z]]$sim_m
                                        #  colnames( temp_sim_m) <- rep(z,dim(rank_result_stand[[z]]$sim_m)[2])
                                        #  pooled_sim_temp <-  cbind(pooled_sim_temp, temp_sim_m)
                }
                pooled_obs <- apply(obs_df, MARGIN=1, FUN=mean)

                for(w in 1:ncol(pooled_sims)) {
                    table_sim_m <- NULL
                    for (zz in  names(rank_result_stand)) {
                        temp_sim_m <-  rank_result_stand[[zz]]$sim_m[,w]
                        table_sim_m <- cbind( table_sim_m, temp_sim_m)
                    }
                    pooled_sims[,w] <- apply( table_sim_m, MARGIN=1, FUN=mean)
                }
                                        # Collect the above components together to a curve_set object
                                        # (with is_residual=TRUE, we tell that the functions are L_mm(r) - L(r))
                curve_set_pooled <- create_curve_set(list(r = r, obs = pooled_obs, sim_m = pooled_sims, is_residual=TRUE))


            }  else { #end if TRUE with only one plot size for one stands

          #      if(method=="remove") {

                   # stand_names_select <-  data_plot_selected[ data_plot_selected$Site_and_Plot_ID==names(rank_result_stand),]


                    stand_names_select <-  data_plot_selected[ data_plot_selected$Site_and_Plot_ID %in% names(rank_result_stand),]


                    stand_selected_diff_1000 <-  stand_names_select[ stand_names_select$plotsize!=1000]
                    names_stand_selected_diff_1000 <- as.character(  stand_selected_diff_1000[,(Site_and_Plot_ID)])

                    selected_diff_1000_randomlabelling_country <- rank_result_stand[  names( rank_result_stand) != names_stand_selected_diff_1000]

                    r <-   selected_diff_1000_randomlabelling_country[[1]]$r
                    pooled_sims <- matrix(nrow=nrow(selected_diff_1000_randomlabelling_country [[1]]$sim_m), ncol=ncol(selected_diff_1000_randomlabelling_country [[1]]$sim_m))
                    obs_df <- NULL
                    pooled_sim_temp <- NULL
                    number_sim <- dim(selected_diff_1000_randomlabelling_country [[1]]$sim_m)[2]
                    for (z in  names(selected_diff_1000_randomlabelling_country )) {
                        data_curve_rank <- selected_diff_1000_randomlabelling_country [[z]]$obs
                        obs_df <- cbind(obs_df, data_curve_rank)
                                        #  temp_sim_m <-rank_result_stand[[z]]$sim_m
                                        #  colnames( temp_sim_m) <- rep(z,dim(rank_result_stand[[z]]$sim_m)[2])
                                        #  pooled_sim_temp <-  cbind(pooled_sim_temp, temp_sim_m)
                    }
                    pooled_obs <- apply(obs_df, MARGIN=1, FUN=mean)

                    for(w in 1:ncol(pooled_sims)) {
                        table_sim_m <- NULL
                        for (zz in  names(selected_diff_1000_randomlabelling_country)) {
                            temp_sim_m <-  selected_diff_1000_randomlabelling_country[[zz]]$sim_m[,w]
                            table_sim_m <- cbind( table_sim_m, temp_sim_m)
                        }
                        pooled_sims[,w] <- apply( table_sim_m, MARGIN=1, FUN=mean)
                    }
                                        # Collect the above components together to a curve_set object
                                        # (with is_residual=TRUE, we tell that the functions are L_mm(r) - L(r))
                    curve_set_pooled <- create_curve_set(list(r = r, obs = pooled_obs, sim_m = pooled_sims, is_residual=TRUE))
  #              }#stop  IF with REMOVE method

        }#stop ELSE when there is 500 and 1000 plots size
            master_list <-  list(curve_set_pooled,  data.table(table_plot_size_Log))
             return(  master_list)
         }# end of y    #end loop by stand END METHOD

        # for (t in 1:length( unique_stand_name)) {


      #  curve_set_pooled_per_country



       # write.table(  table_plot_size_Log, paste0("Replicated Logs",".txt"),sep="\t",row.names = FALSE) #write a file with the plot name where there is point out of the window

        names(curve_set_pooled_per_country) <-  unique_stand_name

        curve_set_pooled_all <-  append(curve_set_pooled_all,list(lapply(curve_set_pooled_per_country, `[[`, 1))) #Select all first object in the list which are the pooled function


        #Print log table per country


       if (file.exists(file.path(mainDir,Status,mark,method,"replicate", country_name ))){
                setwd(file.path(mainDir,Status,mark,method,"replicate", country_name ))
            } else {
                dir.create(file.path(mainDir,Status,mark,method,"replicate", country_name ), showWarnings = FALSE,recursive=TRUE)
                setwd(file.path(mainDir,Status,mark,method,"replicate", country_name ))
            }


       table_plot_size_Log <- do.call(rbind.data.frame, lapply(curve_set_pooled_per_country, `[[`, 2)) #select all second object of the list which are the Log file per country

        write.table( table_plot_size_Log, paste0("Decision Logs ",method, " ", country_name,".txt"),sep="\t",row.names = FALSE)
       setwd(mainDir)

    } #end  loop country

    names(curve_set_pooled_all) <- names(pattern_to_murge)

    return(curve_set_pooled_all)

}#end function



################################################
################################################
################################################
################################################
################################################
################################################
################################################


# Replicated spp to rank test
#'
#' This function will compute the rank test from replicated patterns
#' @param replicate_patterns the output of the replicate_patterns function
#' @param mark description of the mark 'DBH', or NA is none.
#' @param graph TRUE or FALSE. Defaults to FALSE
#' @param Status "living" or "dead"
#' @param method "size500" or "normal"
#' @keywords replicate_patterns
#' @export
#' @examples
#' rank_test_replicated_dbh_500 <- replicate_patterns_to_rank_test(replicate_patterns_dbh_size500,"dbh",graph=TRUE)



replicate_patterns_to_rank_test <- function (Status,replicate_patterns, method, mark,graph) {
    library(spptest)
    rank_env_dbh_replicated_all <- NULL
    for (i in names(replicate_patterns)){
        rank_env_dbh_replicated_per_stand <- NULL
        stands_name <- names(replicate_patterns[[i]]) #

        for ( y in  stands_name ) {

            res <- rank_envelope(replicate_patterns[[i]][[y]],lexo = FALSE,savedevs = TRUE)
            rank_env_dbh_replicated_per_stand <- append(rank_env_dbh_replicated_per_stand,list(res))
        }
        names(rank_env_dbh_replicated_per_stand) <-  stands_name
        rank_env_dbh_replicated_all <- append(rank_env_dbh_replicated_all,list(rank_env_dbh_replicated_per_stand))
    }
    names(rank_env_dbh_replicated_all) <- names(replicate_patterns) #replace names in the list with sample plot ID within a country


    if(graph == TRUE) {

        mainDir <- "/media/tiphaine/73134CEF234D24BB/These/PostDoc/Analyses"#path do the main folder without "/" to the end

        setwd(mainDir)
        subsubDir2 <- "Lmm graph "#output folder
        for (y in names(rank_env_dbh_replicated_all)) {


            if (file.exists(file.path(mainDir,Status,mark,method,"replicate",subsubDir2,i))){#here i is the country name like "GER"
                setwd(file.path(mainDir,Status,mark,method,"replicate",subsubDir2,i))
            } else {
                dir.create(file.path(mainDir,Status,mark,method,"replicate",subsubDir2,i), showWarnings = FALSE,recursive=TRUE)
                setwd(file.path(mainDir,Status,mark,method,"replicate",subsubDir2,i))
            }


       for( z in names(rank_env_dbh_replicated_all[[y]]) ) {



                  #  pdf(paste0("Lmm replicated ",mark," ",method," ",y," #",z,".pdf"))
                plot(rank_env_dbh_replicated_all[[y]][[z]],use_ggplot2=TRUE,ylab=expression(italic(L[mm](r)-L(r)))) +
                    labs(list(title = paste("Plot #",z,sep=" "))) + #put the number of the plot as titlex
                    ggsave(paste0("Lmm replicated ",mark," ",method," ",y," #",z,".pdf"))

              #  dev.off())
            }
        }
        setwd(mainDir)

    }

    return (rank_env_dbh_replicated_all)

}



################################################
################################################
################################################
################################################
################################################
################################################
################################################


# Extraction of Lmm from rank test function for dbh variable
#'
#' This function will convert the rank test in a table with p.value, pattern of the spp (random, aggregate or repuslion)
#' @param rank_result the output of the rank result for dbh
#' @keywords Lmm
#' @export
#' @examples
#' extraction_Lmm_from_rank (rank_test_replicated_500)


extraction_Lmm_from_rank <- function (rank_result ) {
country_names <- names(rank_result)

    Lmm_rank_result <- NULL
data_rank <- NULL
agg_rep <- data.frame(c("agreagation","repulsion"))#names used to know if the ppp is agregation, repulsion, both  or random
agg_rep_star<- data.frame(c("agreagation*","repulsion*"))#names used to know if the ppp is agregation, repulsion, both  or random
for (i in 1:length(names(rank_result))) {
    country_name <-names(rank_result[i])
    for (y in names(rank_result[[i]])) {
        rank_p <-rank_result[[i]][[y]]$p #p.value of the rank envelope test
        rank_p_interval_low <-  rank_result[[i]][[y]]$p_interval[1] #p value interval
        rank_p_interval_up <-  rank_result[[i]][[y]]$p_interval[2] #p value interval

        rank_method <- rank_result[[i]][[y]]$method #method use
        Site_and_Plot_ID <- y #number of the plot in a country
        significance_1_signi <- ifelse( rank_result[[i]][[y]]$p<0.05,1,0)
        if(significance_1_signi == 1) {
            pattern_state <- if(length( agg_rep_star[c(length(unique(rank_result[[i]][[y]]$data_curve<=rank_result[[i]][[y]]$upper))==2,
                                                       length(unique(rank_result[[i]][[y]]$data_curve>=rank_result[[i]][[y]]$lower))==2),])==0) {
                                 "random*"} else {
                                              if (length (as.character( agg_rep_star[c(length(unique(rank_result[[i]][[y]]$data_curve<=rank_result[[i]][[y]]$upper))==2,
                                                                                       length(unique(rank_result[[i]][[y]]$data_curve>=rank_result[[i]][[y]]$lower))==2),]))>1) {"agr+rep*"} else {

                                                                                                                                                                                                       as.character( agg_rep_star[c(length(unique(rank_result[[i]][[y]]$data_curve<=rank_result[[i]][[y]]$upper))==2,
                                                                                                                                                                                                                                    length(unique(rank_result[[i]][[y]]$data_curve>=rank_result[[i]][[y]]$lower))==2),])
                                                                                                                                                                                                   }
                                          }

        } else {              pattern_state <- if(length( agg_rep[c(length(unique(rank_result[[i]][[y]]$data_curve<=rank_result[[i]][[y]]$upper))==2,
                                                                    length(unique(rank_result[[i]][[y]]$data_curve>=rank_result[[i]][[y]]$lower))==2),])==0) {
                                                   "random"} else {
                                                               if (length (as.character( agg_rep[c(length(unique(rank_result[[i]][[y]]$data_curve<=rank_result[[i]][[y]]$upper))==2,
                                                                                                   length(unique(rank_result[[i]][[y]]$data_curve>=rank_result[[i]][[y]]$lower))==2),]))>1) {"agr+rep"} else {

                                                                                                                                                                                                                  as.character( agg_rep[c(length(unique(rank_result[[i]][[y]]$data_curve<=rank_result[[i]][[y]]$upper))==2,
                                                                                                                                                                                                                                          length(unique(rank_result[[i]][[y]]$data_curve>=rank_result[[i]][[y]]$lower))==2),])
                                                                                                                                                                                                              }
                                                           }
        }

        data_rank<- data.table( country_name,Site_and_Plot_ID,rank_method,rank_p_interval_low,rank_p_interval_up,rank_p,significance_1_signi,pattern_state=as.factor(pattern_state))

        Lmm_rank_result <-rbind( Lmm_rank_result,data_rank)
    }
}

return(Lmm_rank_result) ### table

}#end function








