
# R6 object


#' Recm  Robust ensemble classifier machine (Recm)
#' 
#' An object that holds data and an ensemble of classifiers
#' The ensemble is composed of XGBoost classifiers trained on binarized labels.
#'
#' 
#' @export
Recm <- R6Class("Recm",
                  public = list(
                    
                    #' @field name the object's name
                    name = NULL,
                    
                    #' @field data_mode string vector, dictates the types of data engineeering, acceptable values include combination of: original  quartiles  pairs  ranks  sigpairs
                    data_mode = NULL,
                    
                    #' @field signatures lists of variables, must be in data, that will be used together, and compared to other signatures
                    signatures = NULL,
                    
                    #' @field file_name the data file
                    file_name = NULL,
                    
                    #' @field data the data.table used to train and test
                    data = NULL,
                    
                    #' @field the data column used as label, the target
                    label = NULL,
                    
                    #' @sample_id the data column used as label, the target
                    sample_id = NULL,
                    
                    #' @sample_ids the data column used as label, the target
                    sample_ids = NULL,
                    
                    #' @train_sample_ids the sample IDs used in training data
                    train_sample_ids = NULL,

                    #' @test_sample_ids the sample IDs used in test data
                    test_sample_ids = NULL,
                    
                    #' @field cv_rounds the number of cross-validation rounds, values greater than 1 overrides data_split.
                    cv_rounds=1,
                    
                    #' @field data_split numeric value indicating percent data to make into training data
                    data_split = NULL,
                    
                    #' @field train_data the data.table used to train the ensemble
                    train_data = NULL,
                    
                    #' @field train_label the vector used as the training label
                    train_label = NULL,
                    
                    #'@field test_data the data.table used as test data
                    test_data = NULL,
                    
                    #' @field test_label the vector used as test data labels
                    test_label = NULL,

                    #' @field data_colnames the column names used to train the models 
                    data_colnames = NULL,
                    
                    #' @field  unique_labels the unique set of labels
                    unique_labels = NULL,
                    
                    #' @field the ensemble of predictors
                    ensbl = list(),
                    
                    #' @field the table of predictions, collecting from the ensbl list of predictors
                    call_table = NULL,
                    
                    #' @field the table of predictions, collecting from the ensbl list of predictors
                    pred_table = NULL,
                    
                    #' @field cv_results the table of results over all samples.
                    cv_results = NULL,
                    
                    #' @field cv_importance the list of importance from each fold.
                    cv_importance = NULL,
                    
                    #' @description Create a new `Recm` object.
                    #' @param name The object is named.
                    #' @return A new `recm` object.
                    initialize = function(name = NA) {
                      self$name <- name
                      #self$greet()
                    },
                    
                    
                    #' @description
                    #' Creates a printable representation of the object.
                    #' @return A character string representing the object.
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    },
                    
                    
                    #' @description Reads the data file.
                    #' @param file_name The name of the file.
                    #' @param sep The separting character ',' or '\t'
                    read_data = function(file_name, sep, header) {
                      self$file_name <- file_name 
                      self$data <- data.table::fread(file=file_name, sep=sep, header=T)
                      colnames(self$data) <- gsub(" ", "_", colnames(self$data))
                      return(invisible(self))
                    },
                  
                    
                    #' @description Reads the data file.
                    #' @param file_name The name of the file.
                    #' @param sep The separting character ',' or '\t'
                    read_train_data = function(file_name, sep, header) {
                      self$file_name <- file_name
                      self$train_data <- data.table::fread(file=file_name, sep=sep, header=T)
                      colnames(self$train_data) <- gsub(" ", "_", colnames(self$train_data))
                      return(invisible(self))
                    },
                    
                    
                    #' @description Reads the data file.
                    #' @param file_name The name of the file.
                    #' @param sep The separting character ',' or '\t'
                    #' @param header boolean, whether the table has a header line
                    read_test_data = function(file_name, sep, header) {
                      self$file_name <- file_name
                      self$test_data <- data.table::fread(file=file_name, sep=sep, header=T)
                      colnames(self$test_data) <- gsub(" ", "_", colnames(self$test_data))
                      return(invisible(self))
                    },
                    
                    
                    # data engineering
                    #' @description Data engineering, replaces the object's data.table.
                    data_eng = function(data_source=NULL) {
                      
                      # create a new data engineering object
                      this_deng <- Deng$new(self$data_mode, self$signatures)
                                          
                      if (data_source == 'train') {
                        self$train_data <- this_deng$data_eng(self$train_data)
                      } else if (data_source == 'test') {
                        self$test_data <- this_deng$data_eng(self$test_data)
                      } else if (data_source == 'data') {
                        self$data <- this_deng$data_eng(self$data)
                      } else {
                        stop('ERROR: data source must be train, test, or data.')
                      }
                    },
                    
                    
                    #' @description Splits the data into training and test data.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    #' @param data_split numeric value, the percent of data to use in training 
                    data_split_fun = function(data_split, cv_rounds, i) {
                      
                      if (cv_rounds == 1) {
                        # to split the data into training and test components
                        n <- nrow(self$data)
                        idx <- sample.int(n = n, size=data_split*n)
                        jdx <- setdiff( (1:n), idx)
                        # then create the data sets
                        self$train_data <- self$data[idx,]
                        self$train_label <- self$label[idx]
                        self$train_sample_ids <- self$sample_ids[idx]
                        # 
                        self$test_data <- self$data[jdx,]
                        self$test_label <- self$label[jdx]
                        self$test_sample_ids <- self$sample_ids[jdx]
                        # and record the unique categories in labels 
                        self$unique_labels <- unique(self$train_label)
                        
                        # or ELSE we're doing cross-validation
                      } else {
                        #Create 10 equally size folds
                        folds <- cut(seq(1,nrow(self$data)),breaks=cv_rounds,labels=FALSE)
                        
                        #Segement your data by fold using the which() function 
                        testIndexes <- which(folds==i,arr.ind=TRUE)
                        self$train_data <- self$data[-testIndexes, ]
                        self$train_label <- self$label[-testIndexes]
                        self$train_sample_ids <- self$sample_ids[-testIndexes]
                        
                        self$test_data  <- self$data[testIndexes, ]
                        self$test_label <- self$label[testIndexes]
                        self$test_sample_ids <- self$sample_ids[testIndexes]
                        
                        self$unique_labels <- unique(self$train_label)
                      
                      }
                      
                      return()
                    },
                    

                    #' @description Does some setup processing on the data file, drop columns, split data into train and test, and identify the label column.
                    #' @param file_name string, the name of the file
                    #' @param sep string, the separating character
                    #' @param data_mode string, 
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    #' @param data_split numeric value, the percent of data to use in training 
                    data_setup = function(file_name=NULL, 
                                          sep=NULL, 
                                          data_mode=NULL, 
                                          signatures=NULL, 
                                          label_name=NULL, 
                                          sample_id=NULL,
                                          drop_list=NULL, 
                                          data_split=NULL){
                      # First reading it in
                      self$file_name <- file_name
                      self$data_mode <- data_mode
                      self$signatures <- signatures
                      
                      if (is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                        sep = ','
                      }
                      else if (is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                        sep = '\t'
                      } else if (is.null(sep)) {
                        stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                      }
                      
                      thisdata <- data.table::fread(file=file_name, sep=sep, header=T)
                      self$data <- thisdata[sample(nrow(thisdata)),]
                      self$data_colnames <- colnames(self$data)
                      colnames(self$data) <- gsub(" ", "_", colnames(self$data))
                      
                      if (is.null(label_name)) {
                        stop("Make sure label_name is not null!")
                      } else {
                        self$label <- sapply(self$data[[label_name]], as.character)
                        label_name <- gsub(' ', '_', label_name)
                      }
                      
                      if ((!is.null(sample_id)) && sample_id %in% self$data_colnames) {
                        self$sample_id <- gsub(' ', '_', sample_id)
                        self$sample_ids <- sapply(self$data[[self$sample_id]], as.character)
                        set(self$data, j = self$sample_id, value = NULL)  # then del it
                      } else {
                        self$sample_ids <- 1:nrow(self$data)
                      }
                      
                      if (!is.null(drop_list)) {
                        drop_list <- gsub(' ', '_', drop_list)
                      }
                      
                      if (!label_name %in% colnames(self$data)) {
                        stop('Make sure the label name matches one of the columns!')
                      } else {
                        set(self$data, j = label_name, value = NULL)
                      }

                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in% colnames(self$data)))) {
                        set(self$data, j = drop_list, value = NULL)
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }
                      
                      # DATA ENGINEERING
                      self$data_eng('data')
                      
                      return(invisible(self))
                    },
                    
                    
                    #' @description Does some setup processing on the training data file, drop columns and identify the label column.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    train_data_setup = function(file_name=NULL, 
                                                sep=NULL, 
                                                data_mode=NULL, 
                                                signatures=NULL, 
                                                label_name=NULL, 
                                                sample_id=NULL, 
                                                drop_list=NULL){
                      #READING DATA
                      self$file_name <- file_name
                      self$data_mode <- data_mode
                      self$signatures <- signatures
                      
                      if (is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                        sep = ','
                      }
                      else if (is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                        sep = '\t'
                      } else if (is.null(sep)) {
                        stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                      }
                      
                      self$train_data <- data.table::fread(file=file_name, sep=sep, header=T)
                      # grab the original data column names
                      self$data_colnames <- colnames(self$train_data)
                      # fix any spaces in the column names
                      colnames(self$train_data) <- gsub(" ", "_", colnames(self$train_data))
                      
                      if (is.null(label_name)) {
                        stop("Make sure label_name is not null!")
                      } else {
                        if (!label_name %in% colnames(self$train_data)) {
                          stop('Make sure the label name matches one of the columns!')
                        } else {
                          label_name <- gsub(' ', '_', label_name)
                          self$train_label <- sapply(self$train_data[[label_name]], as.character)
                          set(self$train_data, j = label_name, value = NULL)
                        }
                      }
                      
                      if ((!is.null(sample_id)) && sample_id %in% self$data_colnames) {
                        self$sample_id <- gsub(' ', '_', sample_id)
                        self$train_sample_ids <- sapply(self$train_data[[self$sample_id]], as.character)
                        set(self$train_data, j = self$sample_id, value = NULL)  # then del it
                      } else {
                        self$train_sample_ids <- 1:nrow(self$train_data)
                      }
                      
                      
                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in% self$data_colnames))) {
                        drop_list <- gsub(' ', '_', drop_list)
                        set(self$train_data, j = drop_list, value = NULL)
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }
                      
                      # DATA ENGINEERING
                      self$data_eng('train')
                      # and record the unique categories in labels 
                      self$unique_labels <- unique(self$train_label)
                      return(invisible(self))
                    },
                    
                    
                    #' @description Does some setup processing on the test data file, drop columns and identify the label column.
                    #' The data_mode and signatures will have already been set in training.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    test_data_setup = function(file_name=NULL, 
                                               sep=NULL, 
                                               label_name=NULL, 
                                               sample_id=NULL, 
                                               drop_list=NULL){
                      #READ DATA
                      self$file_name <- file_name
                      if (is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                        sep = ','
                      }
                      else if (is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                        sep = '\t'
                      } else if (is.null(sep)) {
                        stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                      }
                      
                      self$test_data <- data.table::fread(file=file_name, sep=sep, header=T)

                      # check that the data column names are the same as used in training
                      if (!all(colnames(self$test_data) %in% self$data_colnames)) {
                        stop('Test data column names must match what was used in training.')
                      } else {
                        colnames(self$test_data) <- gsub(" ", "_", colnames(self$test_data))
                      }
                      
                      if (is.null(label_name)) {
                        self$test_label <- NULL  # don't have to have labels to make calls.
                      } else {
                        if (!label_name %in% colnames(self$test_data)) {
                          stop('Make sure the label name matches one of the columns!')
                        } else {
                          label_name <- gsub(' ', '_', label_name)
                          self$test_label <- sapply(self$test_data[[label_name]], as.character)
                          set(self$test_data, j = label_name, value = NULL)
                        }
                      }
                      
                      
                      if ((!is.null(sample_id))) { # already know that the col names match
                        test_sample_id <- gsub(' ', '_', sample_id)
                        self$test_sample_ids <- sapply(self$test_data[[test_sample_id]], as.character)
                        set(self$test_data, j = test_sample_id, value = NULL)  # then del it
                      } else {
                        self$test_sample_ids <- 1:nrow(self$test_data)
                      }
                      
                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in% self$data_colnames))) {
                        drop_list <- gsub(' ', '_', drop_list)
                        set(self$test_data, j = drop_list, value = NULL)
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }

                      # DATA ENGINEERING
                      self$data_eng('test')

                      return(invisible(self))
                    },

                    
                    # takes the label, and returns a vector of 0s and 1s
                    binarize_label = function(label, x) {
                      if (is.numeric(x)) {
                        x <- as.character(x)
                      }
                      new_label <- ifelse(label == x, yes=1, no=0)
                      return(new_label)
                    },
                    
                    
                    #' @description 
                    #' Builds list of ensembles of XGBoost object, each classifying one binary label.
                    #' @param mode character vector, what types of data modalities to make. possible: pairs, quartiles, set-pairs
                    #' @param size numeric, number of classifiers
                    #' @param label string, the label vector of each data example
                    #' @param max_depth numeric, the depth of the tree in XGBoost
                    #' @param eta numeric, the eta param of XGBoost, speed of learning
                    #' @param nrounds numeric, the number of training rounds
                    #' @param nthreads numeric, the number of threads to use in processing
                    #' @param objective string, binary:logistic, see xgboost docs
                    #'
                    #' @details A list of classifiers, each trained on a random sample of the training data.
                    #'
                    #' @return A ensemble object is added to the list of objects in recm$enbl.
                    #'
                    #'
                    build_label_ensemble = function(size,
                                                    params) {
                      
                      # for each category
                      for (li in self$unique_labels) {
                        
                        # first create the binarized label
                        bin_label <- self$binarize_label(label=self$train_label, x=li)
                        # then create the classifier object

                        self$ensbl[[li]] <- Ensbl$new(name=li, 
                                                      obj_mode='ensemble',
                                                      size=size, 
                                                      data=self$train_data,
                                                      label=bin_label, 
                                                      params=params)
                        
                      }
                      return(invisible(self))
                    },
                    
                    
                    
                    build_pred_table = function() {
                      final_train_data <- list()
                      for (li in self$unique_labels) {
                        final_train_data[[li]] <- self$ensbl[[li]]$pred_combined
                      }
                      
                      self$pred_table <- do.call(cbind.data.frame, final_train_data)
                      
                      return(invisible(self))
                    },
                    
                    
                    
                    remap_multiclass_labels = function(label) {
                      mapper <- list() # first we construct a mapper function
                      idx <- 0
                      for (li in self$unique_labels) { 
                        mapper[[li]] <- idx
                        idx <- idx+1
                      }
                      new_labels <- sapply(label, function(a) mapper[[a]])
                      return(new_labels)
                    },
                    
                    
                    unmap_multiclass_labels = function(labels) {
                      new_labels <- c()
                      for (li in labels) { 
                        new_labels <- c(new_labels, self$unique_labels[(li+1)])
                      }
                      return(new_labels)
                    },
                    
                    
                    
                    build_final_ensemble = function(
                                                size, 
                                                params) {
                      # here the ensemble will be trained on the prior predictions
                      # for each category, get the predictions
                      # turn that into a dataframe / matrix
                      self$build_pred_table()
                      remapped_label <- self$remap_multiclass_labels(self$train_label)
                      #print(head(self$train_label))
                      #print(head(remapped_label))
                      # train a XGBoost that takes multiple labels.

                      self$ensbl[["final"]] <- Ensbl$new(name="final",
                                                         obj_mode="final",
                                                         size=size, 
                                                         data=self$pred_table,
                                                         label=remapped_label, 
                                                         params=params
                                                    )
                      return(invisible(self))
                    },
                    
                    
                    
                    train_models = function(perc) {
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$train_models(perc)
                      }
                      return(invisible(self))
                    },
                    
                    
                    
                    train_final = function(perc) {
                      self$ensbl[['final']]$train_models(perc)
                      return(invisible(self))
                    },
                    
                    
                    
                    ensemble_predict = function(data, combine_function) {
                      if (all(class(data)[1] == 'matrix') == FALSE) {
                        data <- as.matrix(data)
                      }
                      for (li in self$unique_labels) {
                        (self$ensbl[[li]])$member_predict(data, combine_function)
                      }
                      return(invisible(self))
                    },
                    
                    
                    
                    # predict final uses predictions from predict_ensemble
                    predict = function(data, combine_function) {
                      self$ensemble_predict(data, combine_function)
                      self$build_pred_table()

                      # then we should have a new pred_table from the data
                      pred_matrix <- as.matrix(self$pred_table)
                      
                      self$ensbl[['final']]$member_predict(pred_matrix, combine_function)
                      return(invisible(self))
                    },
                    
                    
                    print_error = function(label, root, threshold) {
                      if (all(class(label) == 'numeric') == FALSE) {
                        label <- as.numeric(label)
                      }
                      new_label <- self$binarize_label(label, root)
                      self$ensbl[[root]]$print_error(new_label, threshold)
                      return(invisible(self))
                    },
                    
                    
                    # total percent correct from all calls
                    accuracy = function(labels, calls) {
                      m <- sum(labels == calls)
                      return(m/length(labels))
                    },
                    
                    # precision
                    # what proportion of predicted positives are truly positive
                    precision = function(cmdf, i) {
                      called_pos <- cmdf %>% dplyr::filter(calls==i) %>% pull('Freq')
                      true_pos <- cmdf %>% dplyr::filter(labels==i & calls == i) %>% pull('Freq')
                      return(true_pos / sum(called_pos))
                    },
                    
                    # sensitivity
                    # what proportion of true positives are called positive
                    sensitivity = function(cmdf, i) {
                      true_labels <- cmdf %>% dplyr::filter(labels==i) %>% pull('Freq')
                      true_pos <- cmdf %>% dplyr::filter(labels==i & calls == i) %>% pull('Freq')
                      return(true_pos / sum(true_labels))
                    },
                    
                    # specificity
                    # what proportion of true negatives are called negative
                    specificity = function(cmdf, i) {
                      false_labels <- cmdf %>% dplyr::filter(labels!=i) %>% pull('Freq')
                      true_neg <- cmdf %>% dplyr::filter(labels!=i & calls != i) %>% pull('Freq')
                      return(sum(true_neg) / sum(false_labels))
                    },
                    
                    
                    classification_metrics = function(use_cv_results=TRUE) {
                      
                      if (use_cv_results) {
                        calls <- self$cv_results$BestCalls
                        labels <- self$cv_results$Label
                        
                      } else {
                        if (is.null(self$test_label)) {
                          return("No test labels.")
                        } else {
                          # first make sure our labels are mapped to integers correctly
                          labels <- self$test_label
                          
                          # get the calls
                          mapped_calls <- self$ensbl[['final']]$pred_combined
                          calls <- self$unmap_multiclass_labels(mapped_calls)
                        }
                      }
                        
                        # then build the multi-class confusion matrix
                        confusion_matrix <- table( labels, calls )
                        
                        # labels of the confusion matrix
                        cm_labels <- rownames(confusion_matrix)
                        
                        # and we'll transform that into a data.frame
                        cmdf <- as.data.frame(confusion_matrix, stringsAsFactors = F)
                        
                        # accuracy
                        acc <- self$accuracy(labels, calls)
                        
                        # first compute precision
                        prec <- sapply(cm_labels, function(a) self$precision(cmdf, a))
                        
                        # then specificity
                        spec <- sapply(cm_labels, function(a) self$specificity(cmdf, a))
                        
                        # then sensitivity or recall
                        sens <- sapply(cm_labels, function(a) self$sensitivity(cmdf, a))
                        
                        # then F1
                        f1 <-(2*sens*prec) / (sens+prec)
                        
                        metrics <- data.frame(Label=cm_labels,
                                              Accuracy=acc,
                                              Sensitivity=sens,
                                              Specificity=spec,
                                              Precision=prec,
                                              F1=f1,
                                              stringsAsFactors = F)
                        
                        avg_row <- data.frame(Label="Average",
                                              Accuracy=mean(metrics[,2]),
                                              Sensitivity=mean(metrics[,3]),
                                              Specificity=mean(metrics[,4]),
                                              Precision=mean(metrics[,5]),
                                              F1=mean(metrics[,6]), 
                                              row.names = 'Average',
                                              stringsAsFactors = F)
                        
                        metrics <- rbind(metrics, avg_row)
                        
                        return(metrics)
                      
                    },
                    
                    
                    importance = function() {
                      # for each ensembl 
                      resList <- list()
                      for (i in 1:length(self$ensbl)) {
                        impList <- list()
                        ei <- self$ensbl[[i]]
                        # for each booster in the ensemble member
                        for (j in 1:ei$size) {
                          impList <- rbind(impList, xgb.importance(model=ei$bstl[[j]]))
                        }
                        # then we have a set of tables, and they might or 
                        # might not have the same feature names. Assume they
                        # do not. Also don't have the same sizes.
                        resList[[ei$name]] <- impList %>% 
                          group_by(Feature) %>% 
                          summarise(MedGain=median(Gain),
                                    MedCover=median(Cover),
                                    MedFreq=median(Frequency)) %>%
                          arrange(desc(MedGain))
                      }
                      # now one item per ensemble
                      return(resList)
                    }, 
                    
                    
                    results = function(include_label=FALSE) {
                      
                      # get the calls
                      mapped_calls <- self$ensbl[['final']]$pred_combined
                      # map calls to the feature names
                      calls <- self$unmap_multiclass_labels(mapped_calls)
                      # build a data frame with calls and sample ids
                      df <- data.frame(SampleIDs = self$test_sample_ids, BestCalls=calls)
                      # 
                      pred_sums <- apply(self$pred_table, 1, sum)
                      norm_pred_table <- t(sapply(1:nrow(self$pred_table), function(i) self$pred_table[i,] / pred_sums[i]))
                      df <- cbind(df, norm_pred_table)
                      
                      if (!is.null(self$test_label) && include_label == TRUE) {
                        df <- cbind(df, data.frame(Label=self$test_label))
                        
                      }
                      return(df)
                    },
                    
                    
                    # Run CV or specify a split
                    autopred = function(data_file=NULL,
                                        sep=NULL,
                                        label_name=NULL,
                                        sample_id=NULL,
                                        drop_list=NULL,
                                        cv_rounds=1,
                                        data_split=NULL,
                                        data_mode=NULL,
                                        signatures=NULL,
                                        size=NULL,
                                        params=NULL,
                                        train_perc=NULL,
                                        combine_function=NULL
                                      ) {
                      
                      params[['objective']] <- "binary:logistic"
                      params[['eval_metric']] <-'logloss'
                      final_params <- params
                      final_params[['objective']] <- 'multi:softmax'
                      final_params[['eval_metric']] <- 'mlogloss'
                      
                      
                      # perform the data set up
                      self$data_setup(file_name=data_file,
                                     sep=sep,
                                     data_mode=data_mode,
                                     signatures=signatures,
                                     label_name=label_name, 
                                     sample_id=sample_id,
                                     drop_list=drop_list)
                      
                      
                      for (cvi in 1:cv_rounds){
                        
                        print(paste0("*** Training-Testing Round ", cvi, " ***"))
                        
                        # SPLIT DATA round 1
                        self$data_split_fun(data_split, cv_rounds, cvi)
                        
                        # build the initial set of predictors
                        self$build_label_ensemble(size=size, 
                                                  params=params)
                        
                        # and train them using a random selection of data
                        self$train_models(train_perc)
                        
                        # then make a prediction on the training data
                        self$ensemble_predict(self$train_data, 
                                              combine_function = combine_function)
                        
                        # build the output predictor
                        self$build_final_ensemble(size=size, 
                                                  final_params)
                        
                        # and use the earlier training predictions to train the output                      
                        self$train_final(train_perc)
                        
                        # and finally, make a prediction on some training data.
                        self$predict(self$test_data, combine_function)
                        
                        # capture the feature importance from each fold
                        self$cv_importance[[cvi]] <- self$importance()
                        
                        # append the final results 
                        if (cvi > 1) {
                          self$cv_results = rbind(self$cv_results, self$results(include_label = T))
                        } else {
                          self$cv_results = self$results(include_label = T)
                        }
                        
                      } # done with CV
                    
                    }# end autopred
                  ) # end public
      )


