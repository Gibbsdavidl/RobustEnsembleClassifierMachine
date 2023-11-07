
# R6 object


#' Recm  Robust ensemble classifier machine (Recm)
#' 
#' An object that holds data and an ensemble of classifiers
#' The ensemble is composed of XGBoost classifiers trained on binarized labels.
#'
#' 
#' @export
Robencla <- R6Class("Robencla",
                  public = list(
                    
                    #' @field name the object's name
                    name = NULL,
                    
                    #' @field data_mode string vector, dictates the types of data engineeering, acceptable values include combination of: original  quartiles  pairs  ranks  sigpairs
                    data_mode = NULL,

                    #' @field op_mode string describing the operation mode 'data', 'train', 'test'
                    op_mode = NULL,
                    
                    #' @field signatures lists of variables, must be in data, that will be used together, and compared to other signatures
                    signatures = NULL,
                    
                    #' @field pair_list a list of column names in the data, that will become paired data, named list or vector
                    pair_list = NULL,
                    
                    #' @field file_name the data file
                    file_name = NULL,
                    
                    #' @field data the data.table used to train and test
                    data = NULL,
                    
                    #' @field the data column used as label, the target
                    label = NULL,
                    
                    #' @field the column name containing the label in the training data
                    label_name = NULL,
                    
                    #' @sample_id the data column used to identify samples
                    sample_id = NULL,
                    
                    #' @sample_ids to save the sample_ids
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
                    
                    #' @field combine_function function for combining across ensemble
                    combine_function = NULL,
                    
                    #' @field verbose turn off warnings
                    verbose = NULL,
                    
                    #' @description Create a new `Recm` object.
                    #' @param name The object is named.
                    #' @return A new `recm` object.
                    initialize = function(name = NA) {
                      self$name <- name
                      #self$greet()
                    },
                    
                    #' @description
                    #' Returns the robencla version.
                    #' @return A character string representing the package version.
                    version = function() {
                      return("0.3.3")
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

                    
                    
                    #' @description Splits the data into training and test data.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    #' @param data_split numeric value, the percent of data to use in training 
                    data_split_fun = function(data_split, cv_rounds=1, i=1) {
                      
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
                        #Create N equally size folds
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
                    #' @param data_frame data.frame, data.frame or data.table, if NULL, then use file_name.
                    #' @param file_name string, the name of the file
                    #' @param sep string, the separating character
                    #' @param data_mode string, 
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    #' @param data_split numeric value, the percent of data to use in training 
                    data_setup = function(data_frame=NULL,
                                          file_name=NULL, 
                                          sep=NULL, 
                                          data_mode=NULL, 
                                          signatures=NULL, 
                                          pair_list=NULL,
                                          label_name=NULL, 
                                          sample_id=NULL,
                                          drop_list=NULL, 
                                          data_split=NULL,
                                          verbose=NULL){
                      # First reading it in
                      self$file_name  <- file_name
                      self$data_mode  <- data_mode
                      self$signatures <- signatures
                      self$pair_list  <- pair_list
                      self$data_split <- data_split
                      self$label_name <- label_name
                      self$verbose <- verbose
                      
                      # bug file_name checked although null
                      if (!is.null(file_name)) {
                        if (is.null(data_frame) & is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                          sep = ','
                        } else if (is.null(data_frame) & is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                          sep = '\t'
                        } else if (!is.null(file_name) & is.null(sep)) {
                          stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                        }
                      }
                      
                      # read in the data or convert to a data.table
                      if (is.null(data_frame) & !is.null(file_name)) {
                        thisdata <- data.table::fread(file=file_name, sep=sep, header=T)
                      } else if (!is.null(data_frame) & is.null(file_name)) {
                        colnames(data_frame) <- gsub("\\.","_",colnames(data_frame))
                        thisdata <- as.data.table(data_frame)
                      } else {
                        stop('Specify only ONE of data_frame or file_name.')
                      }
                      
                      # reorder rows
                      self$data <- thisdata[sample(nrow(thisdata)),]
                      
                      # replace spaces with underscores
                      colnames(self$data) <- gsub(" ", "_", colnames(self$data))

                      # make sure we have named label column in the parameters
                      if (is.null(label_name)) {
                        stop("Make sure label_name is not null!")
                      } else {
                        self$label <- sapply(self$data[[label_name]], as.character)
                        label_name <- gsub(' ', '_', label_name)
                      }

                      # make sure we have a sample ID column
                      if ((!is.null(sample_id)) && sample_id %in% colnames(self$data)) {
                        self$sample_id <- gsub(' ', '_', sample_id)
                        self$sample_ids <- sapply(self$data[[self$sample_id]], as.character)
                        set(self$data, j = self$sample_id, value = NULL)  # then del it
                      } else {
                        self$sample_ids <- 1:nrow(self$data)
                      }

                      # if the drop list is defined, standardize the names
                      if (!is.null(drop_list)) {
                        drop_list <- gsub(' ', '_', drop_list)
                      }

                      # make sure we have a label column
                      if (!label_name %in% colnames(self$data)) {
                        stop('Make sure the label name matches one of the columns!')
                      } else {
                        set(self$data, j = label_name, value = NULL)
                      }

                      # if we have a drop list, drop those columns
                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in% colnames(self$data)))) {
                        set(self$data, j = drop_list, value = NULL)
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }

                      # if we have some columns that have zero variance, fix that
                      data_var <- self$data[, lapply(.SD, var, na.rm=TRUE)]
                      data_var_idx <- which(data_var == 0.0)
                      if (length(data_var_idx) > 0) {
                        stop("DATA CONTAINS ZERO VARIANCE COLUMNS.\nThis will need to be fixed. \n Ideas: Drop or fill with random noise...")
                      }
                      
                      # DATA ENGINEERING
                      # self$data_eng('data')
                      self$op_mode <- 'data'
                      
                      return(invisible(self))
                    },
                    
                    
                    #' @description Does some setup processing on the training data file, drop columns and identify the label column.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    train_data_setup = function(data_frame=NULL,
                                                file_name=NULL, 
                                                sep=NULL, 
                                                data_mode=NULL, 
                                                signatures=NULL, 
                                                pair_list=NULL,
                                                label_name=NULL, 
                                                sample_id=NULL, 
                                                drop_list=NULL,
                                                verbose=NULL){
                      #READING DATA
                      self$file_name <- file_name
                      self$data_mode <- data_mode
                      self$signatures <- signatures
                      self$pair_list <- pair_list
                      self$sample_id <- sample_id
                      self$label_name <- label_name
                      self$verbose <- verbose
                      
                      print('starting train data setup')

                      # assume the file format
                      if (!is.null(file_name)) {
                        if (is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                          sep = ','
                        }
                        else if (is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                          sep = '\t'
                        } else if (!is.null(file_name) & is.null(sep)) {
                          stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                        }
                      }
                      
                      # read in the data or convert to a data.table
                      if (is.null(data_frame) & !is.null(file_name)) {
                        thisdata <- data.table::fread(file=file_name, sep=sep, header=T)
                      } else if (!is.null(data_frame) & is.null(file_name)) {
                        colnames(data_frame) <- gsub("\\.","_",colnames(data_frame))
                        thisdata <- as.data.table(data_frame)
                      } else {
                        stop('Specify only ONE of data_frame or file_name.')
                      }
                      
                      # reorder the rows randomly
                      self$train_data <- thisdata[sample(nrow(thisdata)),]
                      
                      # fix any spaces in the column names
                      colnames(self$train_data) <- gsub(" ", "_", colnames(self$train_data))
                      
                      # remove label from data
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
                      
                      # remove sample ID column from data
                      if ((!is.null(sample_id)) && sample_id %in% colnames(self$train_data)) {
                        self$sample_id <- gsub(' ', '_', sample_id)
                        self$train_sample_ids <- sapply(self$train_data[[self$sample_id]], as.character)
                        set(self$train_data, j = self$sample_id, value = NULL)  # then del it
                      } else {
                        self$train_sample_ids <- 1:nrow(self$train_data)
                      }
                      
                      # remove any other data variables
                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in%  colnames(self$train_data)))) {
                        drop_list <- gsub(' ', '_', drop_list)
                        set(self$train_data, j = drop_list, value = NULL)
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }

                      # if we have some columns that have zero variance, fix that
                      data_var <- self$train_data[, lapply(.SD, var, na.rm=TRUE)]
                      data_var_idx <- which(data_var == 0.0)
                      if (length(data_var_idx) > 0) {
                        if (is.null(verbose) || verbose > 0) {
                          print("TRAINING DATA CONTAINS ZERO VARIANCE COLUMNS")
                          print("...filling with random noise...")
                        }
                        for (dvi in data_var_idx) {
                          self$train_data[,dvi] <- runif(n=nrow(self$train_data))
                        }
                      }
                      
                      # save the final data columns used
                      self$data_colnames <- colnames(self$train_data)
                      
                      # and record the unique categories in labels 
                      self$unique_labels <- unique(self$train_label)

                      # DATA ENGINEERING
                      # self$data_eng('train')
                      self$op_mode <- 'train'

                      print('finish train data setup')


                      return(invisible(self))
                    },
                    
                    
                    #' @description Does some setup processing on the test data file, drop columns and identify the label column.
                    #' The data_mode and signatures will have already been set in training.
                    #' @param label_name string, the column name indicating the target label
                    #' @param drop_list a vector of strings indicating what columns to drop
                    test_data_setup = function(data_frame=NULL,
                                               file_name=NULL, 
                                               sep=NULL, 
                                               label_name=NULL, 
                                               sample_id=NULL, 
                                               drop_list=NULL,
                                               verbose=NULL){
                      
                      allgenes <- c(unlist(self$pair_list), unlist(self$signatures))
                      
                      if (!is.null(file_name)) {
                        if (is.null(sep) & stringr::str_detect(file_name, '.csv')) {
                          sep = ','
                        }
                        else if (is.null(sep) & stringr::str_detect(file_name, '.tsv')) {
                          sep = '\t'
                        } else if (!is.null(file_name) & is.null(sep)) {
                          stop('Please specify the sep parameter... or use a .csv or .tsv file.')
                        }
                      }
                      
                      # read in the data or convert to a data.table
                      if (is.null(data_frame) & !is.null(file_name)) {
                        thisdata <- data.table::fread(file=file_name, sep=sep, header=T)
                      } else if (!is.null(data_frame) & is.null(file_name)) {
                        colnames(data_frame) <- gsub("\\.","_",colnames(data_frame))
                        thisdata <- as.data.table(data_frame)
                      } else {
                        stop('Specify only ONE of data_frame or file_name.')
                      }
                      
                      self$test_data <- thisdata[sample(nrow(thisdata)),]
                      # replace spaces with underscores
                      colnames(self$test_data) <- gsub(" ", "_", colnames(self$test_data))

                      ### have to remove class
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
                      
                      # remove column containing sample IDs
                      if ((!is.null(sample_id))) { # already know that the col names match
                        test_sample_id <- gsub(' ', '_', sample_id)
                        self$test_sample_ids <- sapply(self$test_data[[test_sample_id]], as.character)
                        set(self$test_data, j = test_sample_id, value = NULL)  # then del it
                      } else {
                        self$test_sample_ids <- 1:nrow(self$test_data)
                      }
                      
                      # remove any other variables.
                      if ((!is.null(drop_list)) && all(sapply(drop_list, function(a) a %in% colnames(self$test_data)))) {
                        drop_list <- gsub(' ', '_', drop_list)
                        set(self$test_data, j = drop_list, value = NULL)
                      } else if ((!is.null(drop_list))) {
                        stop('Make sure the drop_list contains column names found in the data!')
                      }
                      
                      if ( (self$label_name %in% colnames(self$test_data)) ) {
                        set(self$test_data, j = self$label_name, value = NULL)
                      }
                      
                      # check that the data column names are the same as used in training
                      if (! all(allgenes %in% colnames(self$test_data)) ) {
                        stop('Test data column names must match what was used in training.')
                      } else {
                        # just subset to the proper columns now
                        self$test_data
                      }

                      # if we have some columns that have zero variance, fix that
                      data_var <- self$test_data[, lapply(.SD, var, na.rm=TRUE)]
                      data_var_idx <- which(data_var == 0.0)
                      if (length(data_var_idx) > 0) {
                        if (is.null(verbose) || verbose > 0) {
                          print("TEST DATA CONTAINS ZERO VARIANCE COLUMNS")
                          print("...filling with random noise...")
                        }
                        for (dvi in data_var_idx) {
                          self$test_data[,dvi] <- runif(n=nrow(self$test_data))
                        }
                      }
                      
                      # DATA ENGINEERING
                      # self$data_eng('test')
                      self$op_mode <- 'test'

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
                    #' @param params list, The main set of parameters.
                    #'
                    #' @details A list of classifiers, each trained on a random sample of the training data.
                    #'
                    #' @return A ensemble object is added to the list of objects in recm$enbl.
                    #'
                    #'
                    build_label_ensemble = function(params) {
                      
                      print('starting ensemble build')

                      # for each category
                      for (li in self$unique_labels) {
                        
                        print(paste0('subtype ',li))

                        # first create the binarized label
                        bin_label <- self$binarize_label(label=self$train_label, x=li)
                        # then create the classifier object

                        ## !! pair list is either a named list or a vector
                        if (class(self$pair_list) == "list") {
                          this_pair_list <- self$pair_list[[li]]
                        } else {
                          this_pair_list <- self$pair_list
                        }

                        self$ensbl[[li]] <- Ensemble$new(name=li, 
                                                         obj_mode='ensemble',
                                                         size=params$size, 
                                                         data_mode=self$data_mode,
                                                         train_data=self$train_data,
                                                         pair_list=this_pair_list,
                                                         signatures=self$signatures,
                                                         label=bin_label, 
                                                         params=params)

                        self$ensbl[[li]]$data_eng(self$op_mode)
                        
                      }

                      print('finished ensemble build')
                      
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
                    
                    
                    
                    build_final_ensemble = function(params) {
                      # here the ensemble will be trained on the prior predictions
                      # for each category, get the predictions
                      # turn that into a dataframe / matrix
                      print('building final ensemble')
                      self$build_pred_table()

                      remapped_label <- self$remap_multiclass_labels(self$train_label)
                      #print(head(self$train_label))
                      #print(head(remapped_label))
                      # train a XGBoost that takes multiple labels.

                      self$ensbl[["final"]] <- Ensemble$new(name="final",
                                                         obj_mode="final",
                                                         size=params$size, 
                                                         data_mode=self$data_mode,
                                                         train_data=self$pred_table,
                                                         pair_list=c(),
                                                         signatures=c(),
                                                         label=remapped_label, 
                                                         params=params
                                                    )

                      print('finished building final ensemble')
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
                    
                    
                    ensemble_setup = function(combine_function) {
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$member_predict(
                          as.matrix(self$ensbl[[li]]$train_data), combine_function)
                      }
                      return(invisible(self))
                    },
                    
                    
                    ensemble_predict = function(data, combine_function) {
                      for (li in self$unique_labels) {
                        self$ensbl[[li]]$test_data <- data # can't be matrix until after data eng
                        self$ensbl[[li]]$data_eng('test')
                        self$ensbl[[li]]$member_predict(
                        self$ensbl[[li]]$test_data, combine_function)
                      }
                      return(invisible(self))
                    },
                    
                    
                    # predict final uses predictions from predict_ensemble
                    final_predict = function(data, combine_function) {
                      self$ensemble_predict(data, combine_function)
                      self$build_pred_table()

                      # then we should have a new pred_table from the data
                      pred_matrix <- as.matrix(self$pred_table)
                      
                      self$ensbl[['final']]$member_predict(
                        as.matrix(pred_matrix), combine_function)
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
                    
                    
                    
                    #' @description 
                    #' Returns a table of classification metrics, one row per label.
                    #' @param these_labels vector, the classification labels.
                    #' @param these_calls vector, the predicted calls.
                    #' @param use_cv_results boolean, use the cross validation results or internal test set
                    #'
                    #' @details Returns various metrics associated with machine learning performance.
                    #'
                    #' @return A table of classification metrics for each label and overall.
                    #'
                    #'
                    classification_metrics = function(these_labels=NULL, these_calls=NULL, use_cv_results=TRUE ) {
                      
                        # there are instances where some classes are not returned
                        # in the BestCalls, those can cause numeric(0) in sens, spec, etc.
                        isEmpty <- function(x) {
                          return(identical(x, numeric(0)))
                        }
                      
                        fixMissing <- function(x) {
                          sapply(x, function(a) if(isEmpty(a)){0}else{a})
                        }
                      
                        if (use_cv_results) {
                          calls <- self$cv_results$BestCalls
                          labels <- self$cv_results$Label
                        } 
                        else if ( (!is.null(these_calls)) && (!is.null(these_labels)) ) {
                          labels <- these_labels
                          calls <- these_calls
                        } else {
                          if (is.null(self$test_label)) {
                            return("No test labels.")
                          } else {
                            # first make sure our labels are mapped to integers correctly
                            labels <- self$test_label
                            # then get the calls
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
                        prec <- fixMissing(sapply(cm_labels, function(a) self$precision(cmdf, a)))
                        
                        # then specificity
                        spec <- fixMissing(sapply(cm_labels, function(a) self$specificity(cmdf, a)))
                        
                        # then sensitivity or recall
                        sens <- fixMissing(sapply(cm_labels, function(a) self$sensitivity(cmdf, a)))
                        
                        # then F1
                        if (is.numeric(sens) & is.numeric(prec)) {
                          f1 <-(2*sens*prec) / (sens+prec+0.0000000001)
                        } else {
                          f1 <- 0
                        }
                        
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
                    autocv = function(  data_frame=NULL,
                                        data_file=NULL,
                                        sep=NULL,
                                        label_name=NULL,
                                        sample_id=NULL,
                                        drop_list=NULL,
                                        cv_rounds=1,
                                        data_split=NULL,
                                        data_mode=NULL,
                                        signatures=NULL,
                                        pair_list=NULL,
                                        params=NULL,
                                        combine_function=NULL,
                                        verbose=NULL
                                      ) {
                      
                      params[['objective']] <- "binary:logistic"
                      params[['eval_metric']] <-'logloss'
                      final_params <- params
                      final_params[['objective']] <- 'multi:softmax'
                      final_params[['eval_metric']] <- 'mlogloss'
                      
                      self$combine_function=combine_function
                      
                      # perform the data set up
                      self$data_setup(data_frame=data_frame,
                                      file_name=data_file,
                                      sep=sep,
                                      data_mode=data_mode,
                                      signatures=signatures,
                                      pair_list=pair_list,
                                      label_name=label_name, 
                                      sample_id=sample_id,
                                      drop_list=drop_list,
                                      verbose=verbose)

                      self$op_mode <- 'train'
                      
                      for (cvi in 1:cv_rounds){
                        
                        print(paste0("*** CV Round ", cvi, " ***"))
                        
                        # SPLIT DATA round 1
                        self$data_split_fun(data_split, cv_rounds, cvi)
                        
                        # build the initial set of predictors
                        self$build_label_ensemble(params=params)
                        
                        # and train them using a random selection of data
                        self$train_models(params$train_perc)
                        
                        # then make a prediction on the training data
                        self$ensemble_predict(self$train_data, 
                                              combine_function = params$combine_function)
                        
                        # build the output predictor
                        self$build_final_ensemble(final_params)
                        
                        # and use the earlier training predictions to train the output                      
                        self$train_final(params$train_perc)
                        
                        # and finally, make a prediction on some training data.
                        self$final_predict(self$test_data, params$combine_function)
                        
                        # capture the feature importance from each fold
                        self$cv_importance[[cvi]] <- self$importance()
                        
                        # append the final results 
                        if (cvi > 1) {
                          self$cv_results = rbind(self$cv_results, self$results(include_label = T))
                        } else {
                          self$cv_results = self$results(include_label = T)
                        }
                        
                      } # done with CV
                    
                    },# end autopred
                    
                    
                    # Train a classifier
                    train = function(data_frame=NULL,
                                        data_file=NULL,
                                        sep=NULL,
                                        label_name=NULL,
                                        sample_id=NULL,
                                        drop_list=NULL,
                                        data_mode=NULL,
                                        signatures=NULL,
                                        pair_list=NULL,
                                        params=NULL,
                                        verbose=NULL
                    ) {
                      
                      params[['objective']] <- "binary:logistic"
                      params[['eval_metric']] <-'logloss'
                      final_params <- params
                      final_params[['objective']] <- 'multi:softmax'
                      final_params[['eval_metric']] <- 'mlogloss'
                      
                      self$combine_function=params$combine_function
                    
                      # perform the data set up
                      self$train_data_setup(data_frame=data_frame,
                                            file_name=data_file,
                                            sep=sep,
                                            data_mode=data_mode,
                                            signatures=signatures,
                                            pair_list=pair_list,
                                            label_name=label_name, 
                                            sample_id=sample_id,
                                            drop_list=drop_list,
                                            verbose = verbose)
                      
                      # building the first layer of predictors, each a binary prediction
                      # on one factor in the target labels.
                      # training and making predictions on the training data
                      self$build_label_ensemble(params=params)$
                        train_models(params$train_perc)$
                        ensemble_setup(params$combine_function)

                      
                      # then we build the output layer, trained on the predictions of the first layer
                      self$build_final_ensemble(final_params)$
                        train_final(params$train_perc)
                      
                      return(invisible(self))
                    }, # end autotrain
                    
                    
                    # make predictions on a new data set
                    # after running autotrain()
                    predict = function(data_frame=NULL,
                                        data_file=NULL,
                                        sep=NULL,
                                        label_name=NULL,
                                        sample_id=NULL,
                                        drop_list=NULL,
                                        verbose=NULL) {
                    
                      self$test_data_setup( data_frame=data_frame,
                                      file_name=data_file, 
                                      sep=sep, 
                                      label_name=label_name, 
                                      sample_id=sample_id, 
                                      drop_list=drop_list,
                                      verbose = verbose)
                      
                      self$final_predict(self$test_data, self$final_params$combine_function)
                  
                      return(invisible(self)) 
                    }
                  ) # end public
      )


