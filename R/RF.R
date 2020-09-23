RF <- function (dat, cv_method) {

    `%>%` <- magrittr::`%>%`

    # Shuffle data.frame.
    set.seed(32256)
    row_order <- sample(nrow(dat))
    dat <- data.frame(dat)[row_order, ]

    # Create index column based on CV method for subsetting train/test.
    if (cv_method == 1) {
        dat$idx <- seq(nrow(dat))
    } else if (is.numeric(cv_method)) {
        dat$idx <- cut(seq(nrow(dat)),
                       breaks = cv_method,
                       labels = FALSE)
    } else if (cv_method %in% c("Family", "family")) {
        dat$idx <- paste0(dat$A, dat$B) %>%
            factor() %>%
            as.numeric()
    }

    # Establish parallel enviroment to run CV folds in tandem.
    cl <- parallel::makeCluster(parallel::detectCores() - 2)
    doParallel::registerDoParallel(cl)

    # Run random forest algorithm on different folds in random order.
    set.seed(66824)
    res <- foreach (test_idx = sample(unique(dat$idx))) %dopar% {

        rf_mod <- randomForest::randomForest(formula = Yield ~ .,
                                             data = dat[!dat$idx %in% test_idx, c(1, 6:ncol(dat))],
                                             importance = TRUE)

        stats::predict(rf_mod, dat[dat$idx %in% test_idx, c(1, 6:ncol(dat))]) %>%
            data.frame(GE_ID = dat$GE_ID[dat$idx == test_idx],
                       Pop = paste0(dat$A[dat$idx == test_idx],
                                    dat$B[dat$idx == test_idx]),
                       Pred = .)

    }

    parallel::stopCluster(cl)
    return(rbindlist(res))

}
