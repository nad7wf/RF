#' CV-optimized Random Forest
#'
#' In plant breeding, it is common to leave one population out when training a machine learning model. This package
#' allows the user to run random forest using one population as the test set, as well as with other common CV
#' strategies (e.g. 5-fold, 10-fold, etc.).
#'
#' @param data a data.frame with columns "Y", "GE_ID", "Parent_A", "Parent_B", followed by one column per genetic marker.
#' @param cv_method Either an integer, specifying the number of folds, or "family" to fold according to population.
#'
#' @return A list of two data.frames, one containing the predictions for each GE, and one containing the correlation coefficients
#' between observed and predicted values for each fold.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' set.seed(76123)
#' row_num <- 1000
#'
#' ex <- data.frame(
#'     Y = runif(row_num, 30, 150),
#'     GE_ID = runif(row_num, 1e8, 2e8),
#'     Parent_A = sample(LETTERS[1:13], row_num, replace = TRUE),
#'     Parent_B = sample(LETTERS[14:26], row_num, replace = TRUE)
#' ) %>%
#'     cbind(replicate(10, runif(row_num, 0, 1)) %>%
#'               as.data.frame() %>%
#'               magrittr::set_names(paste0("Marker", seq(10))))
#'
#' result <- RF(ex, "family")
RF <- function (data, cv_method) {

    # Create an index based on CV method for subsetting train/test.
    set.seed(32256)

    if (is.numeric(cv_method)) {
        idx <- seq(nrow(dat)) %>%
            cut(breaks = cv_method,
                labels = FALSE) %>%
            sample()
    } else if (cv_method %in% c("Family", "family")) {
        idx <- paste0(dat$Parent_A, dat$Parent_B)
    }

    comb <- function(x, ...) {
        lapply(seq_along(x),
               function(i) c(x[[i]],
                             lapply(list(...),
                                    function(y) y[[i]])))
    }

    # Establish parallel enviroment to run CV folds in tandem.
    cl <- parallel::makeCluster(2)
    doParallel::registerDoParallel(cl)

    # Run random forest algorithm in random order on different folds.
    res <- foreach::foreach (test_idx = sample(unique(idx)),
                             .combine = "comb",
                             .multicombine = TRUE,
                             .init = list(list(), list())) %dopar% {

                                 train <- dat[idx != test_idx, c(1, 5:ncol(dat))]
                                 test <- dat[idx == test_idx, c(1, 5:ncol(dat))]

                                 rf_mod <- randomForest::randomForest(formula = Y ~ .,
                                                                      data = train)

                                 preds <- stats::predict(rf_mod, test) %>%
                                     data.frame(
                                         GE_ID = dat$GE_ID[idx == test_idx],
                                         Pop = paste0(dat$Parent_A[idx == test_idx],
                                                      dat$Parent_B[idx == test_idx]),
                                         Pred = .
                                     )

                                 cc <- stats::cor(data.frame(preds$Pred, dat$Y[idx == test_idx]))[[2]] %>%
                                     data.frame(Fold = test_idx,
                                                Cor = .)

                                 return(list(preds, cc))
                             } %>%
        lapply(function (list_ele) {
            do.call("rbind", list_ele)
        }) %>%
        `names<-`(c("preds", "cc"))

    parallel::stopCluster(cl)
    return(res)

}
