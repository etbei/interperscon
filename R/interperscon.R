#' @title Interpersonal Consistency
#'
#' @description Calculates the extent of interpersonal consistency for the adjective ranking task described by Block (1961). Instead of the centroid method of factor analysis, which was used in the paper, an eigen decomposition is done. "The "data" needs to be in long format with "id" as key variable. The package returns one value for each particpant, which is the extent of interpersonal consistency. A higher value means a higher interpersonal consistency, the value can range from 0 to 100.
#'
#' @param data yourdataset
#'
#' @param id youridvariable
#'
#' @return NULL
#'
#' @examples interpers_con(yourdata, youridvariable)
#'
#' @export returns one value for each participant in the data (ordered according to the id variable in the dataset in wide format)
#'
#' @author Esther Beierl <esther.beierl@gmx.de> [‘aut‘, ‘cre‘], Lawrence Yu [‘ctb‘]
#'
#' @references Block, J. (1961). Ego Identity, Role Variability, and Adjustment. Journal of Clinical Psychology, 25(5), 392-397.

interpers_con <- function(data, id) {
    id <- dplyr::enquo(id)

    data_cor <- data %>%
      dplyr::group_by(!! id) %>%
      tidyr::nest() %>%
      dplyr::mutate(cor = purrr::map(data, cor))
      cormat_list <- dplyr::pull(data_cor, cor) # calculate a correlation matrix for each participant and store them in a list

      ev1 <- vector()
      interperscon <- vector()
      for(i in 1:length(cormat_list)) {
        ev1[i] <- eigen(cormat_list[[i]])$values[1] # get the first eigenvalue of each correlation matrix and store them in a vector
        interperscon[i] <- (ev1[i]/8)*100 # get the percentage of variance explained by the first factor
        }
      return(interperscon) # the function returns one value for each participant in the order of the original dataset
}

# create examplary data

id <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20))
data <- data.frame(id = id,
                   var1 = sample(1:20, replace = FALSE),
                   var2 = sample(1:20, replace = FALSE),
                   var3 = sample(1:20, replace = FALSE),
                   var4 = sample(1:20, replace = FALSE),
                   var5 = sample(1:20, replace = FALSE),
                   var6 = sample(1:20, replace = FALSE),
                   var7 = sample(1:20, replace = FALSE),
                   var8 = sample(1:20, replace = FALSE))
