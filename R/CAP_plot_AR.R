#' Function plots CAP curve, and calculate Accuracy Rate of the given logistic regression model.
#'
#' @param logistic_probability_vector Vector of probabilities predicted by logistic regression model.
#' @param given_outcome_vector_from_data Vector of outcomes from train or test data.
#'
#' @return CAP curve and Accuracy rate of the model.
#' @export
#'
#' @examples CAP_plot_AR(predicted,outcome_from_data)
CAP_plot_AR <-  function(logistic_probability_vector,given_outcome_vector_from_data){

  length <- length(logistic_probability_vector)
  sum_exited <- sum(given_outcome_vector_from_data)
  prop_Exit = sum_exited/length


  randomModel <- 1:length
  randomModel <- randomModel *prop_Exit
  #transform scale to percentage
  randomModel_percent <- randomModel/sum_exited


  perfectModel <- rep(sum_exited,length)
  perfectModel[1:sum_exited] <- 1:sum_exited
  #transform scale to percentage
  perfectModel_percent <- perfectModel/sum_exited

  #give outcomes proper order from the highest model probability
  logistic_probability_vector <- sort(logistic_probability_vector, decreasing = T)
  given_outcome_vector_from_data <- given_outcome_vector_from_data[as.numeric(names(logistic_probability_vector))]

  logisticRegression <- rep(0,length)
  logisticRegression[1] <- 0 + given_outcome_vector_from_data[1]
  for(i in 2:length){
    logisticRegression[i] <- logisticRegression[i-1] +given_outcome_vector_from_data[i]
  }
  #transform scale to percentage
  logisticRegression_percent <- logisticRegression/sum_exited




  yscale <- 1:length
  yscale <- yscale/length
  data <- data.frame(random = randomModel_percent, logistic=logisticRegression_percent,perfect=perfectModel_percent)

  p <- ggplot2::ggplot(data=data, ggplot2::aes(yscale,y=random)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes(yscale,logistic))+
    ggplot2::geom_line(ggplot2::aes(yscale,perfect)) +
    ggplot2::scale_y_continuous(labels = scales::percent ) +
    ggplot2::scale_x_continuous(labels = scales::percent )


  print(p)


  between <- MESS::auc(1:length,logisticRegression)-MESS::auc(1:length,randomModel)
  over <- MESS::auc(1:length,perfectModel)-MESS::auc(1:length,randomModel)
  AR <- between/over
  print(AR)
}
