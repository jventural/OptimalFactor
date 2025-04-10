\name{stepwise_efa_removal_structure}
\alias{stepwise_efa_removal_structure}
\title{Stepwise Item Removal and Optimization of Factor Structure in Exploratory Factor Analysis}
\description{
This function optimizes an Exploratory Factor Analysis (EFA) model using an iterative process of item removal. It combines two evaluation criteria: (1) improvement of the overall model fit, as measured by the RMSEA (Root Mean Square Error of Approximation), and (2) the quality of the factor loadings, ensuring that each item has a single strong loading (above a specified threshold) on one factor while maintaining lower loadings on the other factors, and that each factor contains a minimum number of items.
}
\usage{
stepwise_efa_removal_structure(data, items = NULL, n_factors = 5, n_items = NULL, name_items = "PPTQ", estimator = "WLSMV", rotation = "oblimin", threshold_rmsea = 0.08, threshold_loading = 0.30, min_items_per_factor = 2, apply_threshold = TRUE, max_steps = NULL, model_index = NULL, verbose = TRUE, ...)
}
\arguments{
  \item{data}{A data frame that contains the variables (items) on which the EFA will be conducted.}
  \item{items}{A character vector with the names of the items to include. If \code{NULL}, the items are inferred from the pattern defined in \code{name_items} and the count provided in \code{n_items}.}
  \item{n_factors}{The number of factors to be extracted in the EFA model.}
  \item{n_items}{The total number of items to consider. If \code{NULL}, the length of the \code{items} vector is used.}
  \item{name_items}{A prefix identifying the item columns in the data frame. For example, \code{"PPTQ"} will generate names such as \code{"PPTQ1"}, \code{"PPTQ2"}, etc.}
  \item{estimator}{The estimation method used by the auxiliary function \code{EFA_modern}.}
  \item{rotation}{The rotation method applied in the EFA (default is \code{"oblimin"}).}
  \item{threshold_rmsea}{The maximum allowed RMSEA value for the model.}
  \item{threshold_loading}{The threshold used to determine significant factor loadings (e.g., 0.30).}
  \item{min_items_per_factor}{The minimum number of items with significant loadings required for each factor.}
  \item{apply_threshold}{A logical value indicating whether the loading threshold should be applied during estimation.}
  \item{max_steps}{The maximum number of iterations allowed in the item removal process. If \code{NULL}, it defaults to the number of items minus one.}
  \item{model_index}{An index that specifies which row (or submodel) to extract the RMSEA from in the \code{Bondades_Original} object. If \code{NULL}, it is determined based on the number of factors.}
  \item{verbose}{A logical value. If \code{TRUE}, informative messages are printed during each iteration of the process.}
  \item{...}{Additional arguments to be passed to the \code{EFA_modern} function.}
}
\details{
The \code{stepwise_efa_removal_structure} function executes an iterative procedure to optimize the factor structure of an EFA model. In each iteration:
\enumerate{
  \item The EFA model is estimated using the \code{EFA_modern} function with the current set of items (excluding those that have already been removed).
  \item The current model's RMSEA is extracted and compared against the threshold specified in \code{threshold_rmsea}.
  \item The factor loading matrix is examined to ensure that each item has a single significant loading (above \code{threshold_loading}) and that each factor includes at least \code{min_items_per_factor} items.
  \item If any of the criteria are not met, the function identifies a problematic item—either because its removal would significantly improve the RMSEA or because it exhibits an inadequate loading pattern (such as cross-loadings or no significant loading)—and removes that item from the model.
}
The procedure continues until a model meeting both criteria is obtained or until the maximum number of iterations is reached.
}
\value{
A list with the following components:
\item{final_structure}{The factor loading matrix of the final acceptable model.}
\item{removed_items}{A character vector containing the names of the items removed during the process.}
\item{steps_log}{A data frame logging, for each iteration, the removed item, the reason for its removal, and the RMSEA value after removal.}
\item{iterations}{The total number of iterations executed.}
\item{final_rmsea}{The final RMSEA value obtained for the model.}
}
\examples{
# Example usage:
# Running the function to extract 3 factors, ensuring that each factor has at least 2 items
# and that the RMSEA is less than or equal to 0.08.
result <- stepwise_efa_removal_structure(
  data = mi_data,
  n_factors = 3,
  name_items = "PPTQ",
  threshold_rmsea = 0.08,
  threshold_loading = 0.30,
  min_items_per_factor = 2,
  verbose = TRUE
)

# Display the process log and final results
print(result$steps_log)
cat("Final RMSEA:", result$final_rmsea, "\n")
cat("Removed Items:", paste(result$removed_items, collapse = ", "), "\n")
}
\author{
Dr. José Ventura-León
}
