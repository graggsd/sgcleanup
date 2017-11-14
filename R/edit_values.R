# edit_values <- function(data, reclass.dict){
#
#     # Check for consistency of rows in reclass dictionary
#     if (length(setdiff(reclass.dict$screening_id, data$screening_id)) > 0) {
#         stop("Rows not in dataset include:",
#              setdiff(reclass.dict$screening_id, data$screening_id))
#     }
#
#     # Check for consistency of columns in reclass dictionary
#     cols <- unique(reclass.dict$variable.to.change)
#     if (length(setdiff(cols, colnames(data))) > 0) {
#         stop("Columns not in dataset include:",
#              setdiff(setdiff(cols, colnames(data))))
#     }
#
#     # Check to make sure that all original column values match up with the dataset
#     merged <- merge.reclass.data(data = data, reclass.dictionary = reclass.dict)
#
#     if (!identical(as.character(merged$value), as.character(merged$old.value))){
#
#         print("The following data from the reclassification dictionary and dataset are mis-matched")
#         print(as.matrix(merged[!(merged$value == merged$old.value),
#                                c("screening_id.key", "value", "old.value")],
#                         ncol = 3))
#         stop(paste0("Make sure that dataset values match values specified in ",
#                     "the 'old.value' column of the reclassification dictionary"))
#     }
#
#     # Create an index
#     idx <- match(reclass.dict$screening_id, data$screening_id)
#     # Create a loop to change values indicated by reclassification dictionary
#     for(i in 1:nrow(reclass.dict)){
#
#         data[idx[i], reclass.dict[i,"variable.to.change"]] <- reclass.dict[i,"new.value"]
#
#     }
#
#     merged <- merge.reclass.data(data = data, reclass.dictionary = reclass.dict)
#
#     # Check to make sure that all values were changed appropriately
#     if (!identical(merged$value, merged$new.value)){
#
#         print(as.matrix(merged[!(merged$value == merged$new.value),
#                                c("screening_id.key", "value", "new.value")],
#                         ncol = 3))
#         stop(paste0("The values in the altered dataset do not match those specified in",
#                     "the 'new.value' column of the reclassification dictionary"))
#     }
#
#     # return results
#     return(data)
#
# }