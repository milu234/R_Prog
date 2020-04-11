library(jsonlite)
yelp <- stream_in(file("yelp_academic_dataset_business.json"))
head(yelp, 10)

yelp_flat <- flatten(yelp)
str(yelp_flat)



# Export a data frame
data_export <-iris
write_json(data_export, "iris.json")