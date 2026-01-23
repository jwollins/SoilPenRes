# Function to convert DMS to decimal degrees
dms_to_decimal <- function(d, m, direction) {
  decimal <- as.numeric(d) + as.numeric(m) / 60
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  return(decimal)
}
