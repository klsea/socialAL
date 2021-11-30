decode_img <- function(dt, img, x) {
# function to decode image name and condition
    if(img == "A1f01") {
    y = dt$A1f01_LikeRating[x]
  } else if (img == 'B1f01') {
    y = dt$B1f01_LikeRating[x]
  } else {
    y = dt$C1f01_LikeRating[x]
  }
  return(y)
}
