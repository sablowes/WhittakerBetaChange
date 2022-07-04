dist2line = function(x, y, a = 1, b = -1){
  # calculate distance from line: ax + by + c 
  # to vector of points(x, y)
  # NB: here line is x==y, so c = 0, a = 1, b = -1 (or a = -b)
  
  numerator = -(a*x + b*y) # negative distance means point below line gets negative value (decreasing beta-diversity)
  denom = sqrt(a^2 + b^2)
  return(numerator / denom)
}

