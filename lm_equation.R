lm_equation <- function(data, xname, yname){
  x <- eval(parse(text = xname),envir=data)
  y <- eval(parse(text = yname),envir=data)
  xy <- data.frame(cbind(x,y))
  m = lm(y ~ x, xy);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}