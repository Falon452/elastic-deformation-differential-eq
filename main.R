### Zadanie obliczeniowe - Metoda Elementów Skończonych

n <- 10


x_i <- function(i){
  return(2*i/n)
}


e_prim <- function(x, i){
  if (x > x_i(i-1) && x <= x_i(i)){
    return (n/2)
  }
  else if (x > x_i(i) && x < x_i(i+1)){
    return (-n/2)
  }
  else {
    return (0)
  }
}

e <- function(x, i){
  if (x > x_i(i-1) && x <= x_i(i)){
    return (n/2*x - i + 1)
  }
  else if (x > x_i(i) && x < x_i(i+1)){
    return (-n/2*x + i + 1)
  }
  else {
    return (0)
  }
}


# integrate <- function(f, a, b){
#   return(
#     (b-a)/2*(
#       f((b-a)/2*(1/sqrt(3)) + (b+a)/2)
#       + f((b-a)/2*(-1/sqrt(3)) + (b+a)/2)
#     )
#   )
# }


E <- function(x) {
  if (x>= 0 && x <= 1) {
    return (3)
  } else if (x >1 && x<=2){
    return (5)
  }
  return (0)
}



E_u_prim_v_prim <- function(i, j){
  return (function(x){
      return(E(x) * e_prim(x, i)*e_prim(x, j))
    }
  )
}


B <- function (i, j) {
  if (!(i == j || i == j+1 || i == j-1)){
      return (0)
  }
  if (i == j){
    lower <- max(0, x_i(i-1))
    upper <- min(2, x_i(i+1))
  } else {
    lower <- max(0, x_i(min(i,j)))
    upper <- min(2, x_i(max(i,j)))
  }
  return(e(0, i) * e(0, j) - integrate(E_u_prim_v_prim, lower, upper))
}

L <- function(i){
  return(10*e(0, i))
}


# główna funkcja znajdująca rozwiązanie równania
solution <- function(){
  # tworzenie głownej macierzy układu równań
  M <- matrix(0, nrow=n, ncol=n)

  for (i in 1:n){
    for (j in 1:n){
      M[i,j] <- B(j-1, i-1)
    }
  }

  # kolumna wyrazów obcych
  C <- vector()
  for (i in 1:n){
    C = c(C, L(i-1))
  }

  # rozwiązanie układu
  vs = solve(M, C)

  # kombinacja liniowa funkcji bazowych
  result_function <- function(x, v = vs){
    result = 0
    for (i in 1:n){
      result = result + v[i]*e(x, i-1)
    }
    return(result)
  }

  return (result_function)
}

# funkcja rysująca rozwiązanie równania
plot_result <- function(){
  u = solution()
  plot(seq(0, 2, 1/(100*n)),
       mapply(u, seq(0, 2, 1/(100*n))),
       main = 'rozwiązanie równania transportu ciepła',
       xlab=' ',
       ylab=' ',
       type='l')
}

plot_result()
